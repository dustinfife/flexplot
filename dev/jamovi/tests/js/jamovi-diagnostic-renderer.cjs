'use strict';
const fs=require('node:fs'),vm=require('node:vm'),assert=require('node:assert/strict');
(async()=>{
    const [runtime,fixtures]=process.argv.slice(2);
    const source=fs.readFileSync(runtime,'utf8');
    const marker=source.indexOf('this.classList.add("jmv-results-html")');
    assert.ok(marker>=0,'Installed HTML renderer not found');
    const start=source.lastIndexOf('class ',marker);
    const end=source.indexOf('customElements.define("jmv-results-html"',marker);
    let baseCalls=0;
    const Html=vm.runInNewContext('('+source.slice(start,end).replace(/;\s*$/,'')+')',
        {Vt:{View:class {render(){baseCalls++;}}}});
    const render=async(content,error)=>{
        const node={innerHTML:'',querySelectorAll:()=>[]};
        const host={model:{attributes:{element:{content},error:error?{message:error}:null}},
            $head:{querySelector:()=>null},promises:[],ready:Promise.resolve(),
            querySelector:()=>node,querySelectorAll:()=>[]};
        Html.prototype.render.call(host);
        await host.ready;
        return node.innerHTML;
    };
    const text=html=>html.replace(/<[^>]*>/g,'').replace(/&lt;/g,'<').replace(/&gt;/g,'>').replace(/&amp;/g,'&');
    const cases=JSON.parse(fs.readFileSync(fixtures,'utf8'));
    for(const fixture of cases){
        const output=await render(fixture.content,fixture.error);
        assert.ok(text(output).includes(fixture.expected),fixture.name+': diagnostic must be rendered, not only stored in error metadata');
        assert.equal(/<(script|img|svg)\b/i.test(output),false,'Untrusted markup must remain escaped');
        if(fixture.compact){assert.ok(output.includes('style="margin:0 0 4px 0;"'));assert.equal(output.includes("<p>"),false);}
        if(fixture.literal)assert.ok(text(output).includes(fixture.literal),'Literal variable name must survive HTML escaping');
    }
    const old=await render('','Invalid binomial outcome');
    assert.equal(baseCalls,0,'This installed HTML override does not invoke the base error renderer');
    assert.equal(old,'');
    assert.throws(()=>assert.ok(text(old).includes('Invalid binomial outcome')),
        'The regression check must reject the previous empty-HTML implementation');
    console.log('PASS: '+cases.length+' real R6 diagnostics render through the installed HTML method; old error-only HTML fails.');
})().catch(error=>{console.error(error);process.exitCode=1;});
