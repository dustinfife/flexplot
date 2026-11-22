'use strict';
const fs=require('node:fs'),vm=require('node:vm'),assert=require('node:assert/strict');
const [runtime,fixture]=process.argv.slice(2);
const source=fs.readFileSync(runtime,'utf8');
const end=source.indexOf(';customElements.define("jmv-results-image"');
const start=source.lastIndexOf('render(){',end);
assert.ok(start>=0 && end>start,'Installed image renderer must be inspectable');
const render=vm.runInNewContext('('+source.slice(start,end-1).replace(/^render/,'function')+')',{qc:()=>''});
for(const record of JSON.parse(fs.readFileSync(fixture,'utf8'))){
    const attributes={'data-status':'running'},style={};
    render.call({$title:null,$image:{style},address:()=>[],
        model:{attributes:{status:record.status,element:record.image}},
        setAttribute:(key,value)=>attributes[key]=value,removeAttribute:key=>delete attributes[key]});
    assert.equal(attributes['data-status'],undefined,'A terminal plot must not remain running');
    assert.ok(record.image.path,'A diagnostic must have a real rendered image');
    assert.ok(style.backgroundImage.includes(record.image.path),'Native renderer must display the rendered image');
}
console.log('PASS: installed native image renderer displays diagnostic and recovered images without a running indicator.');
