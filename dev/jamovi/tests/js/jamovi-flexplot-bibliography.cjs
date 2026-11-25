'use strict';
const fs=require('node:fs'),path=require('node:path'),vm=require('node:vm'),assert=require('node:assert/strict');
const [assets,fixtures]=process.argv.slice(2);
const file=pattern=>fs.readFileSync(path.join(assets,fs.readdirSync(assets).find(n=>pattern.test(n))),'utf8');
const main=file(/^main-.*\.js$/),results=file(/^resultsview-.*\.js$/);
// Execute the installed References panel's collection/update method. Its input
// is AnalysisResponse.references, independently of result-element marker refs.
const start=main.indexOf('update(){let e=[],t=new Set;e.push(this.resolve("jmv",');
assert.ok(start>=0,'Installed bibliography update contract changed');
let end=main.indexOf('{',start)+1,depth=1;
while(depth){if(main[end]==='{')depth++;if(main[end]==='}')depth--;end++;}
const shown=[];
const update=vm.runInNewContext('({'+main.slice(start,end)+'}).update',{
    mo:{name:'jamovi',title:'jamovi software'},Ys:{name:'R',title:'R software'},
    document:{createElement:()=>({setup(number,text){this.number=number;this.text=text;},setAttribute(){},addEventListener(){}})}});
const refEnd=results.indexOf('customElements.define("jmv-reference-numbers"'),refStart=results.lastIndexOf('class ',refEnd);
class Element {attachShadow(){return {innerHTML:'',querySelector:()=>({innerHTML:'',insertAdjacentHTML(_,html){this.innerHTML+=html;}})};}}
const Markers=vm.runInNewContext('('+results.slice(refStart,refEnd).replace(/;\s*$/,'')+')',{HTMLElement:Element,Tg:''});
for(const record of JSON.parse(fs.readFileSync(fixtures,'utf8'))){
    shown.length=0;
    const host={_analyses:[{ns:'flexplot',references:record.references}],
        _body:{innerHTML:'',querySelectorAll:()=>[],appendChild:element=>shown.push(element)},
        resolve:(module,ref)=>({addresses:[{module,name:ref.name}],text:ref.title})};
    update.call(host);
    assert.deepEqual(shown.map(item=>item.text),['jamovi software','R software',...record.references.map(ref=>ref.title)]);
    assert.deepEqual(shown.map(item=>item.number),[1,2,3,4]);
    for(const refs of record.element_refs){
        const marker=new Markers();marker.setTable({mode:'rich',table:{flexplot:3,graphicalAnalysis:4},addEventListener(){}});marker.setRefs(refs);
        assert.equal(marker.count(),0);assert.equal(marker._body.innerHTML,'');
    }
}
console.log('PASS: native References panel keeps both articles/software attribution; native output marker components stay empty');
