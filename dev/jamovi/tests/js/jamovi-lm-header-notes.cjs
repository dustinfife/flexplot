'use strict';
const fs=require('node:fs'),path=require('node:path'),vm=require('node:vm'),assert=require('node:assert/strict');
const [assets,file]=process.argv.slice(2);
const renderer=fs.readFileSync(path.join(assets,fs.readdirSync(assets).find(n=>/^resultsview-.*\.js$/.test(n))),'utf8');
const common=fs.readFileSync(path.join(assets,fs.readdirSync(assets).find(n=>/^highcontrast-.*\.js$/.test(n))),'utf8');
const extract=(source,name)=>{const start=source.indexOf('function '+name+'(');assert.ok(start>=0);let end=source.indexOf('{',start)+1,depth=1;while(depth){if(source[end]==='{')depth++;if(source[end]==='}')depth--;end++;}return source.slice(start,end);};
const helpers=vm.runInNewContext(extract(common,'V')+'\n'+extract(common,'$e')+'\n({escape:V,paragraphs:$e})');
const hStart=renderer.indexOf('a="";for(let R of c.header)'),hEnd=renderer.indexOf('if(this.$columnHeaderRow.innerHTML=a',hStart);
assert.ok(hStart>=0&&hEnd>hStart);
const header=vm.runInNewContext('(function(c,e){let a;'+renderer.slice(hStart,hEnd)+'return a;})',{on:helpers.escape});
const nStart=renderer.indexOf('a="";for(let R=0;R<e.notes.length;R++)'),nEnd=renderer.indexOf('if(this.$tableFooter.innerHTML=a',nStart);
assert.ok(nStart>=0&&nEnd>nStart);
const footer=vm.runInNewContext('(function(e,N,s,rd){let a;'+renderer.slice(nStart,nEnd)+'return a;})',{on:helpers.escape,Gp:helpers.paragraphs,_:s=>s});
const records=JSON.parse(fs.readFileSync(file,'utf8'));
for(const r of records){
    const headings=header({header:r.columns.map(c=>({name:c.name,value:c.title,classes:'',sortable:false}))},{});
    const notes=footer({notes:r.notes},r.columns,[],[]);
    for(const c of r.columns)assert.ok(headings.includes(helpers.escape(c.title)));
    for(const [i,note] of r.notes.entries()){
        const marker=['¹','²','³'][i];assert.ok(note.note.startsWith(marker+' '));
        assert.equal(headings.split(marker).length-1,1,'One marker in the matching heading');
        assert.equal(notes.split(marker).length-1,1,'One matching explanatory note');
        assert.ok(notes.includes(helpers.escape(note.note)));
    }
    assert.ok(r.cells.every(cell=>cell.footnotes.length===0),'No duplicate cell markers');
    assert.equal(r.columns.some(c=>c.name==='p'),r.includeP);
    if(!r.includeP)assert.ok(!notes.includes('F-test'));
}
// Actual native reference-number component: the module selects the owning result,
// while the host still appends that result's marker component after its content.
const cEnd=renderer.indexOf('customElements.define("jmv-reference-numbers"');
const cStart=renderer.lastIndexOf('class ',cEnd);
class Element {attachShadow(){return {innerHTML:'',querySelector:()=>({innerHTML:'',insertAdjacentHTML(_,text){this.innerHTML+=text;}})};}}
const References=vm.runInNewContext('('+renderer.slice(cStart,cEnd).replace(/;\s*$/,'')+')',{HTMLElement:Element,Tg:''});
for(const refs of [[],['flexplot','graphicalAnalysis']]){
    const component=new References();component.setTable({mode:'rich',table:{flexplot:3,graphicalAnalysis:4},addEventListener(){}});component.setRefs(refs);
    assert.equal(component.count(),refs.length);
    assert.equal(component._body.innerHTML.includes('[3]'),refs.length>0);
    assert.equal(component._body.innerHTML.includes('[4]'),refs.length>0);
}
assert.ok(renderer.includes('this.refs.setRefs(this.model.attributes.refs),this.appendChild(this.refs)'));
console.log('PASS: installed native Unicode column headers, matched conditional notes, and result-owned citation placement');
