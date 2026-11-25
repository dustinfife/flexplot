'use strict';
// Execute the installed table-footer code with serialized notes from real R6 tables.
const fs=require('node:fs'),path=require('node:path'),vm=require('node:vm'),assert=require('node:assert/strict');
const [assets,fixtures]=process.argv.slice(2);
const files=fs.readdirSync(assets);
const renderer=fs.readFileSync(path.join(assets,files.find(name=>/^resultsview-.*\.js$/.test(name))),'utf8');
const common=fs.readFileSync(path.join(assets,files.find(name=>/^highcontrast-.*\.js$/.test(name))),'utf8');
assert.ok(renderer.includes('k as on,m as Gp'),'Verify the installed escape/paragraph imports');
assert.ok(common.includes('V as k') && common.includes('$e as m'));
const extract=name=>{
    const start=common.indexOf('function '+name+'(');
    assert.ok(start>=0);
    let end=common.indexOf('{',start)+1,depth=1;
    while(depth){if(common[end]==='{')depth++;if(common[end]==='}')depth--;end++;}
    return common.slice(start,end);
};
const functions=vm.runInNewContext(extract('V')+'\n'+extract('$e')+'\n({escape:V,paragraphs:$e})');
const hStart=renderer.indexOf('a="";for(let R of c.header)'),hEnd=renderer.indexOf('if(this.$columnHeaderRow.innerHTML=a',hStart);
assert.ok(hStart>=0 && hEnd>hStart);
const header=vm.runInNewContext('(function(c,e){let a;'+renderer.slice(hStart,hEnd)+'return a;})',{on:functions.escape});
const start=renderer.indexOf('a="";for(let R=0;R<e.notes.length;R++)');
const end=renderer.indexOf('if(this.$tableFooter.innerHTML=a',start);
assert.ok(start>=0 && end>start,'Installed native table-note footer code must be inspectable');
const footer=vm.runInNewContext('(function(e,N,s,rd){let a;'+renderer.slice(start,end)+'return a;})',
    {on:functions.escape,Gp:functions.paragraphs,_:text=>text});
const markerStart=renderer.indexOf('for(let $ of B.symbols)F.sups');
const markerEnd=renderer.indexOf('if(yg(B))',markerStart);
assert.ok(markerStart>=0 && markerEnd>markerStart);
const marker=vm.runInNewContext('(function(B,i,s,rd){const F={sups:""};'+renderer.slice(markerStart,markerEnd)+'return F.sups;})');
const symbols=JSON.parse(renderer.match(/,rd=(\[[^\]]+\])/)[1]);
const records=JSON.parse(fs.readFileSync(fixtures,'utf8'));
for(const record of records){
    const headings=header({header:record.headings},{});
    const columnNotes=record.notes.filter(note=>/^[ᵃᵇᶜᵈᵉᶠᵍʰⁱʲ] /.test(note.note));
    for(const [i,note] of columnNotes.entries()) {
        const marker=['ᵃ','ᵇ','ᶜ','ᵈ','ᵉ','ᶠ','ᵍ','ʰ','ⁱ','ʲ'][i];
        assert.ok(note.note.startsWith(marker+' '));
        assert.ok(headings.includes(marker),'Column-wide note has a visible marked header');
    }
    const footnotes=[],indices={};
    let marked=0;
    for(const cell of record.cells || []) {
        const text=marker(cell,indices,footnotes,symbols);
        if(cell.footnotes.length){assert.ok(text.length>0,'Native cell superscript must be visible');marked++;}
    }
    const html=footer({notes:record.notes},record.columns,footnotes,symbols);
    assert.equal(footnotes.length,new Set(footnotes).size,'Repeated cell markers must share one footer entry');
    for(const note of footnotes)assert.ok(html.includes(functions.escape(note)),'Every marker has explanatory text');
    if(record.familyContext && footnotes.length)assert.ok(html.indexOf(functions.escape(record.familyContext))<html.indexOf(functions.escape(footnotes[0])),'Context precedes specialized footnotes');
    const text=html.replace(/<[^>]*>/g,'').replace(/&lt;/g,'<').replace(/&gt;/g,'>').replace(/&amp;/g,'&');
    assert.equal((html.match(/class="table-note"/g)||[]).length,record.notes.length,record.name);
    assert.equal(new Set(record.notes.map(note=>note.note.trim())).size,record.notes.length,'No duplicated notes');
    for(const expected of record.expected)assert.ok(text.includes(expected),record.name+': '+expected);
    for(const forbidden of record.forbidden)assert.equal(text.includes(forbidden),false,record.name+': '+forbidden);
    assert.equal(/<(script|img|svg)\b/i.test(html),false,'Native note rendering must escape untrusted text');
    if(!record.notes.length && !footnotes.length)assert.equal(html,'','Hidden or empty tables must not leave note rows');
}
console.log('PASS: installed native table-footer code rendered '+records.length+' serialized GLM note fixtures.');
