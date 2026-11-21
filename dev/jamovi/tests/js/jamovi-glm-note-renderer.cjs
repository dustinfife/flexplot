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
const start=renderer.indexOf('a="";for(let R=0;R<e.notes.length;R++)');
const end=renderer.indexOf('if(this.$tableFooter.innerHTML=a',start);
assert.ok(start>=0 && end>start,'Installed native table-note footer code must be inspectable');
const footer=vm.runInNewContext('(function(e,N){let a;const s=[],rd=[];'+renderer.slice(start,end)+'return a;})',
    {on:functions.escape,Gp:functions.paragraphs,_:text=>text});
const records=JSON.parse(fs.readFileSync(fixtures,'utf8'));
for(const record of records){
    const html=footer({notes:record.notes},record.columns);
    const text=html.replace(/<[^>]*>/g,'').replace(/&lt;/g,'<').replace(/&gt;/g,'>').replace(/&amp;/g,'&');
    assert.equal((html.match(/class="table-note"/g)||[]).length,record.notes.length,record.name);
    assert.equal(new Set(record.notes.map(note=>note.note.trim())).size,record.notes.length,'No duplicated notes');
    for(const expected of record.expected)assert.ok(text.includes(expected),record.name+': '+expected);
    for(const forbidden of record.forbidden)assert.equal(text.includes(forbidden),false,record.name+': '+forbidden);
    assert.equal(/<(script|img|svg)\b/i.test(html),false,'Native note rendering must escape untrusted text');
    if(!record.notes.length)assert.equal(html,'','Hidden or empty tables must not leave note rows');
}
console.log('PASS: installed native table-footer code rendered '+records.length+' serialized GLM note fixtures.');
