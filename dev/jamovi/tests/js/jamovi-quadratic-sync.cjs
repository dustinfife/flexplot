'use strict';
const fs=require('node:fs'),vm=require('node:vm'),assert=require('node:assert/strict');
const [root]=process.argv.slice(2);
const sandbox={module:{exports:{}},utils:{valuesToItems:(values,format)=>values.map(value=>({value,format}))},
    FormatDef:{variable:'variable'}};
vm.runInNewContext(fs.readFileSync(root+'/jamovi/js/model-terms.js','utf8'),sandbox);
const sync=sandbox.module.exports.syncQuadratic;
const equal=(actual,expected)=>assert.equal(JSON.stringify(actual),JSON.stringify(expected));
const control=initial=>{
    let value=initial,writes=0;
    return {value:()=>value,setValue:next=>{value=next;writes++;},writes:()=>writes};
};
const ui={quadratic:control(['has space','removed']),modelTerms:control([]),supplier:control([]),
    comparisonQuadratic:control(['tick`name']),comparisonTerms:control(null),secondSupplier:control([]),
    randomSlopes:control(['random-only'])};
const predictors=['has space','a+b'];
sync(ui,predictors,'quadratic','modelTerms','supplier',false);
equal(ui.quadratic.value(),['has space']);equal(ui.modelTerms.value(),[]);
equal(ui.supplier.value().map(item=>item.value),predictors);
equal(ui.comparisonQuadratic.value(),['tick`name']);equal(ui.comparisonTerms.value(),null);
equal(ui.randomSlopes.value(),['random-only']);equal(predictors,['has space','a+b']);
sync(ui,['tick`name'],'comparisonQuadratic','comparisonTerms','secondSupplier',false);
equal(ui.comparisonTerms.value(),null);
sync(ui,predictors,'quadratic','modelTerms','supplier',true);
equal(ui.modelTerms.value(),[['has space']]);
const writes=ui.modelTerms.writes();
sync(ui,predictors,'quadratic','modelTerms','supplier',true);
assert.equal(ui.modelTerms.writes(),writes,'No duplicate linear terms or redundant option writes');
ui.modelTerms.setValue([['has space','a+b']]);
sync(ui,predictors,'quadratic','modelTerms','supplier',false);
equal(ui.modelTerms.value(),[['has space','a+b']],'Restoration must not add main effects');
sync(ui,predictors,'quadratic','modelTerms','supplier',true);
equal(ui.modelTerms.value(),[['has space','a+b'],['has space']]);
sync(ui,[],'quadratic','modelTerms','supplier',false);
equal(ui.quadratic.value(),[]);equal(ui.supplier.value(),[]);
equal(ui.modelTerms.value(),[['has space','a+b'],['has space']],'Main-term pruning remains the term synchronizer’s responsibility');
ui.quadratic.setValue(null);ui.modelTerms.setValue(null);
sync(ui,predictors,'quadratic','modelTerms','supplier',true);
equal(ui.quadratic.value(),[]);equal(ui.modelTerms.value(),null,'An empty quadratic selection must not repopulate terms');
console.log('PASS: shared quadratic filtering, suppliers, literal names, default linear terms and model independence.');
