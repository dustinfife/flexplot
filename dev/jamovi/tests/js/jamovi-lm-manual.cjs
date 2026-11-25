'use strict';
const vm=require('node:vm');
module.exports=async(ui,view,fire,assert,nodes,runtime)=>{
    const supplier=nodes.find(c=>c.name==='comparisonPlotSupplier');
    assert.equal(supplier.persistentItems,false,'Assigned comparison roles must leave the supplier');
    assert.equal(nodes.find(c=>c.name==='plotSupplier').persistentItems,false);
    const start=runtime.indexOf('filterSuppliersList(e=!1){');
    assert.ok(start>=0,'Native supplier filter must be inspectable');
    let end=runtime.indexOf('{',start)+1,depth=1;
    while(depth){if(runtime[end]==='{')depth++;if(runtime[end]==='}')depth--;end++;}
    const filter=vm.runInNewContext('({'+runtime.slice(start,end)+'}).filterSuppliersList');
    Object.entries({out:'y',preds:['x','z','a'],modelTerms:[['x'],['z']],comparisonTerms:[['a']],
        comparisonX:'x',comparisonPanels:['a'],comparisonGroup:'legacy',compare:true,
        polynomials:false,comparisonPolynomials:false,quadratic:[],comparisonQuadratic:[]})
        .forEach(([name,value])=>ui[name].setValue(value));
    const visible=()=>{
        const assigned=[].concat(ui.comparisonX.value()||[],ui.comparisonPanels.value()||[]);
        const items=ui.comparisonPlotSupplier.value().map(item=>({value:item.value.raw,
            used:assigned.filter(value=>value===item.value.raw).length}));
        const shown=[];
        filter.call({blockFilterProcess:false,$searchInput:{value:''},_persistentItems:supplier.persistentItems,
            _items:items,supplier:{getRow:i=>[{setVisibility:value=>{if(value)shown.push(items[i-1].value);}}]}});
        return shown.sort();
    };
    fire('comparisonPanels','changed');assert.deepEqual(visible(),['z']);
    ui.comparisonPanels.setValue([]);fire('comparisonPanels','changed');assert.deepEqual(visible(),['a','z']);
    ui.comparisonX.setValue('z');fire('comparisonX','changed');assert.deepEqual(visible(),['a','x']);
    ui.comparisonTerms.setValue([['x']]);fire('comparisonTerms','changed');assert.deepEqual(visible(),['x']);
    ui.modelTerms.setValue([['x'],['a']]);fire('modelTerms','changed');assert.deepEqual(visible(),['a','x']);
    assert.equal(ui.comparisonX.value(),null,'Removed predictor no longer occupies x');
    ui.comparisonX.setValue('a');ui.comparisonPanels.setValue(['x']);fire('comparisonPanels','changed');assert.deepEqual(visible(),[]);
    ui.comparisonPanels.setValue([]);fire('comparisonPanels','changed');assert.deepEqual(visible(),['x']);
    assert.equal(ui.comparisonGroup.value(),'legacy','Ordinary unavailable saved grouping stays retained');
    console.log('PASS: native comparison supplier assignment/removal/union and saved grouping retention');
};
