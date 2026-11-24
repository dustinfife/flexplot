'use strict';
module.exports = async function(ui,view,fire,assert,module) {
    const values = {out:'y',preds:['a','x','z'],modelTerms:[['a'],['x'],['z']],quadratic:[],
        plotX:null,plotPanels:['a'],plotGroup:null,graphic:true,ghostLines:true,compare:false};
    if(module==='mixedmodel')Object.assign(values,{cluster:'id',predictionType:'population',preds:['x','a','z'],modelTerms:[['x'],['a'],['z']]});
    for(const [name,value] of Object.entries(values))ui[name].setValue(value);
    const column=name=>({columnFound:true,measureType:['a','id'].includes(name)?'nominal':'continuous',dataType:['a','id'].includes(name)?'integer':'decimal',levels:['a','id'].includes(name)?[{label:'A'},{label:'B'}]:[]});
    view.requestData=(type,query)=>Promise.resolve(column(query.columnName));
    const settle=()=>new Promise(resolve=>setImmediate(resolve));
    fire('ghostLines','changed');await settle();
    const enabled=()=>ui.ghostLines.getPropertyValue('enable');
    assert.equal(enabled(),true,'Automatic continuous x should enable panel ghosts');
    ui.plotX.setValue('x');fire('ghostLines','changed');await settle();assert.equal(enabled(),true);
    ui.plotPanels.setValue(['a','z']);fire('ghostLines','changed');await settle();assert.equal(enabled(),true);
    ui.plotGroup.setValue('z');fire('ghostLines','changed');await settle();assert.equal(enabled(),false);
    assert.equal(ui.ghostLines.value(),true,'Unsupported contexts retain the preference');
    ui.plotGroup.setValue(null);ui.plotX.setValue('a');fire('ghostLines','changed');await settle();assert.equal(enabled(),false);
    ui.plotX.setValue('x');ui.plotPanels.setValue(['a']);ui.graphic.setValue(false);fire('graphic','changed');await settle();assert.equal(enabled(),false);
    ui.graphic.setValue(true);fire('graphic','changed');await settle();assert.equal(enabled(),true);
    view.requestData=(type,query)=>Promise.resolve({...column(query.columnName),levels:query.columnName==='a'?[{label:'only'}]:[]});
    fire('ghostLines','changed');await settle();assert.equal(enabled(),false);
    const replies=[];view.requestData=(type,query)=>new Promise(resolve=>replies.push(()=>resolve(column(query.columnName))));
    fire('ghostLines','changed');ui.plotPanels.setValue([]);fire('ghostLines','changed');replies.reverse().forEach(reply=>reply());await settle();assert.equal(enabled(),false);
    view.requestData=(type,query)=>Promise.resolve(column(query.columnName));ui.plotPanels.setValue(['a']);
    ui.compare.setValue(true);fire('ghostLines','changed');await settle();assert.equal(enabled(),true);
    if(module==='mixedmodel') {
        ui.compare.setValue(false);ui.predictionType.setValue('cas');fire('ghostLines','changed');await settle();assert.equal(enabled(),true);
        ui.predictionType.setValue('cluster');ui.plotPanels.setValue([]);fire('ghostLines','changed');await settle();assert.equal(enabled(),true);
        ui.plotX.setValue(null);ui.modelTerms.setValue([['a'],['x']]);fire('ghostLines','changed');await settle();assert.equal(enabled(),false,'Mixed automatic x keeps the first-predictor convention');
    }
    console.log('PASS: shared native-handler ghost eligibility and preference retention: '+module);
};
