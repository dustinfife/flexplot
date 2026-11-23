'use strict';
const vm = require('node:vm');
// Installed native Option setters and property API; host column replies are controlled.
module.exports = async function({ui,view,handlers,restore,snapshot,assert,extract}) {
    const get = vm.runInNewContext('('+extract('getPropertyValue(e){',true)+')');
    const set = vm.runInNewContext('('+extract('setPropertyValue(e,t){',true)+')');
    for (const name of ['plotStyle','plotScale','comparisonPlotStyle','comparisonPlotScale','ghostLines']) {
        const property = {value:true};
        property.get = () => property.value;
        property.set = value => {property.value=value;};
        Object.assign(ui[name],{properties:{enable:property},params:{},getPropertyValue:get,
            setPropertyValue:set,firePropertyChangedEvent(){},onPropertyChanged(){}});
    }
    const column = name => ({columnFound:true,measureType:name === 'a' ? 'nominal' : 'continuous',dataType:name === 'a' ? 'integer' : 'decimal',levels:name === 'a' ? [{label:'A'},{label:'B'}] : []});
    view.requestData = (type,query) => {
        assert.equal(type,'column');
        assert.ok(['["measureType","dataType"]','["measureType","dataType","levels"]'].includes(JSON.stringify(query.properties)));
        return Promise.resolve(column(query.columnName));
    };
    const settle = () => new Promise(resolve=>setImmediate(resolve));
    const enabled = name => ui[name].getPropertyValue('enable');
    const equal = (a,b) => assert.equal(JSON.stringify(a),JSON.stringify(b));
    restore('display-restored',{out:'y',preds:['x','z','a'],family:'binomial',plotX:'x',plotScale:'logit'});
    await settle();assert.equal(enabled('plotScale'),true);assert.equal(ui.plotScale.value(),'logit');
    assert.equal(ui.comparisonPlotScale.value(),'logit','Legacy shared scale migrates once');
    const terms = JSON.stringify(ui.modelTerms.value());
    ui.compare.setValue(true);ui.comparisonX.setValue('z');await settle();
    ui.plotStyle.setValue('raw');await settle();
    assert.equal(ui.plotScale.value(),'logit','Unavailable preferences are retained');
    assert.equal(enabled('plotScale'),false);assert.equal(enabled('comparisonPlotScale'),true);
    assert.equal(ui.comparisonPlotStyle.value(),'auto');
    ui.plotStyle.setValue('auto');ui.plotX.setValue('a');await settle();
    assert.equal(enabled('plotScale'),false);assert.equal(enabled('plotStyle'),false);
    assert.equal(enabled('comparisonPlotScale'),true);assert.equal(ui.comparisonPlotScale.value(),'logit');
    assert.equal(JSON.stringify(ui.modelTerms.value()),terms);
    ui.plotX.setValue('x');ui.comparisonX.setValue('a');await settle();
    assert.equal(enabled('plotScale'),true);assert.equal(enabled('comparisonPlotScale'),false);
    assert.equal(ui.plotScale.value(),'logit');
    ui.comparisonX.setValue('z');ui.comparisonPlotStyle.setValue('raw');await settle();
    assert.equal(enabled('plotScale'),true);assert.equal(enabled('comparisonPlotScale'),false);
    ui.se.setValue(false);assert.equal(ui.comparisonSe.value(),true);
    ui.comparisonSe.setValue(false);ui.se.setValue(true);assert.equal(ui.comparisonSe.value(),false);
    ui.family.setValue('poisson');await settle();
    for (const name of ['plotStyle','plotScale','comparisonPlotStyle','comparisonPlotScale']) assert.equal(enabled(name),false);
    ui.family.setValue('binomial');await settle();
    assert.equal(enabled('plotScale'),true);assert.equal(enabled('comparisonPlotScale'),false);
    ui.comparisonPlotStyle.setValue('auto');ui.comparisonPlotScale.setValue('probability');await settle();
    const saved = JSON.parse(JSON.stringify(snapshot('display-saved').options));
    restore('display-reopened',saved);await settle();
    for (const name of ['se','comparisonSe','plotScale','comparisonPlotScale','plotStyle','comparisonPlotStyle']) equal(ui[name].value(),saved[name]);
    equal(ui.modelTerms.value(),saved.modelTerms);equal(ui.comparisonTerms.value(),saved.comparisonTerms);
    ui.compare.setValue(false);ui.compare.setValue(true);await settle();
    assert.equal(ui.comparisonPlotScale.value(),'probability');assert.equal(ui.comparisonSe.value(),false);
    restore('legacy-raw-logit',{...saved,outputOptionsVersion:1,se:false,plotStyle:'raw',plotScale:'logit'});await settle();
    assert.equal(ui.plotScale.value(),'logit');assert.equal(ui.comparisonPlotScale.value(),'logit');
    assert.equal(ui.comparisonPlotStyle.value(),'raw');assert.equal(ui.comparisonSe.value(),false);
    assert.equal(ui.outputOptionsVersion.value(),2);
    // Independent request identities: a main edit neither cancels nor repeats
    // an outstanding comparison request, and both reject stale replies.
    const replies=[];
    view.requestData = (type,query) => new Promise(resolve=>replies.push({name:query.columnName,resolve:()=>resolve(column(query.columnName))}));
    restore('asynchronous-displays',{out:'y',preds:['x','z','a'],family:'binomial',compare:true,
        outputOptionsVersion:2,plotX:'x',comparisonX:'z',plotScale:'logit',comparisonPlotScale:'logit'});
    const comparisonCount = replies.filter(r=>r.name==='z').length;
    ui.plotX.setValue('a');
    assert.equal(replies.filter(r=>r.name==='z').length,comparisonCount);
    replies.filter(r=>r.name==='z').forEach(r=>r.resolve());await settle();
    assert.equal(enabled('comparisonPlotScale'),true);
    replies.filter(r=>r.name==='a').forEach(r=>r.resolve());await settle();
    replies.filter(r=>r.name==='x').forEach(r=>r.resolve());await settle();
    assert.equal(enabled('plotScale'),false);assert.equal(enabled('plotStyle'),false);
    ui.plotX.setValue('x');replies.filter(r=>r.name==='x').forEach(r=>r.resolve());await settle();
    ui.comparisonX.setValue('x');ui.comparisonX.setValue('a');
    for (const reply of replies.slice().reverse()) reply.resolve();await settle();
    assert.equal(enabled('plotScale'),true);assert.equal(enabled('comparisonPlotScale'),false);
    // Replacing the restoration workspace invalidates requests from the old one.
    replies.length=0;view.workspace={};ui.plotX.setValue('z');
    const old=replies.splice(0);
    view.workspace={};ui.plotX.setValue('a');
    for (const reply of replies) reply.resolve();await settle();
    for (const reply of old) reply.resolve();await settle();
    assert.equal(enabled('plotScale'),false);assert.equal(enabled('comparisonPlotScale'),false);
    // Clearing main effects via the full builder preserves square-only/interaction-only specifications.
    view.requestData = (type,query)=>Promise.resolve(column(query.columnName));
    restore('no-duplicate-main',{out:'y',preds:['x','z'],interactions:true,interactionSpecVersion:2,modelTerms:[['x','z']],comparisonTerms:[['z']],compare:true});
    await settle();equal(ui.modelTerms.value(),[['x','z']]);equal(ui.comparisonTerms.value(),[['z']]);
    assert.equal(ui.mainEffects.value().length,0);
    restore('ghost-fresh',{out:'y',preds:['x','z','a'],family:'binomial',plotX:'x'});
    await settle();assert.equal(ui.ghostLines.value(),false);assert.equal(enabled('ghostLines'),false);
    ui.plotPanels.setValue(['a']);await settle();assert.equal(enabled('ghostLines'),true);
    ui.ghostLines.setValue(true);await settle();assert.equal(ui.compare.value(),false);
    const ghostSaved=JSON.parse(JSON.stringify(snapshot('ghost-saved').options));
    restore('ghost-reopened',ghostSaved);await settle();
    assert.equal(ui.ghostLines.value(),true);assert.equal(enabled('ghostLines'),true);
    ui.plotPanels.setValue(['a','z']);await settle();assert.equal(enabled('ghostLines'),true);
    ui.plotPanels.setValue(['z']);await settle();
    ui.plotGroup.setValue('a');await settle();assert.equal(enabled('ghostLines'),false);
    assert.equal(ui.ghostLines.value(),true);
    ui.plotGroup.setValue(null);ui.plotPanels.setValue(['a']);await settle();assert.equal(enabled('ghostLines'),true);
    for(const family of ['gaussian','binomial','poisson','gammaLog','Gamma','negativebinomial']) {
        ui.family.setValue(family);await settle();assert.equal(enabled('ghostLines'),true);
    }
    ui.plotX.setValue('a');ui.plotPanels.setValue(['z']);await settle();assert.equal(enabled('ghostLines'),false);
    ui.plotX.setValue('x');ui.plotPanels.setValue([]);await settle();assert.equal(enabled('ghostLines'),false);
    ui.plotPanels.setValue(['a']);ui.graphic.setValue(false);await settle();assert.equal(enabled('ghostLines'),false);
    ui.graphic.setValue(true);await settle();assert.equal(enabled('ghostLines'),true);
    ui.compare.setValue(true);await settle();assert.equal(enabled('ghostLines'),true);
    ui.compare.setValue(false);await settle();assert.equal(enabled('ghostLines'),true);
    // A one-level categorical panel must not enable a meaningless comparison.
    view.requestData=(type,query)=>Promise.resolve({...column(query.columnName),levels:query.columnName==='a'?[{label:'A'}]:[]});
    handlers.view_updated.call(view,ui);await settle();assert.equal(enabled('ghostLines'),false);
    assert.equal(ui.ghostLines.value(),true);
    const ghostReplies=[];
    view.requestData=(type,query)=>new Promise(resolve=>ghostReplies.push(()=>resolve(column(query.columnName))));
    handlers.view_updated.call(view,ui);ui.plotPanels.setValue([]);
    for(const reply of ghostReplies.reverse())reply();await settle();assert.equal(enabled('ghostLines'),false);
    view.requestData=(type,query)=>Promise.resolve(column(query.columnName));
    restore('ghost-before-empty-restoration',ghostSaved);await settle();assert.equal(enabled('ghostLines'),true);
    view.flags.updating=true;ui.preds.setValue([]);ui.out.setValue(null);view.flags.updating=false;
    handlers.view_updated.call(view,ui);await settle();
    assert.equal(enabled('ghostLines'),false);assert.equal(ui.ghostLines.value(),true);
    console.log('PASS: independent native GLM display constraints, versioned restoration, stale host replies and independent model terms.');
};
