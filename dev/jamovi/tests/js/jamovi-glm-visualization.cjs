'use strict';
const vm = require('node:vm');
// Installed native Option setters and property API; host column replies are controlled.
module.exports = async function({ui,view,handlers,restore,snapshot,assert,extract}) {
    const get = vm.runInNewContext('('+extract('getPropertyValue(e){',true)+')');
    const set = vm.runInNewContext('('+extract('setPropertyValue(e,t){',true)+')');
    for (const name of ['plotStyle','plotScale']) {
        const property = {value:true};
        property.get = () => property.value;
        property.set = value => {property.value=value;};
        Object.assign(ui[name],{properties:{enable:property},params:{},getPropertyValue:get,
            setPropertyValue:set,firePropertyChangedEvent(){},onPropertyChanged(){}});
    }
    const column = name => ({columnFound:true,measureType:name === 'a' ? 'nominal' : 'continuous',dataType:name === 'a' ? 'integer' : 'decimal'});
    view.requestData = (type,query) => {
        assert.equal(type,'column');
        assert.equal(JSON.stringify(query.properties),'["measureType","dataType"]');
        return Promise.resolve(column(query.columnName));
    };
    const settle = () => new Promise(resolve=>setImmediate(resolve));
    const enabled = name => ui[name].getPropertyValue('enable');
    const equal = (a,b) => assert.equal(JSON.stringify(a),JSON.stringify(b));
    restore('display-restored',{out:'y',preds:['x','z','a'],family:'binomial',plotX:'x',plotScale:'logit'});
    await settle();assert.equal(enabled('plotScale'),true);assert.equal(ui.plotScale.value(),'logit');
    const terms = JSON.stringify(ui.modelTerms.value());
    ui.plotStyle.setValue('raw');await settle();
    assert.equal(ui.plotScale.value(),'probability');assert.equal(enabled('plotScale'),false);
    // Even a stale programmatic selection is normalized by the actual handler.
    ui.plotScale.setValue('logit');await settle();assert.equal(ui.plotScale.value(),'probability');
    ui.plotStyle.setValue('auto');await settle();assert.equal(enabled('plotScale'),true);
    ui.plotScale.setValue('logit');ui.plotX.setValue('a');await settle();
    assert.equal(enabled('plotScale'),false);assert.equal(enabled('plotStyle'),false);
    assert.equal(ui.plotScale.value(),'probability');
    assert.equal(JSON.stringify(ui.modelTerms.value()),terms);
    ui.plotX.setValue('x');await settle();assert.equal(enabled('plotStyle'),true);
    assert.equal(enabled('plotScale'),true);assert.equal(ui.plotScale.value(),'probability');
    ui.family.setValue('poisson');await settle();assert.equal(enabled('plotScale'),false);
    ui.family.setValue('binomial');await settle();assert.equal(enabled('plotScale'),true);
    ui.compare.setValue(true);ui.comparisonX.setValue('a');await settle();
    assert.equal(enabled('plotScale'),false); // The scale applies to both plots.
    ui.comparisonX.setValue('z');await settle();assert.equal(enabled('plotScale'),true);
    ui.plotScale.setValue('logit');await settle();
    const saved = JSON.parse(JSON.stringify(snapshot('display-saved').options));
    restore('display-reopened',saved);await settle();
    assert.equal(ui.plotScale.value(),'logit');equal(ui.modelTerms.value(),saved.modelTerms);
    equal(ui.comparisonTerms.value(),saved.comparisonTerms);
    restore('legacy-raw-logit',{...saved,plotStyle:'raw',plotScale:'logit'});await settle();
    assert.equal(ui.plotScale.value(),'probability');
    restore('legacy-categorical-logit',{...saved,plotX:'a',plotScale:'logit'});await settle();
    assert.equal(ui.plotScale.value(),'probability');
    // Out-of-order host replies cannot re-enable a categorical control.
    const replies=[];
    view.requestData = (type,query) => new Promise(resolve=>replies.push(()=>resolve(column(query.columnName))));
    ui.compare.setValue(false);ui.plotX.setValue('x');ui.plotX.setValue('a');
    for (const reply of replies.reverse()) reply();await settle();
    assert.equal(enabled('plotScale'),false);assert.equal(enabled('plotStyle'),false);
    // Restoration replaces workspace; old replies must not collide with a new token.
    replies.length=0;view.workspace={};ui.plotX.setValue('x');
    const old=replies.splice(0);
    view.workspace={};ui.plotX.setValue('a');
    for (const reply of replies) reply();await settle();
    for (const reply of old) reply();await settle();
    assert.equal(enabled('plotScale'),false);assert.equal(enabled('plotStyle'),false);
    // Clearing main effects via the full builder preserves square-only/interaction-only specifications.
    view.requestData = (type,query)=>Promise.resolve(column(query.columnName));
    restore('no-duplicate-main',{out:'y',preds:['x','z'],interactions:true,interactionSpecVersion:2,modelTerms:[['x','z']],comparisonTerms:[['z']],compare:true});
    await settle();equal(ui.modelTerms.value(),[['x','z']]);equal(ui.comparisonTerms.value(),[['z']]);
    assert.equal(ui.mainEffects.value().length,0);
    console.log('PASS: native GLM display constraints, legacy restoration, stale host replies and independent model terms.');
};
