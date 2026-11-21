'use strict';
// Exercise the installed Option/View lifecycle through the bundled analysis handlers.
module.exports = function({ui,native,view,restore,snapshot,assert,analysis}) {
    const records = [];
    const equal = (actual,expected) => assert.equal(JSON.stringify(actual),JSON.stringify(expected));
    const save = stage => records.push(snapshot(stage));
    const additive = [['x'],['z']];
    const interacting = additive.concat([['x','z']]);
    restore('interaction-fresh',{out:'y',preds:['x','z']});
    assert.equal(native.interactionSpecVersion.getValue(),analysis === 'glmgeneralized' ? 2 : 1);
    assert.equal(ui.interactions.value(),false);
    assert.equal(ui.comparisonInteractions.value(),false);
    equal(ui.mainEffects.value(),['x','z']);
    equal(ui.modelTerms.value(),additive);
    save('fresh');
    ui.interactions.setValue(true);
    equal(ui.modelTerms.value(),additive);save('enabled-empty');
    ui.mainEffects.setValue(['x']);
    equal(ui.modelTerms.value(),[['x']]);save('main-effects-edited');
    ui.mainEffects.setValue(['x','z']);
    ui.modelTerms.setValue(interacting);
    equal(ui.mainEffects.value(),['x','z']);save('interaction');
    ui.compare.setValue(true);
    ui.comparisonMainEffects.setValue(['z']);
    equal(ui.comparisonTerms.value(),[['z']]);
    ui.comparisonInteractions.setValue(true);
    equal(ui.comparisonTerms.value(),[['z']]);
    ui.comparisonTerms.setValue([['z'],['x','z']]);
    equal(ui.modelTerms.value(),interacting);save('independent-second');
    ui.interactions.setValue(false);
    equal(ui.modelTerms.value(),additive);
    equal(ui.comparisonTerms.value(),[['z'],['x','z']]);save('primary-off');
    ui.compare.setValue(false);
    equal(ui.comparisonTerms.value(),[['z'],['x','z']]);save('comparison-off');
    ui.compare.setValue(true);save('comparison-on');
    for (let i=0;i<2;i++) {
        ui.interactions.setValue(true);
        equal(ui.modelTerms.value(),additive);
        ui.modelTerms.setValue(interacting);
        ui.interactions.setValue(false);
        equal(ui.modelTerms.value(),additive);
        assert.equal(native.interactionSpecVersion.getValue(),analysis === 'glmgeneralized' ? 2 : 1);
    }
    save('repeated-edits');
    ui.comparisonInteractions.setValue(false);
    equal(ui.comparisonTerms.value(),[['z']]);
    equal(ui.modelTerms.value(),additive);save('second-off');
    ui.comparisonInteractions.setValue(true);
    equal(ui.comparisonTerms.value(),[['z']]);
    ui.comparisonTerms.setValue([['z'],['x','z']]);
    ui.interactions.setValue(true);ui.modelTerms.setValue([['x','z']]);
    equal(ui.mainEffects.value(),[]);save('interaction-only');
    // Removing an ordinary main effect must not remove an allowed interaction.
    ui.mainEffects.setValue(['x']);ui.mainEffects.setValue([]);
    equal(ui.modelTerms.value(),[['x','z']]);
    const legacy = {out:'y',preds:['x','z'],modelTerms:interacting,
        comparisonTerms:[['z'],['x','z']],compare:false};
    restore('legacy-both-models',legacy);
    assert.equal(ui.interactions.value(),true);
    assert.equal(ui.comparisonInteractions.value(),true);
    equal(ui.mainEffects.value(),['x','z']);
    equal(ui.comparisonMainEffects.value(),['z']);save('legacy-comparison-off');
    ui.compare.setValue(true);save('legacy-enabled');
    ui.compare.setValue(false);
    // Staged native restoration is performed by restore() under flags.updating;
    // its assertions require zero writes until the installed view.ready handler.
    const persisted = snapshot('saved').options;
    restore('interaction-reopened',JSON.parse(JSON.stringify(persisted)));
    equal(ui.modelTerms.value(),interacting);
    equal(ui.comparisonTerms.value(),legacy.comparisonTerms);save('reopened');
    ui.interactions.setValue(false);ui.comparisonInteractions.setValue(false);
    const disabled = snapshot('disabled-saved').options;
    restore('explicit-off-reopened',JSON.parse(JSON.stringify(disabled)));
    assert.equal(ui.interactions.value(),false);
    assert.equal(ui.comparisonInteractions.value(),false);
    equal(ui.modelTerms.value(),additive);
    equal(ui.comparisonTerms.value(),[['z']]);save('explicit-off-reopened');
    restore('legacy-empty-primary',{out:'y',preds:['x','z'],modelTerms:[],comparisonTerms:null});
    equal(ui.modelTerms.value(),[]);
    equal(ui.mainEffects.value(),[]);
    equal(ui.comparisonTerms.value(),additive);save('empty-primary');
    ui.interactions.setValue(true);
    equal(ui.modelTerms.value(),[]);save('empty-enabled');
    // Real native event setters must defer ALL dependent state during restoration.
    const before = JSON.stringify(snapshot('before-staging').options);
    view.flags.updating = true;
    native.modelTerms.setValue(interacting);native.interactions.setValue(false);
    native.interactionSpecVersion.setValue(0);
    const partial = JSON.stringify(snapshot('partial').options);
    for (const name of ['primaryMainSupplier','comparisonMainSupplier','primarySupplier','comparisonSupplier'])
        ui[name].setValue([]);
    equal(JSON.stringify(snapshot('still-partial').options),partial);
    view.flags.updating = false;
    // Complete restoration with a ready event, rather than bypassing its lifecycle.
    restore('staged-legacy',legacy);
    assert.notEqual(JSON.stringify(snapshot('complete').options),before);
    equal(ui.modelTerms.value(),interacting);save('staged-legacy');
    console.log('PASS: independent interaction switches, main effects, legacy migration and saved native options.');
    return records;
};
