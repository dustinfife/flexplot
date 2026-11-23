'use strict';

// Runs through the compiled view's installed automatic event bindings.
module.exports = function(ui, view, fire, assert) {
    fire('view','loaded');
    ui.casGhost.setValue(true);
    ui.cluster.setValue('id'); fire('cluster','changed');
    assert.equal(ui.plotX.value(), null);
    assert.deepEqual(JSON.parse(JSON.stringify(ui.plotPanels.value())), ['id']);
    ui.preds.setValue(['x','a']); fire('preds','changed');
    assert.equal(JSON.stringify(ui.modelTerms.value()), '[["x"],["a"]]');
    assert.equal(ui.plotX.value(), 'x');
    assert.equal(JSON.stringify(ui.plotPanels.value()), '["id"]');
    ui.interactions.setValue(true); fire('interactions','changed');
    ui.polynomials.setValue(true); fire('polynomials','changed');
    ui.modelTerms.setValue([['x'],['a'],['x','a']]); fire('modelTerms','changed');
    ui.randomSlopes.setValue(['x']); fire('randomSlopes','changed');
    ui.quadratic.setValue(['x']); fire('quadratic','changed');
    assert.equal(JSON.stringify(ui.modelTerms.value()), '[["x"],["a"],["x","a"]]');
    // Saved options retain independent random structure and explicit fixed terms.
    view.workspace = {};
    ui.modelTerms.setValue([['a']]); ui.plotX.setValue('x');
    ui.plotPanels.setValue([]); ui.plotGroup.setValue('id');
    fire('view','updated');
    assert.equal(JSON.stringify(ui.modelTerms.value()), '[["a"]]');
    assert.equal(JSON.stringify(ui.randomSlopes.value()), '["x"]');
    assert.equal(JSON.stringify(ui.quadratic.value()), '["x"]');
    assert.equal(ui.plotGroup.value(), 'id');
    assert.equal(ui.plotPanels.value().length, 0);
    assert.equal(ui.plotX.value(), 'x');
    ui.preds.setValue(['a']); fire('preds','changed');
    assert.equal(ui.randomSlopes.value().length, 0);
    assert.equal(ui.quadratic.value().length, 0);
    assert.equal(ui.plotX.value(), 'a');
    assert.equal(ui.plotGroup.value(), 'id');
    ui.cluster.setValue('school'); fire('cluster','changed');
    assert.equal(ui.plotGroup.value(), null);
    assert.equal(JSON.stringify(ui.plotPanels.value()), '["school"]');
    ui.plotPanels.setValue(['a','school']); fire('plotPanels','changed');
    assert.equal(JSON.stringify(ui.plotPanels.value()), '["school"]');
    ui.modelTerms.setValue([]); fire('modelTerms','changed');
    assert.equal(ui.plotX.value(), null);
    assert.equal(JSON.stringify(ui.plotPanels.value()), '["school"]');
    assert.equal(ui.casGhost.value(), true, 'Unrelated updates must preserve the saved CAS ghost choice');
    // A legacy saved comparison requests one non-passive calculation update.
    ui.preds.setValue(['x','a']);
    ui.modelTerms.setValue([['x']]);
    ui.comparisonFixed.setValue(['x','a']);
    ui.comparisonTerms.setValue([['x'],['a']]);
    ui.samplePolicyVersion.setValue(1);
    ui.compare.setValue(true);
    fire('view','updated');
    assert.equal(ui.samplePolicyVersion.value(), 2);
    assert.equal(JSON.stringify(ui.modelTerms.value()), '[["x"]]');
    assert.equal(JSON.stringify(ui.comparisonTerms.value()), '[["x"],["a"]]');
    let changes = 0;
    const setRevision = ui.samplePolicyVersion.setValue;
    ui.samplePolicyVersion.setValue = value => { changes++; setRevision(value); };
    fire('view','updated');
    assert.equal(changes, 0, 'A current saved comparison must not request another recalculation');
    ui.preds.setValue(['x','a']); ui.modelTerms.setValue([['x'],['a']]);
    ui.plotX.setValue('x'); ui.plotGroup.setValue('a'); ui.compare.setValue(true);
    for (const type of ['population','cluster','cas']) {
        ui.predictionType.setValue(type);fire('predictionType','changed');
        assert.equal(ui.plotGroup.value(),'a','Retain the shared standalone/CAS group');
        const event={value:'a'};view.handlers.plotGroup_changing.call(view,ui,event);
        assert.equal(event.cancel,type==='cas' ? undefined : true);
    }
    ui.compare.setValue(false);fire('compare','changed');
    const event={value:'a'};view.handlers.plotGroup_changing.call(view,ui,event);assert.equal(event.cancel,undefined);
    console.log('PASS: Mixed Model initialization, independent random slopes, quadratics, role synchronization and restored layouts.');
};
