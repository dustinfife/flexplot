'use strict';

// Runs through the compiled view's installed automatic event bindings.
module.exports = function(ui, view, fire, assert) {
    fire('view','loaded');
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
    console.log('PASS: Mixed Model initialization, independent random slopes, quadratics, role synchronization and restored layouts.');
};
