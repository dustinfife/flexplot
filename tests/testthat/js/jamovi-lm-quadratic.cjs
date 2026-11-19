'use strict';
module.exports = function(ui, view, fire, assert) {
    const records = [];
    const snapshot = stage => {
        const names = ['preds','modelTerms','compare','comparisonTerms','polynomials','quadratic',
            'comparisonPolynomials','comparisonQuadratic'];
        records.push({stage,options:Object.fromEntries(names.map(name => [name,ui[name].value()]))});
    };
    view.workspace = {};
    ui.preds.setValue(['x','z']);ui.modelTerms.setValue([['z']]);
    ui.comparisonTerms.setValue([['x']]);ui.compare.setValue(false);
    ui.polynomials.setValue(false);ui.comparisonPolynomials.setValue(false);
    ui.quadratic.setValue([]);ui.comparisonQuadratic.setValue([]);
    fire('view','updated');snapshot('default');
    ui.polynomials.setValue(true);fire('polynomials','changed');snapshot('enabled-empty');
    assert.equal(ui.quadratic.value().length,0);
    ui.quadratic.setValue(['x']);fire('quadratic','changed');snapshot('squared');
    assert.deepEqual(JSON.parse(JSON.stringify(ui.modelTerms.value())),[['z'],['x']]);
    ui.modelTerms.setValue([['z']]);fire('modelTerms','changed');snapshot('square-only-x');
    assert.deepEqual(JSON.parse(JSON.stringify(ui.quadratic.value())),['x']);
    assert.ok(ui.plotSupplier.value().some(item=>item.value.raw==='x'));
    ui.quadratic.setValue(['x','z']);fire('quadratic','changed');snapshot('two-squares');
    ui.compare.setValue(true);ui.comparisonPolynomials.setValue(true);
    ui.comparisonQuadratic.setValue(['z']);fire('comparisonQuadratic','changed');snapshot('comparison');
    assert.deepEqual(JSON.parse(JSON.stringify(ui.quadratic.value())),['x','z']);
    assert.deepEqual(JSON.parse(JSON.stringify(ui.comparisonTerms.value())),[['x'],['z']]);
    ui.polynomials.setValue(false);fire('polynomials','changed');snapshot('disabled-primary');
    assert.equal(ui.quadratic.value().length,0);
    assert.deepEqual(JSON.parse(JSON.stringify(ui.comparisonQuadratic.value())),['z']);
    assert.deepEqual(JSON.parse(JSON.stringify(ui.modelTerms.value())),[['z'],['x']]);
    ui.comparisonPolynomials.setValue(false);fire('comparisonPolynomials','changed');snapshot('disabled-second');
    assert.equal(ui.comparisonQuadratic.value().length,0);
    // Restore independent saved options without reintroducing intentionally removed linear terms.
    ui.polynomials.setValue(true);ui.quadratic.setValue(['x']);ui.modelTerms.setValue([['z']]);
    ui.comparisonPolynomials.setValue(true);ui.comparisonQuadratic.setValue(['z']);ui.comparisonTerms.setValue([['x']]);
    view.workspace = {};fire('view','updated');snapshot('restored');
    assert.deepEqual(JSON.parse(JSON.stringify(ui.modelTerms.value())),[['z']]);
    assert.deepEqual(JSON.parse(JSON.stringify(ui.comparisonTerms.value())),[['x']]);
    ui.preds.setValue(['z']);fire('preds','changed');snapshot('removed-predictor');
    assert.equal(ui.quadratic.value().length,0);
    assert.deepEqual(JSON.parse(JSON.stringify(ui.comparisonQuadratic.value())),['z']);
    console.log('PASS: LM quadratic initialization, default hierarchy, independent models, removal and saved options.');
    return records;
};
