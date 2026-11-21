'use strict';
// Use the installed Option/View lifecycle and automatic handler bindings.
module.exports = function({ui,native,view,restore,snapshot,assert}) {
    const records = [];
    const equal = (a,b) => assert.equal(JSON.stringify(a),JSON.stringify(b));
    const save = stage => records.push(snapshot(stage));
    const additive = [['x'],['z']];
    restore('polynomial-fresh',{out:'y',preds:['x','z']});
    assert.equal(ui.polynomials.value(),false);
    assert.equal(ui.comparisonPolynomials.value(),false);
    assert.equal(native.interactionSpecVersion.getValue(),2);
    save('fresh');
    ui.polynomials.setValue(true);
    equal(ui.quadratic.value(),[]);equal(ui.modelTerms.value(),additive);save('enabled-empty');
    ui.quadratic.setValue(['x']);equal(ui.modelTerms.value(),additive);save('quadratic');
    ui.modelTerms.setValue([['z']]);equal(ui.quadratic.value(),['x']);
    ui.modelTerms.setValue([['x'],['z']]);ui.modelTerms.setValue([['z']]);
    equal(ui.quadratic.value(),['x']);save('square-only');
    ui.quadratic.setValue(['x','z']);equal(ui.modelTerms.value(),[['z'],['x']]);save('two-squares');
    ui.interactions.setValue(true);ui.modelTerms.setValue([['x'],['z'],['x','z']]);save('interaction-squares');
    ui.compare.setValue(true);ui.comparisonPolynomials.setValue(true);
    equal(ui.comparisonQuadratic.value(),[]);
    ui.comparisonQuadratic.setValue(['z']);ui.comparisonMainEffects.setValue(['x']);
    equal(ui.quadratic.value(),['x','z']);save('independent-second');
    ui.compare.setValue(false);save('comparison-off');
    ui.compare.setValue(true);equal(ui.comparisonQuadratic.value(),['z']);save('comparison-on');
    ui.polynomials.setValue(false);
    equal(ui.quadratic.value(),[]);equal(ui.comparisonQuadratic.value(),['z']);
    equal(ui.modelTerms.value(),[['x'],['z'],['x','z']]);save('primary-off');
    ui.comparisonPolynomials.setValue(false);equal(ui.comparisonQuadratic.value(),[]);
    equal(ui.comparisonTerms.value(),[['x']]);save('second-off');
    for (let i=0;i<2;i++) {
        ui.polynomials.setValue(true);equal(ui.quadratic.value(),[]);
        ui.quadratic.setValue(['x']);ui.polynomials.setValue(false);
        equal(ui.modelTerms.value(),[['x'],['z'],['x','z']]);
    }
    save('repeated-edits');
    const disabled = JSON.parse(JSON.stringify(snapshot('disabled').options));
    restore('polynomial-off-reopened',disabled);
    assert.equal(ui.polynomials.value(),false);assert.equal(ui.comparisonPolynomials.value(),false);
    save('off-reopened');
    const legacy = {out:'y',preds:['x','z'],modelTerms:[['z']],quadratic:['x'],
        comparisonTerms:[['x']],comparisonQuadratic:['z'],compare:false};
    for (const version of [0,1]) {
        restore('legacy-polynomial-'+version,{...legacy,interactionSpecVersion:version});
        assert.equal(ui.polynomials.value(),true);assert.equal(ui.comparisonPolynomials.value(),true);
        equal(ui.modelTerms.value(),[['z']]);equal(ui.comparisonTerms.value(),[['x']]);
        save('legacy-'+version);
    }
    ui.compare.setValue(true);save('legacy-enabled');
    restore('polynomial-reopened',JSON.parse(JSON.stringify(snapshot('persisted').options)));
    equal(ui.modelTerms.value(),[['z']]);equal(ui.comparisonTerms.value(),[['x']]);save('reopened');
    ui.preds.setValue(['z']);equal(ui.quadratic.value(),[]);equal(ui.comparisonQuadratic.value(),['z']);save('removed-predictor');
    restore('polynomial-empty',{out:'y',preds:['x','z'],modelTerms:[],quadratic:[],
        comparisonTerms:[],comparisonQuadratic:[],compare:false});
    equal(ui.modelTerms.value(),[]);equal(ui.comparisonTerms.value(),[]);
    assert.equal(ui.polynomials.value(),false);save('empty');
    // First explicit off during migration wins over the old square selection.
    view.flags.updating = true;
    native.modelTerms.setValue(additive);native.quadratic.setValue(['x']);
    native.interactionSpecVersion.setValue(1);native.polynomials.setValue(true);
    view.flags.updating = false;ui.polynomials.setValue(false);
    equal(ui.quadratic.value(),[]);equal(ui.modelTerms.value(),additive);save('explicit-migration-off');
    console.log('PASS: GLM polynomial migration, independent switches, square-only models and native saved restoration.');
    return records;
};
