'use strict';
module.exports=function({ui,restore,snapshot,assert}) {
    const records=[];
    const equal=(actual,expected)=>assert.equal(JSON.stringify(actual),JSON.stringify(expected));
    const save=stage=>records.push(snapshot(stage));
    restore('lm-main-fresh',{out:'y',preds:['x']});
    equal(ui.modelTerms.value(),[['x']]);save('fresh');
    ui.preds.setValue(['x','z']);equal(ui.modelTerms.value(),[['x'],['z']]);save('add');
    ui.preds.setValue(['z']);equal(ui.modelTerms.value(),[['z']]);save('remove');
    ui.preds.setValue(['x','z']);
    ui.compare.setValue(true);ui.comparisonX.setValue('z');
    ui.comparisonInteractions.setValue(true);
    ui.comparisonTerms.setValue([['x'],['z'],['x','z']]);
    ui.comparisonMainEffects.setValue([]);
    equal(ui.comparisonTerms.value(),[['x','z']]);
    equal(ui.modelTerms.value(),[['z'],['x']]);save('second-interaction-only');
    ui.comparisonInteractions.setValue(false);ui.comparisonPolynomials.setValue(true);
    ui.comparisonQuadratic.setValue(['x']);
    equal(ui.comparisonTerms.value(),[['x']],'Quadratic selection retains the existing default linear term');
    ui.comparisonMainEffects.setValue([]);
    equal(ui.comparisonTerms.value(),[]);equal(ui.comparisonQuadratic.value(),['x']);save('second-square-only');
    ui.compare.setValue(false);ui.compare.setValue(true);
    equal(ui.comparisonTerms.value(),[]);equal(ui.modelTerms.value(),[['z'],['x']]);save('comparison-toggle');
    for(const [stage,terms,quadratic,interactions] of [
        ['legacy-subset',[['z']],[],false],['legacy-interaction',[['x','z']],[],true],
        ['legacy-square',[],['x'],false],['legacy-empty',[],[],false]]) {
        restore(stage,{out:'y',preds:['x','z'],modelTerms:terms,quadratic,
            polynomials:quadratic.length>0,interactions,compare:true,comparisonTerms:[['z']],comparisonX:'z',
            mainEffects:['x','z']}); // Stale projection must not replace explicit terms.
        equal(ui.modelTerms.value(),terms);save(stage);
        const saved=snapshot('to-reopen').options;
        restore(stage+'-reopened',JSON.parse(JSON.stringify(saved)));
        equal(ui.modelTerms.value(),terms);save(stage+'-reopened');
        ui.compare.setValue(false);ui.compare.setValue(true);
        equal(ui.modelTerms.value(),terms);
        ui.preds.setValue(['x','z','new predictor']);
        equal(ui.modelTerms.value(),terms.length ? terms.concat([['new predictor']]) : []);
        ui.preds.setValue(['x','z']);equal(ui.modelTerms.value(),terms);
        equal(ui.comparisonTerms.value(),[['z']]);save(stage+'-edited');
    }
    console.log('PASS: simplified LM main effects, independent nonhierarchical Model 2 and legacy restoration.');
    return records;
};
