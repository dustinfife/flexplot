'use strict';

const updateTerms = require('./model-terms');
const layouts = [{supplier:'plotSupplier', models:['modelTerms'],
    selections:['plotX', 'plotPanels', 'plotGroup'], exclusive:true, requireX:true, extra:'quadratic'},
    {supplier:'comparisonPlotSupplier', models:['modelTerms','comparisonTerms'],
    selections:['comparisonX','comparisonPanels'], exclusive:true, requireX:true,
    extra:['quadratic','comparisonQuadratic']}];
const synchronize = function(ui, context, added, changed = null) {
    // Host options arrive separately during restoration; do not prune partial state.
    if (context.flags && (context.flags.updating || context.flags.loaded === false)) return;
    if (context.workspace.glmSynchronizing) return;
    if (!added && !(ui.preds.value() || []).length) return;
    context.workspace.glmSynchronizing = true;
    ui.view.model.options.beginEdit();
    try {
        updateTerms.prepareInteractions(ui, changed);
        // Version 2 adds polynomial switches; restore legacy squares before pruning.
        const options = ui.view.model.options;
        if (options.getOption('interactionSpecVersion').getValue() < 2) {
            for (const [enabled, quadratic] of [
                ['polynomials','quadratic'], ['comparisonPolynomials','comparisonQuadratic']]) {
                if (changed !== enabled && (ui[quadratic].value() || []).length)
                    ui[enabled].setValue(true);
            }
            options.setOptionValue('interactionSpecVersion',2);
        }
        const predictors = ui.preds.value() || [];
        for (const [enabled, quadraticName, modelName, supplier] of [
            ['polynomials','quadratic','modelTerms','quadraticSupplier'],
            ['comparisonPolynomials','comparisonQuadratic','comparisonTerms','comparisonQuadraticSupplier']]) {
            const quadratic = ui[enabled].value() === true ?
                (ui[quadraticName].value() || []).filter(name => predictors.includes(name)) : [];
            if (JSON.stringify(ui[quadraticName].value()) !== JSON.stringify(quadratic)) ui[quadraticName].setValue(quadratic);
            ui[supplier].setValue(utils.valuesToItems(predictors, FormatDef.variable));
            if (changed === quadraticName) {
                const terms = ui[modelName].value() || [];
                const missing = quadratic.filter(name => !terms.some(term => term.length === 1 && term[0] === name));
                if (missing.length) ui[modelName].setValue(terms.concat(missing.map(name => [name])));
            }
        }
        updateTerms(ui, context, added, ['modelTerms','comparisonTerms'], ['primarySupplier','comparisonSupplier'], layouts);
        updateTerms.syncMainEffects(ui);
    } finally {
        try { ui.view.model.options.endEdit(); }
        finally { context.workspace.glmSynchronizing = false; }
    }
};
module.exports = {
    interactions_changed: function(ui) { synchronize(ui, this, false, 'interactions'); },
    comparisonInteractions_changed: function(ui) { synchronize(ui, this, false, 'comparisonInteractions'); },
    mainEffects_changed: function(ui) { synchronize(ui, this, false, 'mainEffects'); },
    comparisonMainEffects_changed: function(ui) { synchronize(ui, this, false, 'comparisonMainEffects'); },
    primaryMainSupplier_updated: function(ui) { synchronize(ui, this, false); },
    comparisonMainSupplier_updated: function(ui) { synchronize(ui, this, false); },
    compare_changed: function(ui) { synchronize(ui, this, false); },
    view_loaded: function(ui) { synchronize(ui, this, false); },
    view_updated: function(ui) { synchronize(ui, this, false); },
    preds_changed: function(ui) { synchronize(ui, this, true); },
    modelTerms_changed: function(ui) { synchronize(ui, this, false); },
    polynomials_changed: function(ui) { synchronize(ui, this, false, 'polynomials'); },
    comparisonPolynomials_changed: function(ui) { synchronize(ui, this, false, 'comparisonPolynomials'); },
    quadratic_changed: function(ui) { synchronize(ui, this, false, 'quadratic'); },
    quadraticSupplier_updated: function(ui) { synchronize(ui, this, false); },
    primarySupplier_updated: function(ui) { synchronize(ui, this, false); },
    plotSupplier_updated: function(ui) { synchronize(ui, this, false); },
    plotX_changed: function(ui) { synchronize(ui, this, false); },
    plotPanels_changed: function(ui) { synchronize(ui, this, false); },
    comparisonTerms_changed: function(ui) { synchronize(ui, this, false); },
    comparisonQuadratic_changed: function(ui) { synchronize(ui, this, false, 'comparisonQuadratic'); },
    comparisonQuadraticSupplier_updated: function(ui) { synchronize(ui, this, false); },
    comparisonSupplier_updated: function(ui) { synchronize(ui, this, false); },
    comparisonPlotSupplier_updated: function(ui) { synchronize(ui, this, false); },
    comparisonX_changed: function(ui) { synchronize(ui, this, false); },
    comparisonPanels_changed: function(ui) { synchronize(ui, this, false); },
    plotGroup_changed: function(ui) { synchronize(ui, this, false); }
};
