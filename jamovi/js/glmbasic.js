'use strict';

const updateTerms = require('./model-terms');
const layouts = [{"supplier": "plotSupplier", "models": ["modelTerms"], "selections": ["plotX", "plotPanels", "plotGroup"], "extra": "quadratic"}, {"supplier": "comparisonPlotSupplier", "models": ["modelTerms", "comparisonTerms"], "selections": ["comparisonX", "comparisonPanels", "comparisonGroup"], "extra": ["quadratic", "comparisonQuadratic"]}];
const synchronize = (ui, context, added, changed = null) => {
    // Restore all host options before pruning dependent selections.
    if (context.flags && (context.flags.updating || context.flags.loaded === false)) return;
    if (context.workspace.lmSynchronizing) return;
    if (!added && !(ui.preds.value() || []).length) return;
    context.workspace.lmSynchronizing = true;
    ui.view.model.options.beginEdit();
    try {
        updateTerms.prepareInteractions(ui, changed);
        const predictors = ui.preds.value() || [];
        for (const [enabled, quadratic, termsName, supplier] of [
            ['polynomials','quadratic','modelTerms','quadraticSupplier'],
            ['comparisonPolynomials','comparisonQuadratic','comparisonTerms','comparisonQuadraticSupplier']]) {
            const selected = ui[enabled].value() === true ?
                (ui[quadratic].value() || []).filter(name => predictors.includes(name)) : [];
            if (JSON.stringify(ui[quadratic].value()) !== JSON.stringify(selected)) ui[quadratic].setValue(selected);
            ui[supplier].setValue(utils.valuesToItems(predictors, FormatDef.variable));
            if (changed === quadratic) {
                const terms = ui[termsName].value() || [];
                const missing = selected.filter(name => !terms.some(term => term.length === 1 && term[0] === name));
                if (missing.length) ui[termsName].setValue(terms.concat(missing.map(name => [name])));
            }
        }
        updateTerms(ui, context, added, ['modelTerms','comparisonTerms'], ['primarySupplier','comparisonSupplier'], layouts);
        updateTerms.syncMainEffects(ui);
    } finally {
        try { ui.view.model.options.endEdit(); }
        finally { context.workspace.lmSynchronizing = false; }
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
    polynomials_changed: function(ui) { synchronize(ui, this, false); },
    quadratic_changed: function(ui) { synchronize(ui, this, false, 'quadratic'); },
    quadraticSupplier_updated: function(ui) { synchronize(ui, this, false); },
    comparisonPolynomials_changed: function(ui) { synchronize(ui, this, false); },
    comparisonQuadratic_changed: function(ui) { synchronize(ui, this, false, 'comparisonQuadratic'); },
    comparisonQuadraticSupplier_updated: function(ui) { synchronize(ui, this, false); },
    preds_changed: function(ui) { synchronize(ui, this, true); },
    modelTerms_changed: function(ui) { synchronize(ui, this, false); },
    primarySupplier_updated: function(ui) { synchronize(ui, this, false); },
    plotSupplier_updated: function(ui) { synchronize(ui, this, false); },
    comparisonTerms_changed: function(ui) { synchronize(ui, this, false); },
    comparisonSupplier_updated: function(ui) { synchronize(ui, this, false); },
    comparisonPlotSupplier_updated: function(ui) { synchronize(ui, this, false); },
    plotX_changed: function(ui) { synchronize(ui, this, false); },
    plotPanels_changed: function(ui) { synchronize(ui, this, false); },
    plotGroup_changed: function(ui) { synchronize(ui, this, false); },
    comparisonX_changed: function(ui) { synchronize(ui, this, false); },
    comparisonPanels_changed: function(ui) { synchronize(ui, this, false); },
    comparisonGroup_changed: function(ui) { synchronize(ui, this, false); }
};
