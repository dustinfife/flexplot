'use strict';

const updateTerms = require('./model-terms');
const updateGhostControls = require('./glm-ghosts');
const layouts = [{supplier:'plotSupplier', models:['modelTerms'],
    selections:['plotX', 'plotPanels', 'plotGroup'], exclusive:true, requireX:true, extra:'quadratic'},
    updateTerms.comparisonLayout('comparisonPlotSupplier',['quadratic','comparisonQuadratic'])];
const synchronize = function(ui, context, added, changed = null) {
    // Host options arrive separately during restoration; do not prune partial state.
    if (context.flags && (context.flags.updating || context.flags.loaded === false)) return;
    if (context.workspace.glmSynchronizing) return;
    if (!added && !(ui.preds.value() || []).length) {
        updatePlotControls(ui, context);
        return;
    }
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
            if (ui[enabled].value() !== true && (ui[quadraticName].value() || []).length) ui[quadraticName].setValue([]);
            updateTerms.syncQuadratic(ui, predictors, quadraticName, modelName, supplier, changed === quadraticName);
        }
        updateTerms(ui, context, added, ['modelTerms','comparisonTerms'], ['primarySupplier','comparisonSupplier'], layouts);
        // Keep the old option as a saved-state projection, without a duplicate UI.
        const main = (ui.modelTerms.value() || []).filter(term => term.length === 1).map(term => term[0]);
        if (JSON.stringify(options.getOption('mainEffects').getValue()) !== JSON.stringify(main))
            options.setOptionValue('mainEffects', main);
        const second = (ui.comparisonTerms.value() || []).filter(term => term.length === 1).map(term => term[0]);
        if (JSON.stringify(ui.comparisonMainEffects.value()) !== JSON.stringify(second)) ui.comparisonMainEffects.setValue(second);
        ui.comparisonMainSupplier.setValue(utils.valuesToItems(predictors.filter(name => !second.includes(name)), FormatDef.variable));
    } finally {
        try { ui.view.model.options.endEdit(); }
        finally { context.workspace.glmSynchronizing = false; }
    }
    updatePlotControls(ui, context);
};
// Use the host's column metadata, not the spelling/storage type of a variable.
// A response for a previous selection must never change the current controls.
const updatePlotControls = function(ui, context) {
    if (context.flags && (context.flags.updating || context.flags.loaded === false)) return;
    updateGhostControls(ui, context);
    if (typeof context.requestData !== 'function') return;
    const token = {};
    context.workspace.glmPlotRequest = token;
    const names = [ui.plotX.value()];
    if (ui.compare.value()) names.push(ui.comparisonX.value());
    const binomial = ui.family.value() === 'binomial';
    ui.plotStyle.setPropertyValue('enable', false);
    ui.plotScale.setPropertyValue('enable', false);
    if (!binomial) return;
    return Promise.all(names.map(columnName => columnName ? context.requestData('column',
        {columnName, properties:['measureType','dataType']}) : Promise.resolve(null))).then(columns => {
        if (token !== context.workspace.glmPlotRequest) return;
        const continuous = columns.every(column => column && column.columnFound && column.measureType === 'continuous' &&
            ['decimal','integer'].includes(column.dataType));
        ui.plotStyle.setPropertyValue('enable', continuous);
        const logit = continuous && ui.plotStyle.value() === 'auto';
        ui.plotScale.setPropertyValue('enable', logit);
        if (!logit && ui.plotScale.value() !== 'probability') ui.plotScale.setValue('probability');
    }).catch(() => {
        // Leave controls disabled if metadata is unavailable; R independently
        // validates the actual fitted data and safely resolves legacy options.
        if (token === context.workspace.glmPlotRequest && ui.plotScale.value() !== 'probability')
            ui.plotScale.setValue('probability');
    });
};
module.exports = {
    interactions_changed: function(ui) { synchronize(ui, this, false, 'interactions'); },
    comparisonInteractions_changed: function(ui) { synchronize(ui, this, false, 'comparisonInteractions'); },
    comparisonMainEffects_changed: function(ui) { synchronize(ui, this, false, 'comparisonMainEffects'); },
    comparisonMainSupplier_updated: function(ui) { synchronize(ui, this, false); },
    out_changed: function(ui) { updatePlotControls(ui, this); },
    ghostLines_changed: function(ui) { updatePlotControls(ui, this); },
    graphic_changed: function(ui) { updatePlotControls(ui, this); },
    family_changed: function(ui) { updatePlotControls(ui, this); },
    plotStyle_changed: function(ui) { updatePlotControls(ui, this); },
    plotScale_changed: function(ui) { updatePlotControls(ui, this); },
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
