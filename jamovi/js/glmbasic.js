'use strict';

const updateTerms = require('./model-terms');
const migrateOutput = require('./output-options');
const updateEditors = require('./plot-label-editors');
const layouts = [{supplier:'plotSupplier', models:['modelTerms'],
    selections:['plotX','plotPanels','plotGroup'], extra:'quadratic'},
    updateTerms.comparisonLayout('comparisonPlotSupplier',['quadratic','comparisonQuadratic'],false),
    {supplier:'comparisonGroupingLayout',models:['modelTerms','comparisonTerms'],selections:[],
        extra:['quadratic','comparisonQuadratic']}];
// Native `changing` events can reject a transfer before it changes an option.
// Restored options bypass this guard and remain subject to R's layout validation.
const plotRoleChanging = (ui, context, name, event) => {
    if (context.workspace.lmSynchronizing || (context.flags &&
        (context.flags.updating || context.flags.loaded === false))) return;
    let value = event.value;
    if ((event.key || []).length) {
        value = (ui[name].value() || []).slice();
        if (event.insert) value.splice(event.key[0], 0, event.value);
        else value[event.key[0]] = event.value;
    }
    const items = value => [].concat(value || []).filter(Boolean);
    if (!items(value).some(item => !items(ui[name].value()).includes(item))) return;
    const roles = Object.fromEntries(['plotX','plotPanels','plotGroup'].map(role =>
        [role, items(role === name ? value : ui[role].value())]));
    const terms = ui.modelTerms.value();
    const available = [...new Set((terms == null ? ui.preds.value() || [] : terms.flat())
        .concat(ui.quadratic.value() || []))];
    const selected = Object.values(roles).flat();
    if (selected.some(item => !available.includes(item)) ||
        new Set(selected).size !== selected.length || roles.plotX.length > 1 ||
        roles.plotPanels.length > 2 || roles.plotGroup.length > 1 ||
        (!roles.plotX.length && !available.some(item => !selected.includes(item)))) event.cancel = true;
};
const synchronize = (ui, context, added, changed = null) => {
    // Restore all host options before pruning dependent selections.
    if (context.flags && (context.flags.updating || context.flags.loaded === false)) return;
    migrateOutput(ui, context, 'glmbasic');
    updateEditors(ui, context, 'glmbasic');
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
            if (ui[enabled].value() !== true && (ui[quadratic].value() || []).length) ui[quadratic].setValue([]);
            updateTerms.syncQuadratic(ui, predictors, quadratic, termsName, supplier, changed === quadratic);
        }
        const empty = added && (context.workspace.glmPredictors || []).length ?
            ['modelTerms','comparisonTerms'].filter(name => ui[name].value() != null && !ui[name].value().length) : [];
        updateTerms(ui, context, added, ['modelTerms','comparisonTerms'], ['primarySupplier','comparisonSupplier'], layouts);
        if (empty.length) {
            for (const name of empty) ui[name].setValue([]);
            updateTerms(ui, context, false, ['modelTerms','comparisonTerms'], ['primarySupplier','comparisonSupplier'], layouts);
        }
        // Legacy Model 1 option remains a projection, without a duplicate control.
        const main = (ui.modelTerms.value() || []).filter(term => term.length === 1).map(term => term[0]);
        const options = ui.view.model.options;
        if (JSON.stringify(options.getOption('mainEffects').getValue()) !== JSON.stringify(main))
            options.setOptionValue('mainEffects', main);
        const second = (ui.comparisonTerms.value() || []).filter(term => term.length === 1).map(term => term[0]);
        if (JSON.stringify(ui.comparisonMainEffects.value()) !== JSON.stringify(second)) ui.comparisonMainEffects.setValue(second);
        ui.comparisonMainSupplier.setValue(utils.valuesToItems(predictors.filter(name => !second.includes(name)), FormatDef.variable));
        updateTerms.invalidateComparisonMagnet(ui, context);
    } finally {
        try { ui.view.model.options.endEdit(); }
        finally { context.workspace.lmSynchronizing = false; }
    }
};

module.exports = {
    comparisonGroupingLayout_updated: function(ui) { synchronize(ui, this, false); },
    out_changed: function(ui) { synchronize(ui, this, false); },
    comparisonGroup_changing: function(ui, event) {
        updateTerms.groupChanging(ui.compare.value() === true &&
            !!ui.view.model.options.getOption('comparisonMagnet') &&
            ui.view.model.options.getOption('comparisonMagnet').getValue() === true, event);
    },
    plotX_changing: function(ui, event) { plotRoleChanging(ui, this, 'plotX', event); },
    plotPanels_changing: function(ui, event) { plotRoleChanging(ui, this, 'plotPanels', event); },
    plotGroup_changing: function(ui, event) { plotRoleChanging(ui, this, 'plotGroup', event); },
    interactions_changed: function(ui) { synchronize(ui, this, false, 'interactions'); },
    comparisonInteractions_changed: function(ui) { synchronize(ui, this, false, 'comparisonInteractions'); },
    comparisonMainEffects_changed: function(ui) { synchronize(ui, this, false, 'comparisonMainEffects'); },
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
