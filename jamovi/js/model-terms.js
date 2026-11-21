'use strict';

// Each model has its own term selection; a deliberately empty list means intercept only.
const updateTerms = function(ui, context, addNew, models, suppliers, layouts = []) {
    const predictors = ui.preds.value() || [];
    const previous = context.workspace.glmPredictors || (addNew ? [] : predictors);
    // Automatic handlers receive helpers through utils, not the View instance.
    const items = utils.valuesToItems(predictors.map(name => [name]), FormatDef.term);
    for (const supplier of suppliers) ui[supplier].setValue(items);
    ui.view.model.options.beginEdit();
    try {
        for (const control of models.map(name => ui[name])) {
            const current = control.value();
            let terms = current == null ? predictors.map(name => [name]) :
                current.filter(term => term.every(name => predictors.includes(name)));
            if (current != null && addNew) {
                const added = predictors.filter(name => !previous.includes(name));
                terms = terms.concat(added.map(name => [name]));
            }
            if (JSON.stringify(current) !== JSON.stringify(terms))
                control.setValue(terms);
        }
        for (const layout of layouts) {
            const permittedVariables = layout.include ? predictors.concat(ui[layout.include].value() || []) : predictors;
            const available = [...new Set(layout.models.flatMap(name => {
                const terms = ui[name].value();
                return terms == null ? predictors : terms.flat();
            }).concat((layout.extra ? [].concat(layout.extra) : []).flatMap(name => ui[name].value() || [])))].filter(name =>
                permittedVariables.includes(name) && !(layout.exclude || []).includes(name));
            ui[layout.supplier].setValue(utils.valuesToItems(available, FormatDef.variable));
            const assigned = new Set();
            for (const name of layout.selections) {
                const value = ui[name].value();
                if (value == null) continue;
                const permitted = name => available.includes(name) && (!layout.exclusive || !assigned.has(name));
                const filtered = Array.isArray(value) ? value.filter(item => permitted(item)) :
                    (permitted(value) ? value : null);
                for (const item of (Array.isArray(filtered) ? filtered : [filtered])) if (item) assigned.add(item);
                if (JSON.stringify(value) !== JSON.stringify(filtered)) ui[name].setValue(filtered);
            }
            if (layout.requireX && !ui[layout.selections[0]].value()) {
                const x = available.find(name => !assigned.has(name)) || available[0];
                if (x) {
                    ui[layout.selections[0]].setValue(x);
                    for (const name of layout.selections.slice(1)) {
                        const value = ui[name].value();
                        const filtered = Array.isArray(value) ? value.filter(item => item !== x) : (value === x ? null : value);
                        if (JSON.stringify(value) !== JSON.stringify(filtered)) ui[name].setValue(filtered);
                    }
                }
            }
        }
    } finally {
        ui.view.model.options.endEdit();
    }
    context.workspace.glmPredictors = predictors.slice();
};

// Migration runs only after the host's complete option batch has arrived.
// modelTerms remains authoritative; the main-effect controls are editable projections.
updateTerms.prepareInteractions = function(ui, changed) {
    const options = ui.view.model.options;
    const models = [['modelTerms','interactions','mainEffects'],
        ['comparisonTerms','comparisonInteractions','comparisonMainEffects']];
    if (options.getOption('interactionSpecVersion').getValue() < 1) {
        for (const [termsName, enabled] of models) {
            if (changed === enabled) continue; // Respect an explicit user toggle.
            const interacting = (ui[termsName].value() || []).some(term => term.length > 1);
            if (interacting && ui[enabled].value() !== true) ui[enabled].setValue(true);
        }
        options.setOptionValue('interactionSpecVersion',1);
    }
    for (const [termsName, enabled, mainName] of models) {
        let terms = ui[termsName].value();
        if (changed === mainName) {
            const selected = ui[mainName].value() || [];
            const current = terms == null ? (ui.preds.value() || []).map(name => [name]) : terms;
            terms = current.filter(term => term.length > 1 || selected.includes(term[0]));
            terms = terms.concat(selected.filter(name => !terms.some(term =>
                term.length === 1 && term[0] === name)).map(name => [name]));
        }
        if (terms != null && ui[enabled].value() === false) terms = terms.filter(term => term.length === 1);
        if (JSON.stringify(terms) !== JSON.stringify(ui[termsName].value())) ui[termsName].setValue(terms);
    }
};

updateTerms.syncMainEffects = function(ui) {
    const predictors = ui.preds.value() || [];
    for (const [termsName, mainName, supplier] of [
        ['modelTerms','mainEffects','primaryMainSupplier'],
        ['comparisonTerms','comparisonMainEffects','comparisonMainSupplier']]) {
        const terms = ui[termsName].value();
        const selected = terms == null ? predictors : terms.filter(term => term.length === 1).map(term => term[0]);
        if (JSON.stringify(ui[mainName].value()) !== JSON.stringify(selected)) ui[mainName].setValue(selected);
        ui[supplier].setValue(utils.valuesToItems(predictors.filter(name => !selected.includes(name)), FormatDef.variable));
    }
};

module.exports = updateTerms;
