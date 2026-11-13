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
            const available = [...new Set(layout.models.flatMap(name => {
                const terms = ui[name].value();
                return terms == null ? predictors : terms.flat();
            }).concat(layout.extra ? (ui[layout.extra].value() || []) : []))].filter(name => predictors.includes(name));
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

module.exports = updateTerms;
