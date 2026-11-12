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
            }))].filter(name => predictors.includes(name));
            ui[layout.supplier].setValue(utils.valuesToItems(available, FormatDef.variable));
            for (const name of layout.selections) {
                const value = ui[name].value();
                if (value == null) continue;
                const filtered = Array.isArray(value) ? value.filter(item => available.includes(item)) :
                    (available.includes(value) ? value : null);
                if (JSON.stringify(value) !== JSON.stringify(filtered)) ui[name].setValue(filtered);
            }
        }
    } finally {
        ui.view.model.options.endEdit();
    }
    context.workspace.glmPredictors = predictors.slice();
};

module.exports = updateTerms;
