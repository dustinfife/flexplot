'use strict';

// Each model has its own term selection; a deliberately empty list means intercept only.
const updateTerms = function(ui, context, addNew) {
    const predictors = ui.preds.value() || [];
    const previous = context.workspace.glmPredictors || (addNew ? [] : predictors);
    // Automatic handlers receive helpers through utils, not the View instance.
    const items = utils.valuesToItems(predictors.map(name => [name]), FormatDef.term);
    ui.primarySupplier.setValue(items);
    ui.comparisonSupplier.setValue(items);
    ui.view.model.options.beginEdit();
    try {
        for (const control of [ui.modelTerms, ui.comparisonTerms]) {
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
    } finally {
        ui.view.model.options.endEdit();
    }
    context.workspace.glmPredictors = predictors.slice();
};

module.exports = {
    view_loaded: function(ui) { updateTerms(ui, this, false); },
    view_updated: function(ui) { updateTerms(ui, this, false); },
    preds_changed: function(ui) { updateTerms(ui, this, true); },
    primarySupplier_updated: function(ui) { updateTerms(ui, this, false); },
    comparisonSupplier_updated: function(ui) { updateTerms(ui, this, false); }
};
