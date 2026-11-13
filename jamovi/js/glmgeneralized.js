'use strict';

const updateTerms = require('./model-terms');
const layouts = [{supplier:'plotSupplier', models:['modelTerms'],
    selections:['plotX', 'plotPanels', 'plotGroup'], exclusive:true, requireX:true, extra:'quadratic'}];
const synchronize = function(ui, context, added, quadraticChanged = false) {
    const predictors = ui.preds.value() || [];
    const quadratic = (ui.quadratic.value() || []).filter(name => predictors.includes(name));
    if (JSON.stringify(ui.quadratic.value()) !== JSON.stringify(quadratic)) ui.quadratic.setValue(quadratic);
    ui.quadraticSupplier.setValue(utils.valuesToItems(predictors, FormatDef.variable));
    if (quadraticChanged) {
        const terms = ui.modelTerms.value() || [];
        const missing = quadratic.filter(name => !terms.some(term => term.length === 1 && term[0] === name));
        if (missing.length) ui.modelTerms.setValue(terms.concat(missing.map(name => [name])));
    }
    updateTerms(ui, context, added, ['modelTerms'], ['primarySupplier'], layouts);
};
module.exports = {
    view_loaded: function(ui) { synchronize(ui, this, false); },
    view_updated: function(ui) { synchronize(ui, this, false); },
    preds_changed: function(ui) { synchronize(ui, this, true); },
    modelTerms_changed: function(ui) { synchronize(ui, this, false); },
    quadratic_changed: function(ui) { synchronize(ui, this, false, true); },
    quadraticSupplier_updated: function(ui) { synchronize(ui, this, false); },
    primarySupplier_updated: function(ui) { synchronize(ui, this, false); },
    plotSupplier_updated: function(ui) { synchronize(ui, this, false); },
    plotX_changed: function(ui) { synchronize(ui, this, false); },
    plotPanels_changed: function(ui) { synchronize(ui, this, false); },
    plotGroup_changed: function(ui) { synchronize(ui, this, false); }
};
