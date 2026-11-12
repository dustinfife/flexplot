'use strict';

const updateTerms = require('./model-terms');
const layouts = [{"supplier": "plotSupplier", "models": ["modelTerms"], "selections": ["plotX", "plotPanels", "plotGroup"]}];
const synchronize = (ui, context, added) => updateTerms(ui, context, added, ["modelTerms"], ["primarySupplier"], layouts);

module.exports = {
    view_loaded: function(ui) { synchronize(ui, this, false); },
    view_updated: function(ui) { synchronize(ui, this, false); },
    preds_changed: function(ui) { synchronize(ui, this, true); },
    modelTerms_changed: function(ui) { synchronize(ui, this, false); },
    primarySupplier_updated: function(ui) { synchronize(ui, this, false); },
    plotSupplier_updated: function(ui) { synchronize(ui, this, false); },
    plotX_changed: function(ui) { synchronize(ui, this, false); },
    plotPanels_changed: function(ui) { synchronize(ui, this, false); },
    plotGroup_changed: function(ui) { synchronize(ui, this, false); }
};
