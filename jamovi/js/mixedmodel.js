'use strict';

const updateTerms = require('./model-terms');
const layouts = [{supplier:'plotSupplier', models:['modelTerms'],
    selections:['plotX','plotPanels','plotGroup'], exclusive:true, requireX:true,
    extra:['quadratic','randomSlopes','cluster'], include:'cluster'}];
const synchronize = function(ui, context, added, quadraticChanged = false) {
    // Native setters synchronously invoke other handlers while an option edit is
    // still in progress. Synchronize once, and send the complete option batch.
    if (context.flags && context.flags.updating) return;
    if (context.workspace.mixedSynchronizing) return;
    context.workspace.mixedSynchronizing = true;
    const options = ui.view.model.options;
    options.beginEdit();
    try {
        const predictors = ui.preds.value() || [];
        for (const name of ['quadratic','randomSlopes']) {
            const value = (ui[name].value() || []).filter(variable => predictors.includes(variable));
            if (JSON.stringify(ui[name].value()) !== JSON.stringify(value)) ui[name].setValue(value);
        }
        ui.quadraticSupplier.setValue(utils.valuesToItems(predictors, FormatDef.variable));
        ui.randomSupplier.setValue(utils.valuesToItems(predictors, FormatDef.variable));
        if (quadraticChanged) {
            const terms = ui.modelTerms.value() || [];
            const missing = (ui.quadratic.value() || []).filter(name => !terms.some(term => term.length === 1 && term[0] === name));
            if (missing.length) ui.modelTerms.setValue(terms.concat(missing.map(name => [name])));
        }
        // A new grouping selection defaults to panels; do not overwrite a saved layout.
        const cluster = ui.cluster.value();
        if (cluster !== context.workspace.mixedCluster) {
            const previous = context.workspace.mixedCluster;
            if (previous && ui.plotX.value() === previous) ui.plotX.setValue(null);
            if (previous && ui.plotGroup.value() === previous) ui.plotGroup.setValue(null);
            const panels = (ui.plotPanels.value() || []).filter(name => name !== previous);
            const restored = context.workspace.mixedCluster === undefined &&
                [ui.plotX.value(), ui.plotGroup.value(), ...panels].includes(cluster);
            if (cluster && !restored && panels.length < 2 && ![ui.plotX.value(),ui.plotGroup.value(),...panels].includes(cluster)) panels.push(cluster);
            context.workspace.mixedCluster = cluster;
            if (JSON.stringify(ui.plotPanels.value()) !== JSON.stringify(panels)) ui.plotPanels.setValue(panels);
        }
        if (cluster && ui.plotX.value() === cluster) ui.plotX.setValue(null);
        updateTerms(ui, context, added, ['modelTerms'], ['primarySupplier'], layouts);
        if (cluster && ui.plotX.value() === cluster) {
            ui.plotX.setValue(null);
            const panels = ui.plotPanels.value() || [];
            if (panels.length < 2 && !panels.includes(cluster) && ui.plotGroup.value() !== cluster)
                ui.plotPanels.setValue(panels.concat(cluster));
        }
    } finally {
        try { options.endEdit(); }
        finally { context.workspace.mixedSynchronizing = false; }
    }
};
module.exports = {
    // loaded precedes host option initialization; ready/view_updated is the
    // first safe point for defaults or synchronization of a saved analysis.
    view_loaded: function() {},
    view_updated: function(ui) { synchronize(ui,this,false); },
    preds_changed: function(ui) { synchronize(ui,this,true); },
    modelTerms_changed: function(ui) { synchronize(ui,this,false); },
    primarySupplier_updated: function(ui) { synchronize(ui,this,false); },
    quadratic_changed: function(ui) { synchronize(ui,this,false,true); },
    quadraticSupplier_updated: function(ui) { synchronize(ui,this,false); },
    randomSlopes_changed: function(ui) { synchronize(ui,this,false); },
    randomSupplier_updated: function(ui) { synchronize(ui,this,false); },
    cluster_changed: function(ui) { synchronize(ui,this,false); },
    plotSupplier_updated: function(ui) { synchronize(ui,this,false); },
    plotX_changed: function(ui) { synchronize(ui,this,false); },
    plotPanels_changed: function(ui) { synchronize(ui,this,false); },
    plotGroup_changed: function(ui) { synchronize(ui,this,false); }
};
