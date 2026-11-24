'use strict';

const updateTerms = require('./model-terms');
const migrateOutput = require('./output-options');
const updateEditors = require('./plot-label-editors');
const updateGhostControls = require('./model-ghosts');
const synchronize = function(ui, context, added, quadraticChanged = null) {
    // Native setters synchronously invoke other handlers while an option edit is
    // still in progress. Synchronize once, and send the complete option batch.
    if (context.flags && context.flags.updating) return;
    if (context.workspace.mixedSynchronizing) return;
    context.workspace.mixedSynchronizing = true;
    const options = ui.view.model.options;
    options.beginEdit();
    try {
        // Preserve the complete legacy layout before synchronous setters can
        // normalize the newly independent main controls.
        migrateOutput(ui, context, 'mixedmodel');
        updateEditors(ui, context, 'mixedmodel');
        const predictors = ui.preds.value() || [];
        const comparing = ui.compare.value() === true;
        // Old saved results may lack Model 2 tables. Request one
        // recalculation when their controls initialize, preserving model options.
        if (comparing && options.getOption('samplePolicyVersion').getValue() !== 2)
            options.setOptionValue('samplePolicyVersion', 2);
        let secondPredictors = ui.comparisonFixed.value();
        if (secondPredictors == null && (comparing || ui.comparisonTerms.value() != null)) {
            // Old saved analyses already contain explicit terms. Preserve them;
            // otherwise initialize from Model 1, excluding random-slope-only variables.
            const terms = ui.comparisonTerms.value() == null ? ui.modelTerms.value() : ui.comparisonTerms.value();
            const quadratic = ui.comparisonTerms.value() == null ? ui.quadratic.value() : ui.comparisonQuadratic.value();
            secondPredictors = [...new Set((terms == null ? predictors : terms.flat()).concat(quadratic || []))];
        }
        if (secondPredictors != null) {
            secondPredictors = secondPredictors.filter(name => predictors.includes(name));
            if (JSON.stringify(ui.comparisonFixed.value()) !== JSON.stringify(secondPredictors))
                ui.comparisonFixed.setValue(secondPredictors);
        }
        ui.comparisonFixedSupplier.setValue(utils.valuesToItems(
            predictors.filter(name => !(secondPredictors || []).includes(name)), FormatDef.variable));
        if (secondPredictors != null) {
            const secondContext = Object.assign({}, context, {workspace:context.workspace.mixedSecond || {}});
            context.workspace.mixedSecond = secondContext.workspace;
            const secondUI = Object.assign({}, ui, {preds:ui.comparisonFixed});
            updateTerms(secondUI, secondContext, quadraticChanged === 'comparisonFixed',
                ['comparisonTerms'], ['comparisonSupplier']);
        }

        for (const [termsName, interactionName, polynomialName, quadraticName, slopesName, supplier, randomSupplier] of [
            ['modelTerms','interactions','polynomials','quadratic','randomSlopes','quadraticSupplier','randomSupplier'],
            ['comparisonTerms','comparisonInteractions','comparisonPolynomials','comparisonQuadratic','comparisonRandomSlopes','comparisonQuadraticSupplier','comparisonRandomSupplier']]) {
            if (ui[interactionName].value() === false && ui[termsName].value() != null) {
                const main = ui[termsName].value().filter(term => term.length === 1);
                if (JSON.stringify(main) !== JSON.stringify(ui[termsName].value())) ui[termsName].setValue(main);
            }
            if (ui[polynomialName].value() === false && (ui[quadraticName].value() || []).length)
                ui[quadraticName].setValue([]);
            const modelPredictors = termsName === 'comparisonTerms' ? (secondPredictors || []) : predictors;
            updateTerms.syncQuadratic(ui, modelPredictors, quadraticName, termsName, supplier, quadraticChanged === quadraticName);
            const slopes = (ui[slopesName].value() || []).filter(variable => predictors.includes(variable));
            if (JSON.stringify(ui[slopesName].value()) !== JSON.stringify(slopes)) ui[slopesName].setValue(slopes);
            ui[randomSupplier].setValue(utils.valuesToItems(predictors, FormatDef.variable));
        }
        const cluster = ui.cluster.value();
        const layouts = [];
        for (const comparison of [false,true]) {
            const x = comparison ? 'comparisonX' : 'plotX';
            const panelsName = comparison ? 'comparisonPanels' : 'plotPanels';
            const group = comparison ? 'comparisonGroup' : 'plotGroup';
            const prediction = ui[comparison ? 'comparisonPredictionType' : 'predictionType'].value();
            const cas = prediction === 'cas';
            const ordinaryComparison = comparison && !cas;
            const layout = {supplier:comparison ? 'comparisonPlotSupplier' : 'plotSupplier',
                models:comparison ? ['modelTerms','comparisonTerms'] : ['modelTerms'],
                selections:ordinaryComparison ? [x,panelsName] : [x,panelsName,group], exclusive:true, requireX:true,
                reserved:ordinaryComparison ? [].concat(ui[group].value() || []) : [],
                extra:comparison ? ['quadratic','randomSlopes','comparisonQuadratic','comparisonRandomSlopes','cluster'] : ['quadratic','randomSlopes','cluster'],
                include:'cluster',exclude:cas && cluster ? [cluster] : []};
            layouts.push(layout,Object.assign({},layout,{supplier:comparison ? 'comparisonGroupingLayout' : 'plotGroupingLayout',selections:[],requireX:false}));
            const key = comparison ? 'mixedComparisonCluster' : 'mixedCluster';
            if (cluster !== context.workspace[key]) {
                const previous = context.workspace[key];
                if (previous && ui[x].value() === previous) ui[x].setValue(null);
                if (previous && ui[group].value() === previous) ui[group].setValue(null);
                const panels = (ui[panelsName].value() || []).filter(name => name !== previous);
                const restored = context.workspace[key] === undefined && (panels.length > 0 || !!ui[group].value());
                if (cluster && prediction === 'cluster' && !restored && panels.length < 2 && ![ui[x].value(),ui[group].value(),...panels].includes(cluster)) panels.push(cluster);
                context.workspace[key] = cluster;
                if (JSON.stringify(ui[panelsName].value()) !== JSON.stringify(panels)) ui[panelsName].setValue(panels);
            }
            if (cluster && ui[x].value() === cluster) ui[x].setValue(null);
        }
        updateTerms(ui, context, added, ['modelTerms'], ['primarySupplier'], layouts);
        for (const [x,panelsName,group] of [['plotX','plotPanels','plotGroup'],['comparisonX','comparisonPanels','comparisonGroup']]) {
            if (cluster && ui[x].value() === cluster) {
                ui[x].setValue(null);
                const panels = ui[panelsName].value() || [];
                if (panels.length < 2 && !panels.includes(cluster) && ui[group].value() !== cluster) ui[panelsName].setValue(panels.concat(cluster));
            }
        }
    } finally {
        try { options.endEdit(); }
        finally { context.workspace.mixedSynchronizing = false; }
    }
    updateGhostControls(ui, context, 'mixedmodel');
};
module.exports = {
    graphic_changed: function(ui) { updateGhostControls(ui, this, 'mixedmodel'); },
    ghostLines_changed: function(ui) { updateGhostControls(ui, this, 'mixedmodel'); },
    plotGroupingLayout_updated: function(ui) { synchronize(ui,this,false); },
    plotGroup_changing: function(ui, event) {
        updateTerms.groupChanging(true,event);
    },
    comparisonGroup_changing: function(ui,event) {
        updateTerms.groupChanging(ui.comparisonPredictionType.value() === 'cas',event);
    },
    comparisonPlotSupplier_updated: function(ui) { synchronize(ui,this,false); },
    comparisonGroupingLayout_updated: function(ui) { synchronize(ui,this,false); },
    comparisonX_changed: function(ui) { synchronize(ui,this,false); },
    comparisonPanels_changed: function(ui) { synchronize(ui,this,false); },
    comparisonGroup_changed: function(ui) { synchronize(ui,this,false); },
    comparisonPredictionType_changed: function(ui) { synchronize(ui,this,false); },
    // loaded precedes host option initialization; ready/view_updated is the
    // first safe point for defaults or synchronization of a saved analysis.
    view_loaded: function() {},
    view_updated: function(ui) { synchronize(ui,this,false); },
    preds_changed: function(ui) { synchronize(ui,this,true); },
    interactions_changed: function(ui) { synchronize(ui,this,false); },
    polynomials_changed: function(ui) { synchronize(ui,this,false); },
    modelTerms_changed: function(ui) { synchronize(ui,this,false); },
    primarySupplier_updated: function(ui) { synchronize(ui,this,false); },
    quadratic_changed: function(ui) { synchronize(ui,this,false,'quadratic'); },
    quadraticSupplier_updated: function(ui) { synchronize(ui,this,false); },
    randomSlopes_changed: function(ui) { synchronize(ui,this,false); },
    randomSupplier_updated: function(ui) { synchronize(ui,this,false); },
    compare_changed: function(ui) { synchronize(ui,this,false); },
    comparisonFixed_changed: function(ui) { synchronize(ui,this,false,'comparisonFixed'); },
    // Required by native Supplier; its display changes must not edit model options.
    comparisonFixedSupplier_updated: function() {},
    comparisonTerms_changed: function(ui) { synchronize(ui,this,false); },
    comparisonInteractions_changed: function(ui) { synchronize(ui,this,false); },
    comparisonPolynomials_changed: function(ui) { synchronize(ui,this,false); },
    comparisonQuadratic_changed: function(ui) { synchronize(ui,this,false,'comparisonQuadratic'); },
    comparisonRandomSlopes_changed: function(ui) { synchronize(ui,this,false); },
    comparisonSupplier_updated: function(ui) { synchronize(ui,this,false); },
    comparisonQuadraticSupplier_updated: function(ui) { synchronize(ui,this,false); },
    comparisonRandomSupplier_updated: function(ui) { synchronize(ui,this,false); },
    cluster_changed: function(ui) { synchronize(ui,this,false); },
    predictionType_changed: function(ui) { synchronize(ui,this,false); },
    plotSupplier_updated: function(ui) { synchronize(ui,this,false); },
    plotX_changed: function(ui) { synchronize(ui,this,false); },
    plotPanels_changed: function(ui) { synchronize(ui,this,false); },
    plotGroup_changed: function(ui) { synchronize(ui,this,false); }
};
