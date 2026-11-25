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
        const capability = options.getOption('comparisonPAvailable');
        const pKey = () => JSON.stringify(['interactions','polynomials','comparisonInteractions','comparisonPolynomials','out','preds','cluster','modelTerms','quadratic','randomIntercept','randomSlopes',
            'estimation','comparisonFixed','comparisonTerms','comparisonQuadratic','comparisonRandomIntercept',
            'comparisonRandomSlopes','comparisonEstimation','compare','comparisonStatistics'].map(name=>ui[name].value()));
        if (context.workspace.mixedPKey !== undefined && context.workspace.mixedPKey !== pKey() && capability && capability.getValue())
            options.setOptionValue('comparisonPAvailable',false);
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
            layouts.push(layout);
            const modeKey = comparison ? 'mixedComparisonMode' : 'mixedMode';
            if (context.workspace[modeKey] !== undefined && context.workspace[modeKey] !== prediction &&
                prediction === 'cluster' && cluster && ui[group].value() !== cluster) {
                const panels = ui[panelsName].value() || [];
                if (panels.length < 2 && !panels.includes(cluster)) ui[panelsName].setValue(panels.concat(cluster));
            }
            context.workspace[modeKey] = prediction;
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
        context.workspace.mixedPKey = pKey();
    } finally {
        try { options.endEdit(); }
        finally { context.workspace.mixedSynchronizing = false; }
    }
    updateGhostControls(ui, context, 'mixedmodel');
    updatePControl(ui,context);
};
const updatePControl = function(ui, context) {
    const capability = ui.view.model.options.getOption('comparisonPAvailable');
    if (ui.includeP && typeof ui.includeP.setPropertyValue === 'function')
        ui.includeP.setPropertyValue('enable',ui.compare.value() === true && ui.comparisonStatistics.value() === true &&
            !!capability && capability.getValue() === true);
};
const roleChanging = function(ui, event, comparison, x) {
    const values = [].concat(event.value || []);
    const cas = ui[comparison ? 'comparisonPredictionType' : 'predictionType'].value() === 'cas';
    if ((x || cas) && values.includes(ui.cluster.value())) event.cancel = true;
};
module.exports = {
    randomIntercept_changed: function(ui) { synchronize(ui,this,false); },
    comparisonRandomIntercept_changed: function(ui) { synchronize(ui,this,false); },
    comparisonPAvailable_changed: function(ui) { updatePControl(ui,this); },
    comparisonStatistics_changed: function(ui) { synchronize(ui,this,false); },
    estimation_changed: function(ui) { synchronize(ui,this,false); },
    comparisonEstimation_changed: function(ui) { synchronize(ui,this,false); },
    plotX_changing: function(ui,event) { roleChanging(ui,event,false,true); },
    comparisonX_changing: function(ui,event) { roleChanging(ui,event,true,true); },
    plotPanels_changing: function(ui,event) { roleChanging(ui,event,false,false); },
    comparisonPanels_changing: function(ui,event) { roleChanging(ui,event,true,false); },
    graphic_changed: function(ui) { updateGhostControls(ui, this, 'mixedmodel'); },
    ghostLines_changed: function(ui) { updateGhostControls(ui, this, 'mixedmodel'); },
    plotGroup_changing: function(ui, event) {
        roleChanging(ui,event,false,false);
    },
    comparisonGroup_changing: function(ui,event) {
        updateTerms.groupChanging(ui.comparisonPredictionType.value() === 'cas',event);
        roleChanging(ui,event,true,false);
    },
    comparisonPlotSupplier_updated: function(ui) { synchronize(ui,this,false); },
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
