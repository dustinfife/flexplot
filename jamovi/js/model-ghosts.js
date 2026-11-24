'use strict';
// Metadata gates potential layouts; R validates the actual prepared prediction grid.
module.exports = function(ui, context, module = 'glmgeneralized') {
    if (!ui.ghostLines || typeof ui.ghostLines.setPropertyValue !== 'function') return;
    if (context.flags && (context.flags.updating || context.flags.loaded === false)) return;
    const token = {};
    context.workspace.modelGhostRequest = token;
    ui.ghostLines.setPropertyValue('enable',false);
    const mixed = module === 'mixedmodel';
    const terms = ui.modelTerms.value();
    const predictors = [...new Set((terms == null ? ui.preds.value() || [] : terms.flat()).concat(ui.quadratic.value() || []))];
    const group = ui.plotGroup.value();
    let panels = ui.plotPanels.value() || [];
    if (mixed && ui.predictionType.value() === 'cluster' && !panels.length && !group && ui.cluster.value()) panels = [ui.cluster.value()];
    if (!ui.graphic.value() || !ui.out.value() || !predictors.length || !panels.length || panels.length > 2 ||
        (group || []).length || mixed && (ui.compare.value() || ui.predictionType.value() === 'cas') ||
        typeof context.requestData !== 'function') return;
    const selected = ui.plotX.value();
    const available = mixed ? predictors.filter(name => name !== ui.cluster.value()) : predictors.filter(name => !panels.includes(name));
    const candidates = selected ? [selected] : available;
    if (!candidates.length) return;
    const names = [...new Set([...candidates,...panels])];
    return Promise.all(names.map(columnName => context.requestData('column',
        {columnName,properties:['measureType','dataType','levels']}))).then(columns => {
        if (context.workspace.modelGhostRequest !== token) return;
        const metadata = name => columns[names.indexOf(name)];
        const numeric = column => column && column.columnFound && column.measureType === 'continuous' &&
            ['integer','decimal'].includes(column.dataType);
        // LM/GLM resolve automatic x to the first numeric fitted predictor outside
        // panels. Mixed retains its existing first-predictor convention.
        const x = selected || (mixed ? candidates[0] : candidates.find(name => numeric(metadata(name))) || candidates[0]);
        const column = metadata(x);
        if (!numeric(column) || Array.isArray(column.levels) && column.levels.length === 1 ||
            panels.includes(x) || columns.some(item => !item || !item.columnFound)) return;
        const multiple = panels.some(name => {
            const panel = metadata(name);
            return Array.isArray(panel.levels) && panel.levels.length > 1 ||
                numeric(panel) && (!Array.isArray(panel.levels) || panel.levels.length !== 1);
        });
        ui.ghostLines.setPropertyValue('enable',multiple);
    }).catch(() => {
        if (context.workspace.modelGhostRequest === token) ui.ghostLines.setPropertyValue('enable',false);
    });
};
