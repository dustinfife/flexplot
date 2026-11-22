'use strict';
// Column metadata gates potential layouts; R validates the actual prepared grid.
module.exports = function(ui, context) {
    if (!ui.ghostLines || typeof ui.ghostLines.setPropertyValue !== 'function') return;
    if (context.flags && (context.flags.updating || context.flags.loaded === false)) return;
    const token = {};
    context.workspace.glmGhostRequest = token;
    ui.ghostLines.setPropertyValue('enable',false);
    const panels = ui.plotPanels.value() || [];
    const terms = ui.modelTerms.value();
    if (!ui.graphic.value() || !ui.out.value() || !(ui.preds.value() || []).length || !ui.plotX.value() || !panels.length ||
        (ui.plotGroup.value() || []).length ||
        (Array.isArray(terms) && !terms.length && !(ui.quadratic.value() || []).length) ||
        typeof context.requestData !== 'function') return;
    const names = [ui.plotX.value(),...panels];
    return Promise.all(names.map(columnName => context.requestData('column',
        {columnName,properties:['measureType','dataType','levels']}))).then(columns => {
        if (context.workspace.glmGhostRequest !== token) return;
        const numeric = column => column && column.columnFound && column.measureType === 'continuous' &&
            ['integer','decimal'].includes(column.dataType);
        if (!numeric(columns[0]) || Array.isArray(columns[0].levels) && columns[0].levels.length === 1 || columns.some(column => !column || !column.columnFound)) return;
        const multiple = columns.slice(1).some(column =>
            Array.isArray(column.levels) && column.levels.length > 1 ||
            numeric(column) && (!Array.isArray(column.levels) || column.levels.length !== 1));
        ui.ghostLines.setPropertyValue('enable',multiple);
    }).catch(() => {
        if (context.workspace.glmGhostRequest === token) ui.ghostLines.setPropertyValue('enable',false);
    });
};
