'use strict';

const outputs = {
    model:{x:'plotX',style:'plotStyle',scale:'plotScale'},
    comparison:{x:'comparisonX',style:'comparisonPlotStyle',scale:'comparisonPlotScale'}
};

// Eligibility never overwrites a stored preference: R resolves unavailable display
// combinations using the existing probability fallback, independently per plot.
module.exports = function(ui, context) {
    if (typeof context.requestData !== 'function') return;
    const states = context.workspace.glmPlotControls || (context.workspace.glmPlotControls = {});
    return Promise.all(Object.entries(outputs).map(([output,names]) => {
        const selected = ui.family.value();
        let family = selected;
        if (selected === 'auto') {
            const options = ui.view.model.options;
            const detected = options.getOption('automaticFamily');
            const inspected = options.getOption('automaticFamilyOutcome');
            family = inspected && inspected.getValue() === ui.out.value() && detected ? detected.getValue() : null;
        }
        const x = ui[names.x].value(), style = ui[names.style].value();
        const active = output !== 'comparison' || ui.compare.value() === true;
        const key = JSON.stringify([selected,family,ui.out.value(),x,style,active]);
        if (states[output] && states[output].key === key) return states[output].pending;
        const state = {key};
        states[output] = state;
        ui[names.style].setPropertyValue('enable',false);
        ui[names.scale].setPropertyValue('enable',false);
        if (!active || family !== 'binomial' || !x) return;
        state.pending = context.requestData('column',
            {columnName:x,properties:['measureType','dataType']}).then(column => {
            if (context.workspace.glmPlotControls !== states || states[output] !== state) return;
            const continuous = column && column.columnFound && column.measureType === 'continuous' &&
                ['decimal','integer'].includes(column.dataType);
            ui[names.style].setPropertyValue('enable',!!continuous);
            ui[names.scale].setPropertyValue('enable',!!continuous && style === 'auto');
        }).catch(() => {
            // Failed metadata stays disabled. A later view update can retry;
            // invalidated responses cannot replace the newer output's state.
            if (context.workspace.glmPlotControls === states && states[output] === state) state.key = null;
        });
        return state.pending;
    }));
};
