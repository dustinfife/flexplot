const updates = new WeakMap();

const events = {
    update: function(ui) {
        updateUI(ui, this);
    },
    onChange_preds: function(ui) {
        updateUI(ui, this);
    },
    onChange_given: function(ui) {
        updateUI(ui, this);
    },
    onRemoteDataChanged: function(ui) {
        updateUI(ui, this);
    }
};

const updateUI = function(ui, context) {
    const request = {};
    updates.set(ui, request);
    const predictors = ui.preds.value() || [];
    const panels = ui.given.value() || [];
    const hasPanels = panels.length > 0;
    ui.view.model.options.beginEdit();
    ui.suppr.setPropertyValue('enable', true);
    ui.ghost.setPropertyValue('enable', hasPanels);
    ui.resid.setPropertyValue('enable', hasPanels);
    ui.bins.setPropertyValue('enable', false);
    ui.view.model.options.endEdit();

    // Only the second axis and conditioning variables are binned. Numeric
    // storage alone does not make nominal/ordinal variables continuous.
    const binning = [...new Set([...predictors.slice(1, 2), ...panels])];
    const names = [...new Set([...predictors.slice(0, 1), ...binning])];
    Promise.all(names.map(columnName => context.requestData('column', {
        columnName, properties: ['dataType', 'measureType']
    }))).then(columns => {
        if (updates.get(ui) !== request) return;
        const metadata = new Map(names.map((name, index) => [name, columns[index]]));
        const numeric = column => column && column.columnFound &&
            column.measureType === 'continuous' && ['decimal', 'integer'].includes(column.dataType);
        ui.view.model.options.beginEdit();
        ui.bins.setPropertyValue('enable', binning.some(name => numeric(metadata.get(name))));
        const first = metadata.get(predictors[0]);
        if (first && first.columnFound) {
            const numericStorage = first.dataType === 'decimal' || first.dataType === 'integer';
            ui.line.setPropertyValue('enable', numericStorage);
            ui.se.setPropertyValue('enable', numericStorage);
            ui.center.setPropertyValue('enable', first.dataType === 'text');
            ui.plmethod.setPropertyValue('enable', first.dataType === 'text');
        }
        ui.view.model.options.endEdit();
    }).catch(() => {
        // An unavailable column cannot establish bin eligibility. Do not let
        // an obsolete failed request disable a newly eligible layout.
        if (updates.get(ui) === request) ui.bins.setPropertyValue('enable', false);
    });
};

module.exports = events;
