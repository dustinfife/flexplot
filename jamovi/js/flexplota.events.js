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

    // data changed event
    onRemoteDataChanged: function(ui, data) {
        updateUI(ui, this);
    }
};

const updateUI = function(ui, context) {

    // change depending on numeric/categorical variable
    let columnNames = ui.preds.value();
    let givenNames = ui.given.value();

    // if givenNames array is empty or null
    if ( ! givenNames || givenNames.length === 0) {
          ui.view.model.options.beginEdit();
          ui.ghost.setPropertyValue('enable', false);
          ui.bins.setPropertyValue('enable', false);
          ui.resid.setPropertyValue('enable', false);
          ui.view.model.options.endEdit();
    }
    else {
        ui.view.model.options.beginEdit();
        ui.ghost.setPropertyValue('enable', true);
        ui.bins.setPropertyValue('enable', true);
        ui.resid.setPropertyValue('enable', true);
        ui.view.model.options.endEdit();
    }

    // if columnNames array is not empty or null
    if (columnNames && columnNames.length > 0) {
        let columnName = columnNames[0];
        //request column info - dataType and measureType
        let promise = context.requestData("column", { columnName: columnName, properties: ["dataType", "measureType"] })
        promise.then(rData => {
            if (rData.columnFound) {
                console.log(rData.dataType);
                console.log(rData.measureType);

                //disable checkbox call 'suppr' and 'line' when the dataType is not an integer
                ui.view.model.options.beginEdit();
                ui.suppr.setPropertyValue('enable', rData.dataType === 'decimal' || rData.dataType === 'integer');
                ui.line.setPropertyValue('enable', rData.dataType === 'decimal' || rData.dataType === 'integer');
                ui.se.setPropertyValue('enable', rData.dataType === 'decimal' || rData.dataType === 'integer' );
                ui.center.setPropertyValue('enable', rData.dataType === 'text' );
                ui.plmethod.setPropertyValue('enable', rData.dataType === 'text' );
                ui.view.model.options.endEdit();
            }
        });
    }
};

module.exports = events;
