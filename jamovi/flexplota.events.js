const events = {
    update: function(ui) {
        updateUI(ui, this);
    },

    onChange_preds: function(ui) {
        updateUI(ui, this);
    },

    // data changed event
    onRemoteDataChanged: function(ui, data) {
        updateUI(ui, this);
    }
};

const updateUI = function(ui, context) {
    let columnName = ui.preds.value();
    if (columnName) {

        //request column info - dataType and measureType
        let promise = context.requestData("column", { columnName: columnName, properties: ["dataType", "measureType"] })
        promise.then(rData => {
            if (rData.columnFound) {
                console.log(rData.dataType);
                console.log(rData.measureType);

                //disable checkbox call 'suppr' and 'line' when the dataType is not an integer
                ui.suppr.setPropertyValue('enable', rData.dataType === 'integer' );
                //ui.line.setPropertyValue('enable', rData.dataType === 'integer' );
            } 
        });
    }
};

module.exports = events;
