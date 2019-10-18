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
    let columnName = ui.preds.value([0]);
    let givenName = ui.given.value([0]);
    
    if (givenName==="") {
          ui.view.model.options.beginEdit();
          ui.ghost.setPropertyValue('enable');      
          ui.bins.setPropertyValue('enable'); 
          ui.view.model.options.endEdit();
    }    
    
    if (columnName) {

        //request column info - dataType and measureType
        let promise = context.requestData("column", { columnName: columnName, properties: ["dataType", "measureType"] })
        promise.then(rData => {
            if (rData.columnFound) {
                console.log(rData.dataType);
                console.log(rData.measureType);

                //disable checkbox call 'suppr' and 'line' when the dataType is not an integer
                ui.view.model.options.beginEdit();
                ui.suppr.setPropertyValue('enable', rData.dataType === 'integer' );
                ui.line.setPropertyValue('enable', rData.dataType === 'integer' );
                ui.se.setPropertyValue('enable', rData.dataType === 'integer' );
                ui.center.setPropertyValue('enable', rData.dataType === 'text' );
                ui.plmethod.setPropertyValue('enable', rData.dataType === 'text' );
                ui.view.model.options.endEdit();
            } 
        });
    }
};

module.exports = events;
