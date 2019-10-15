  module.exports = {
    
        view_loaded: function(ui, event) {
                ui.view.model.options.beginEdit();
                ui.text.setValue("empty");
                ui.view.model.options.endEdit();
        },    
        given_changed: function(ui, event) {
            let givVal = ui.given.value();
            //if (givVal === "")
                ui.view.model.options.beginEdit();
                ui.jitty.setValue(0.212);
                ui.jittx.setValue(0.312);
                ui.sample.setValue(51);
                ui.ghost.removeAttribute("enabled");
                ui.text.setValue(givVal);
                ui.view.model.options.endEdit();
        }
    };