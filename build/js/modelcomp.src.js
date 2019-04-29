
// This file is an automatically generated and should not be edited

'use strict';

const options = [{"name":"data","type":"Data"},{"name":"dep","title":"Dependent Variable","type":"Variable"},{"name":"pred","title":"Predictor Variable(s) for Model 1","type":"Variables"},{"name":"pred2","title":"Predictor Variable(s) for Model 2","type":"Variables"}];

const view = View.extend({
    jus: "2.0",

    events: [

	]

});

view.layout = ui.extend({

    label: "Model Comparison",
    jus: "2.0",
    type: "root",
    stage: 0, //0 - release, 1 - development, 2 - proposed
    controls: [
		{
			type: DefaultControls.VariableSupplier,
			persistentItems: false,
			stretchFactor: 1,
			controls: [
				{
					type: DefaultControls.TargetLayoutBox,
					label: "Dependent Variable",
					controls: [
						{
							type: DefaultControls.VariablesListBox,
							name: "dep",
							maxItemCount: 1,
							isTarget: true
						}
					]
				},
				{
					type: DefaultControls.TargetLayoutBox,
					label: "Predictor Variable(s) for Model 1",
					controls: [
						{
							type: DefaultControls.VariablesListBox,
							name: "pred",
							isTarget: true
						}
					]
				}
			]
		},
		{
			type: DefaultControls.VariableSupplier,
			persistentItems: false,
			stretchFactor: 1,
			controls: [
				{
					type: DefaultControls.TargetLayoutBox,
					label: "Predictor Variable(s) for Model 2",
					controls: [
						{
							type: DefaultControls.VariablesListBox,
							name: "pred2",
							isTarget: true
						}
					]
				}
			]
		}
	]
});

module.exports = { view : view, options: options };
