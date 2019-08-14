
// This file is an automatically generated and should not be edited

'use strict';

const options = [{"name":"data","type":"Data"},{"name":"out","title":"Dependent variable","type":"Variable"},{"name":"preds","title":"Predictor variable","type":"Variables"},{"name":"graphic","title":"Analysis graphs","type":"Bool","default":true},{"name":"graphicassump","title":"Diagnostics","type":"Bool","default":false},{"name":"estimates","title":"Estimates and effect eizes","type":"Bool","default":true},{"name":"se","title":"Standard errors","type":"Bool","default":true},{"name":"line","title":"Fitted line (for scatterplots)","type":"List","options":["Loess","Regression","Logistic"],"default":"Loess"},{"name":"center","title":"Center/spread (for dot plots)","type":"List","options":["Median + quartiles","Mean + sterr","Mean + stdev"],"default":"Median + quartiles"}];

const view = View.extend({
    jus: "2.0",

    events: [

	]

});

view.layout = ui.extend({

    label: "General Linear Model",
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
					label: "Outcome Variable",
					controls: [
						{
							type: DefaultControls.VariablesListBox,
							name: "out",
							maxItemCount: 1,
							isTarget: true
						}
					]
				},
				{
					type: DefaultControls.TargetLayoutBox,
					label: "Predictor Variable(s)",
					controls: [
						{
							type: DefaultControls.VariablesListBox,
							name: "preds",
							isTarget: true
						}
					]
				}
			]
		},
		{
			type: DefaultControls.LayoutBox,
			margin: "large",
			stretchFactor: 1,
			controls: [
				{
					type: DefaultControls.LayoutBox,
					margin: "large",
					stretchFactor: 1,
					cell: {"column":0,"row":0},
					controls: [
						{
							type: DefaultControls.Label,
							label: "Output Options",
							controls: [
								{
									type: DefaultControls.CheckBox,
									name: "graphic"
								},
								{
									type: DefaultControls.CheckBox,
									name: "graphicassump"
								},
								{
									type: DefaultControls.CheckBox,
									name: "estimates"
								}
							]
						}
					]
				}
			]
		},
		{
			type: DefaultControls.LayoutBox,
			margin: "large",
			controls: [
				{
					type: DefaultControls.LayoutBox,
					margin: "large",
					stretchFactor: 1,
					cell: {"column":0,"row":0},
					controls: [
						{
							type: DefaultControls.Label,
							label: "Graphic Options",
							controls: [
								{
									type: DefaultControls.CheckBox,
									name: "se"
								},
								{
									type: DefaultControls.ComboBox,
									name: "line"
								},
								{
									type: DefaultControls.ComboBox,
									name: "center"
								}
							]
						}
					]
				}
			]
		}
	]
});

module.exports = { view : view, options: options };
