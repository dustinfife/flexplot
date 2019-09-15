
// This file is an automatically generated and should not be edited

'use strict';

const options = [{"name":"data","type":"Data"},{"name":"out","title":"Dependent variable","type":"Variable"},{"name":"preds","title":"Predictor variable","type":"Variables"},{"name":"given","title":"Paneled variable","type":"Variables"},{"name":"se","title":"Confidence bands (for scatterplots)","type":"Bool","default":true},{"name":"line","title":"Fitted line (for scatterplots)","type":"List","options":["Loess","Regression","Logistic","Polynomial","Cubic"],"default":"Loess"},{"name":"ghost","title":"Ghost line","type":"Bool","default":true},{"name":"resid","title":"Residualize predictor variable","type":"Bool","default":false},{"name":"center","title":"Center/spread (for dot plots)","type":"List","options":["Median + quartiles","Mean + sterr","Mean + stdev"],"default":"Median + quartiles"},{"name":"alpha","title":"Transparency of dots (%)","type":"Number","min":0,"max":100,"default":50}];

const view = View.extend({
    jus: "2.0",

    events: [

	]

});

view.layout = ui.extend({

    label: "Flexplot",
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
					label: "Outcome variable",
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
					label: "Predictor variable",
					controls: [
						{
							type: DefaultControls.VariablesListBox,
							name: "preds",
							isTarget: true
						}
					]
				},
				{
					type: DefaultControls.TargetLayoutBox,
					label: "Paneled variable",
					controls: [
						{
							type: DefaultControls.VariablesListBox,
							name: "given",
							isTarget: true
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
									type: DefaultControls.CheckBox,
									name: "ghost"
								},
								{
									type: DefaultControls.CheckBox,
									name: "resid"
								},
								{
									type: DefaultControls.ComboBox,
									name: "line"
								},
								{
									type: DefaultControls.ComboBox,
									name: "center"
								},
								{
									type: DefaultControls.TextBox,
									name: "alpha",
									format: FormatDef.number
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
