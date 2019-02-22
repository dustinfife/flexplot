
// This file is an automatically generated and should not be edited

'use strict';

const options = [{"name":"data","type":"Data"},{"name":"out","title":"Outcome Variable","type":"Variable"},{"name":"preds","title":"Predictor Variable(s)","type":"Variables"},{"name":"se","title":"Plot Confidence Bands (for scatterplots)","type":"Bool","default":true},{"name":"line","title":"Fitted line (for scatterplots)","type":"List","options":["loess","lm","logistic"],"default":"loess"},{"name":"center","title":"Center/Spread (for dot plots)","type":"List","options":["median + quartiles","mean + sterr","mean + stdev"],"default":"median + quartiles"},{"name":"alpha","title":"Transparency of Dots","type":"Number","min":0,"max":1,"default":0.5}];

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
