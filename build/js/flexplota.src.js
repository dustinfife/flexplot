
// This file is an automatically generated and should not be edited

'use strict';

const options = [{"name":"data","type":"Data"},{"name":"out","title":"Dependent variable","type":"Variable"},{"name":"preds","title":"Predictor variable","type":"Variables"},{"name":"given","title":"Paneled variable","type":"Variables"},{"name":"se","title":"Confidence bands (for scatterplots)","type":"Bool","default":true},{"name":"line","title":"Fitted line (for scatterplots)","type":"List","options":["Loess","Regression","Logistic","Polynomial","Cubic","Robust"],"default":"Loess"},{"name":"ghost","title":"Ghost line","type":"Bool","default":true},{"name":"thirdeye","title":"The \"third eye\"","type":"Bool","default":false},{"name":"diff","title":"Difference plot (related t)","type":"Bool","default":false},{"name":"plmethod","title":"Plot method (categorical predictors)","type":"List","options":["Jittered-density plot","Boxplot","Violin plot"],"default":"Jittered-density plot"},{"name":"resid","title":"Added variable plot","type":"Bool","default":false},{"name":"suppr","title":"Suppress fitted line","type":"Bool","default":false},{"name":"center","title":"Center/spread (categorical predictors)","type":"List","options":["Median + quartiles","Mean + sterr","Mean + stdev"],"default":"Median + quartiles"},{"name":"alpha","title":"Transparency of dots (%)","type":"Number","min":0,"max":100,"default":50},{"name":"sample","title":"Percent of dots to display","type":"Number","min":0,"max":100,"default":100},{"name":"jittx","title":"Amount of jittering in X","type":"Number","min":0,"max":0.5,"default":0.2},{"name":"jitty","title":"Amount of jittering in Y","type":"Number","min":0,"max":0.5,"default":0},{"name":"bins","title":"Number of bins (for numeric variables)","type":"Number","min":2,"max":6,"default":3},{"name":"text","type":"String"}];

const view = function() {
    
    

    View.extend({
        jus: "2.0",

        events: [

	],

	update: require('./flexplota.events').update,

	remoteDataChanged: require('./flexplota.events').onRemoteDataChanged

    }).call(this);
}

view.layout = ui.extend({

    label: "Flexplot",
    jus: "2.0",
    type: "root",
    stage: 0, //0 - release, 1 - development, 2 - proposed
    controls: [
		{
			type: DefaultControls.VariableSupplier,
			typeName: 'VariableSupplier',
			persistentItems: false,
			stretchFactor: 1,
			controls: [
				{
					type: DefaultControls.TargetLayoutBox,
					typeName: 'TargetLayoutBox',
					label: "Outcome variable",
					controls: [
						{
							type: DefaultControls.VariablesListBox,
							typeName: 'VariablesListBox',
							name: "out",
							maxItemCount: 1,
							isTarget: true
						}
					]
				},
				{
					type: DefaultControls.TargetLayoutBox,
					typeName: 'TargetLayoutBox',
					label: "Predictor variable",
					controls: [
						{
							type: DefaultControls.VariablesListBox,
							typeName: 'VariablesListBox',
							name: "preds",
							isTarget: true,
							events: [
								{ execute: require('./flexplota.events').onChange_preds }
							]
						}
					]
				},
				{
					type: DefaultControls.TargetLayoutBox,
					typeName: 'TargetLayoutBox',
					label: "Paneled variable",
					controls: [
						{
							type: DefaultControls.VariablesListBox,
							typeName: 'VariablesListBox',
							name: "given",
							isTarget: true
						}
					]
				}
			]
		},
		{
			type: DefaultControls.CollapseBox,
			typeName: 'CollapseBox',
			label: "Plot type",
			collapsed: true,
			margin: "large",
			stretchFactor: 1,
			controls: [
				{
					type: DefaultControls.CheckBox,
					typeName: 'CheckBox',
					name: "resid"
				},
				{
					type: DefaultControls.CheckBox,
					typeName: 'CheckBox',
					name: "thirdeye"
				},
				{
					type: DefaultControls.CheckBox,
					typeName: 'CheckBox',
					name: "diff"
				}
			]
		},
		{
			type: DefaultControls.CollapseBox,
			typeName: 'CollapseBox',
			label: "Fit options",
			collapsed: true,
			margin: "large",
			stretchFactor: 1,
			controls: [
				{
					type: DefaultControls.CheckBox,
					typeName: 'CheckBox',
					name: "se"
				},
				{
					type: DefaultControls.ComboBox,
					typeName: 'ComboBox',
					name: "line"
				},
				{
					type: DefaultControls.CheckBox,
					typeName: 'CheckBox',
					name: "suppr"
				},
				{
					type: DefaultControls.ComboBox,
					typeName: 'ComboBox',
					name: "center"
				},
				{
					type: DefaultControls.ComboBox,
					typeName: 'ComboBox',
					name: "plmethod"
				}
			]
		},
		{
			type: DefaultControls.CollapseBox,
			typeName: 'CollapseBox',
			label: "Point options",
			collapsed: true,
			margin: "large",
			stretchFactor: 1,
			controls: [
				{
					type: DefaultControls.TextBox,
					typeName: 'TextBox',
					name: "alpha",
					format: FormatDef.number
				},
				{
					type: DefaultControls.TextBox,
					typeName: 'TextBox',
					name: "sample",
					format: FormatDef.number
				},
				{
					type: DefaultControls.TextBox,
					typeName: 'TextBox',
					name: "jittx",
					format: FormatDef.number
				},
				{
					type: DefaultControls.TextBox,
					typeName: 'TextBox',
					name: "jitty",
					format: FormatDef.number
				}
			]
		},
		{
			type: DefaultControls.CollapseBox,
			typeName: 'CollapseBox',
			label: "Paneling options",
			collapsed: true,
			name: "panel_box",
			margin: "large",
			stretchFactor: 1,
			controls: [
				{
					type: DefaultControls.CheckBox,
					typeName: 'CheckBox',
					name: "ghost"
				},
				{
					type: DefaultControls.TextBox,
					typeName: 'TextBox',
					name: "bins",
					format: FormatDef.number
				}
			]
		},
		{
			type: DefaultControls.LayoutBox,
			typeName: 'LayoutBox',
			margin: "large",
			controls: [
				{
					type: DefaultControls.TextBox,
					typeName: 'TextBox',
					name: "text",
					format: FormatDef.string
				}
			]
		}
	]
});

module.exports = { view : view, options: options };
