import QtQuick 2.8
import JASP.Controls 1.0
import JASP.Widgets 1.0
import JASP.Theme 1.0
Form 
{
    usesJaspResults: true

    VariablesForm
	{
        AvailableVariablesList { name: "allVariables" }

        AssignedVariablesList 
		{ 
			name: "dependent";	
			title: qsTr("Dependent Variable");
			singleVariable: true 
		}
        
		AssignedVariablesList 
		{ 
			name: "variables";	
			title: qsTr("Independent Variable(s)");
			singleVariable: false 
		}
        
		AssignedVariablesList 
		{
			name: "paneledVars";	
			title: qsTr("Panelled Variable(s)");
			singleVariable: false 
		}
	
	VariablesList 
	{
			title: qsTr("Ghost Lines")
			source: "paneledVars"
			name: "ghostLines"
			listViewType: "AssignedVariables"
			height: 60
			draggable: false

			ExtraControlColumn {
				type: "TextField"
				name: "ghostlinepoint"
			}
		}

    }

    ExpanderButton
    {
        title: qsTr("Options")

        Group
        {
            CheckBox{name:"confidence"; label: qsTr("Plot Confidence Bands")}
		DropDown{
			name: "type"
			values: ["loess", "regression", "polynomial", "cubic"]
			label: qsTr("Fitted Line")
		}

		
        }
    }

}
