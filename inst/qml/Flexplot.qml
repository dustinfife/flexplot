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
		  id: varlist
			name: "variables";	
			title: qsTr("Independent Variable(s)");
			singleVariable: false 
		}
        
		AssignedVariablesList 
		{
  		id: paneledVars
			name: "paneledVars";	
			title: qsTr("Panelled Variable(s)");
			singleVariable: false 
		}

  }

  ExpanderButton
  {
      title: qsTr("Options")
      
        Group{
        title: qsTr("Visual Aids")
             CheckBox{
              name:"ghost"; 
              label: qsTr("Ghost lines");
              checked: true
              enabled: paneledVars.count > 0
            }
		        Slider{
              name: "alpha"
              label: qsTr("Point transparency")
              value: 0.4
              vertical: false
              enabled: varlist.count > 0
            }
        }
        
        Group{
        title: qsTr("Aesthetics")
            DropDown{
			        name: "theme"
			        values: ["JASP", "Black and white", "Minimal", "Classic", "Dark"]
			        label: qsTr("GGplot theme")
		        }
        }
        
        Group{
        title: qsTr("Visual Statistics")
            CheckBox{
              name:"confidence"; 
              label: qsTr("Plot confidence bands")
               enabled: varlist.count > 0
            }
		        DropDown{
			        name: "type"
			        values: ["Loess", "Regression", "Quadratic", "Cubic"]
			        label: qsTr("Fitted line (scatterplots)")
			         enabled: varlist.count > 0
		        }
		        DropDown{
			        name: "intervals"
			        values: ["Quartiles", "Standard errors", "Standard deviations"]
			        label: qsTr("Intervals")
			         enabled: varlist.count > 0
		        }		        
        }
  }

}
