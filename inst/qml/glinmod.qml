import QtQuick 2.8
import JASP.Controls 1.0
import JASP.Widgets 1.0
import JASP.Theme 1.0
Form 
{
  usesJaspResults: true

  VariablesForm
	{
    AvailableVariablesList { 
      name: "allVariables" 
    }
    AssignedVariablesList { 
			name: "dependent";	
			title: qsTr("Dependent Variable");
			singleVariable: true 
		}
		AssignedVariablesList { 
			name: "variables";	
			title: qsTr("Independent Variable(s)");
			singleVariable: false 
		}
  }
  
  
  ExpanderButton{
    title: qsTr("Interaction Terms")  
    VariablesForm
    {
      height: 150
      AvailableVariablesList { 
        name: "components"; 
        title: qsTr("Components"); 
        source: ["variables"] 
      }
  
      AssignedVariablesList 
      { 
        name: "interactions"; 
        title: qsTr("Model Terms"); 
        listViewType:"Interaction" 
      }
    }
  }  
  
		
  ExpanderButton{
    title: qsTr("Plot Options")

    Group{
		  CheckBox{
			  name:"model"; 
			  label: qsTr("Model plot")
			  }
      CheckBox{
			  name:"univariates"; 
			  label: qsTr("Univariates")
			  }			  
      CheckBox{
			  name:"residuals"; 
			  label: qsTr("Diagnostics")
			  }			  
		  }
		}
		
		
	ExpanderButton{
    title: qsTr("Estimation Options")

    Group{
      CheckBox{
			  name:"ci"; 
			  label: qsTr("Show Intervals")
			}	
		  DropDown{
			  name: "estimationmethod"
			  values: ["Bootstrapped Intervals", "Credible Interval", "Confidence Interval"]
			  label: qsTr("Interval Estimation")
		  }
		}
  }
  
}