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
    title: qsTr("Plot Options")

    Group{
		  
		  CheckBox{
			  name:"modelPlot"; 
			  label: qsTr("Model plot")
			  }
      
      CheckBox{
			  name:"univariates"; 
			  label: qsTr("Univariates")
			  }			  
      
      CheckBox{
			  name:"residualPlot"; 
			  label: qsTr("Diagnostics")
			  }			  
			  
		  }
		}		
}