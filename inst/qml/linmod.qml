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
			singleVariable: true;
			allowedColumns: ["scale"]
		}
		AssignedVariablesList { 
		  id: vars
			name: "variables";	
			title: qsTr("Independent Variable(s)");
			singleVariable: false 
		}
  }
  
  ExpanderButton{
    title: qsTr("Visual Fitting")  
        DropDown{
	        name: "linetype"
	        values: ["Regression", "Quadratic", "Cubic"]
	        label: qsTr("Fitted line (scatterplots)")
	         enabled: varlist.count > 0
        }
        
  }

  
  ExpanderButton{
    title: qsTr("Model Terms")  
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
        title: qsTr("Model terms"); 
        listViewType:"Interaction"
        enabled: vars.count > 1
      }
    }
  }  
  
		
  ExpanderButton{
    title: qsTr("Results Displays")

    Group{
    title: qsTr("Plots")
		  CheckBox{
			  name:"model"; 
			  label: qsTr("Model plot");
			  checked: true
			  }
      CheckBox{
			  name:"residuals"; 
			  label: qsTr("Diagnostics")
			  }
      CheckBox{
			  name:"avp"; 
			  label: qsTr("Added variable plot")
			  }			  
		  }
		  
		  
		Group{
    title: qsTr("Estimation")
      CheckBox{
			  name:"modinf"; 
			  label: qsTr("Show model comparisons")
			}	
      CheckBox{
			  name:"means"; 
			  label: qsTr("Report means");
			  checked: true
			}	
      CheckBox{
			  name:"diff"; 
			  label: qsTr("Show mean differences")
			  checked: true
			}	
      CheckBox{
			  name:"sl"; 
			  label: qsTr("Show slopes/intercepts");
			  checked: true
			}		
      CheckBox{
			  name:"ci"; 
			  label: qsTr("Show 95% intervals");
			  checked: true
			}	 			
		}
	}
		
		  ExpanderButton
  {
      title: qsTr("Plot Controls")
      
        Group{
        title: qsTr("Visual Aids")
             CheckBox{
              name:"ghost"; 
              label: qsTr("Ghost lines");
              checked: true
              enabled: vars.count > 0 & vars.count< 4
            }
		        Slider{
              name: "alpha"
              label: qsTr("Point transparency")
              value: 0.4
              vertical: true
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
  }

  
  
}