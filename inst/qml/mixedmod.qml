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
		  id: vars
			name: "variables";	
			title: qsTr("Independent Variable(s)");
			singleVariable: false 
		}
		AssignedVariablesList { 
			name: "rvariables";	
			title: qsTr("Random");
			singleVariable: true 
		}		
  }
  
  ExpanderButton{
    title: qsTr("Model Builder")  
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
        title: qsTr("Fixed terms"); 
        listViewType:"Interaction"
  
        ExtraControlColumn {
          type: "CheckBox"
          name: "randeff2"
          title: "Add as a random effect"
        }
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
			  name:"univariates"; 
			  label: qsTr("Univariate plots");
			  checked: true
			  }	

      CheckBox{
			  name:"residuals"; 
			  label: qsTr("Diagnostics")
			  }
			  
		  }
		Group{
    title: qsTr("Estimation")
      CheckBox{
			  name:"fixeff"; 
			  label: qsTr("Report fixed effects");
			  checked: true
			}	
      CheckBox{
			  name:"randeff"; 
			  label: qsTr("Report random effects")
			  checked: false
			}	
		}

		}
		  
		  

		
		  ExpanderButton
  {
      title: qsTr("Plot Controls")
      
        Group{
        title: qsTr("Point controls")
        columns: 4
		        Slider{
              name: "alpha"
              label: qsTr("Point transparency")
              value: 0.4
              vertical: true
              enabled: varlist.count > 0
            }
		        Slider{
              name: "jitx"
              label: qsTr("Jitter in X")
              value: .1
              min: 0
              max: .5
              vertical: true
              enabled: varlist.count > 0
            }  
		        Slider{
              name: "jity"
              label: qsTr("Jitter in Y")
              value: 0
              min: 0
              max: .5
              vertical: true
              enabled: varlist.count > 0
            }   
        }
        Group{
        title: qsTr("Other parameters")
            DropDown{
			        name: "theme"
			        values: ["JASP", "Black and white", "Minimal", "Classic", "Dark"]
			        label: qsTr("GGplot theme")
		        }
		      IntegerField{
          name: "nsamp"
          label: qsTr("Number of clusters")
          defaultValue: 3
          min: 1
          max: 20
          enabled: varlist.count > 0
        }   
        }

  }
  
	
  
 
}