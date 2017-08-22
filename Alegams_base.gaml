/**
* 
* Author: Arend, Hiêp
* Description: first version of update ABM for the Alegams project
* To be presented during the Vietnam workshop second half oktober 2016
* August 2016
* TBD 
* adding decision making behaviour according flow diagram
*/



model Alegams_base


import "./Alegams_globals.gaml"
import "./Alegams_farm.gaml"
import "./Alegams_plot.gaml"
import "./Alegams_statistics.gaml"

global{
	
	
	
	geometry shape <- envelope(plot_file);

	init{
	
	// create plot though GIS file
	
		create plot from: plot_file with: 	
		[
		plot_Id::int(read("OBJECTID")),
		tot_Area::(float(read("Shape_Area")))/10000,
		LU_model::int(read("LU_model")),
		LU_local::int(read("LU_local")),
		LU_office::string(read("LU_Office")),
		LU_cad::string(read("LU_Cad1"))
			
		]{		}

	//create a farm on each plot with a shrimp farm
		ask plot{
			
			if self.production_System != 999{
					//write "Creating farmer";
					create farm number:1 {
						set farmPlot <- myself;
						set name <- "Schrimpfarmer_"+farmPlot.plot_Id;
						set farmPlot.name <- "plot of: "+name;
						location <- any_location_in(farmPlot);
					}		
				}
		}


	}//end init
	
	reflex output_Statistics{
		do calculate_averag_HH_account;
		do calculate_tot_areas;
		
	}
	
} //end global section


//Species section


	
//experiment section 
		
experiment alegams type: gui {
	parameter "Plot file" var: plot_file category: "GIS" ;

	output{
		display map_display {
			species plot aspect: base;
			species farm aspect: default;
		}
	
		display HH_Account {
			chart "Average saldo " type: series background: rgb ('white') size: {1,0.5} position: {0,0}{
		 	data "AVG Saldo" value: avg_HH_Account color: rgb ('red');
		 	data "Total Area INT" value: tot_INT color: rgb (128, 0, 255,255);
		 	data "Total Area IE" value: tot_IE color: rgb (255, 128, 128,255);
		 	data "Total Area IMS" value: tot_IMS color: #green;
		 	
		 	//data "std +" value: std_up_HH_Account color: rgb ('green');
		 	//data "std -" value: std_down_HH_Account color: rgb ('green');		 	
			}
		}
		monitor "Average saldo" value: avg_HH_Account refresh:every(1);
		monitor "STD dev saldo" value: std_HH_Account refresh:every(1);			
		monitor "Max saldo" value: max_HH_Account refresh:every(1);
		monitor "Min dev saldo" value: min_HH_Account refresh:every(1);		
		monitor "Total Area INT" value: tot_INT refresh:every(1);
		monitor "Total Area IE" value: tot_IE refresh:every(1);
		monitor "Total Area IMS" value: tot_IMS refresh:every(1);
		
		
	}
}	


