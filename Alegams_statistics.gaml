/**
* Name: Alegams_statistics
* Author: Ligte002
* Description: 
* Tags: Tag1, Tag2, TagN
*/



model Alegams_statistics

import "./Alegams_globals.gaml"
import "./Alegams_farm.gaml"
import "./Alegams_plot.gaml"

global{

float avg_HH_Account;
float std_HH_Account;
float std_up_HH_Account;
float std_down_HH_Account;
float min_HH_Account;
float max_HH_Account;
float tot_INT;
float tot_IE;
float tot_IMS;
float tot_INT_IE;
float tot_INT_IMS;
float tot_IE_IMS;

	action calculate_averag_HH_account{
		list<float> HH_account_List <- [];
		std_HH_Account <- 0.0;
		ask farm{
			add HH_Account to:  HH_account_List; 
		}	
		
		avg_HH_Account <- mean(HH_account_List);
		std_HH_Account <- mean_deviation(HH_account_List);
		min_HH_Account <- min(HH_account_List);
		max_HH_Account <- max(HH_account_List);
		std_up_HH_Account <- avg_HH_Account + std_HH_Account;
		std_down_HH_Account <- avg_HH_Account - std_HH_Account;  
	}

	action calculate_tot_areas{
		set tot_INT <- 0.0;
		set tot_IE <- 0.0;
		set tot_IMS <- 0.0;
		ask plot{
			set tot_INT <- tot_INT + area_INT;
			set tot_IE <- tot_IE + area_IE;
			set tot_IMS <- tot_IMS + area_IMS;		
		}		
	}


}





