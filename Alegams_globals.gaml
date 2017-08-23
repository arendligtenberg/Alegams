/**
* Name: alagamsglobals
* Author: ligte002
* Description: 
* Tags: Tag1, Tag2, TagN
*/

model Alegams_globals
 global{
	// gobal vars	
 	
 	//coding of production systems
 	int INT <- 1;
 	int IE <- 2;
 	int IMS <- 3;
 	int INT_IE <- 4;
 	int INT_IMS <- 5;
 	int IE_IMS <- 6;
 	int unKnown <- 999;
 	

 	//init probabilities for initial production systems
 	float Prob_INT <- 0.5;
 	float Prob_IE <- 0.5;
 	float Prob_IMS <- 0.1;
 	float Prob_INT_IE <- 0.1;
 	float Prob_INT_IMS <- 0.3;
 	float Prob_IE_IMS <- 0.1;
 	
 	//max number of Cycles:
 	int max_cycle_INT_mono <- 2;
 	int max_cycle_INT_vana <- 3;
	int max_cycle_IE <- 3;
	int max_cycle_IMS <- 999; 
	
 	//Times to harvest
 	int time_Harvest_INT_mono <- 4 parameter: "time to harvest intensive Monodon (months)" category: "Crop" ;
	int time_Harvest_INT_vana <- 3 parameter: "time to harvest intensive Vannamei (months)" category: "Crop" ;
	int time_Harvest_IE <- 4 parameter: "time to harvest intensive improved extensive (months)" category: "Crop" ;
	int time_Harvest_IMS <- 3 parameter: "time to harvest integrated mangrove (months)" category: "Crop" ;
	
	//In case of disease can harvest after:
 	int time_Harvest_fail_INT_mono <- 2 parameter: "min. req. period toharvest intensive Monodon (months)" category: "Crop" ;
	int time_Harvest_fail_INT_vana <- 1 parameter: "min. req. period toharvest intensive Vannamei (months)" category: "Crop" ;
	int time_Harvest_fail_IE <- 1 parameter: "min. req. period toharvest improved extensive (months)" category: "Crop" ;
	int time_Harvest_fail_IMS <- 2 parameter: "min. req. period toharvest integrated mangrove (months)" category: "Crop" ;

 	// In case Breakeven harvest time due to disease
 	//Parameter to harvest
 	int time_Harvest_breakeven_INT_mono <- 3 parameter: "period have to harvest intensive Monodon (months)" category: "Crop" ;
	int time_Harvest_breakeven_INT_vana <- 2 parameter: "period have to harvest intensive Vannamei (months)" category: "Crop" ;
	int time_Harvest_breakeven_IE <- 2 parameter: "period have to harvest improved extensive (months)" category: "Crop" ;
 	
 	
 	//Risk for disease for the different production systems (per month)
 	//float farmPlotFailureRate_IE <- 0.58.;
 	//float farmPlotFailureRate_INT <- 0.56.;
 	//float farmPlotFailureRate_IMS <- 0.47.;

 	float farmPlotFailureRate_INT <- 0.6 parameter: "chance of disease for intensive" category: "Crop" ; //0.2;
 	float farmPlotFailureRate_IE <- 0.6 parameter: "chance of disease for improved extensive" category: "Crop" ; //0.12;
 	float farmPlotFailureRate_IMS <- 0.3 parameter: "chance of disease for integrated mangrove" category: "Crop" ; //0.1;
	
	//reducing condition for each system
	int fail_time_to_reduce_INT <-2;
	int fail_time_to_reduce_IE <-3;
	
	//probability to shift
	
	float Prob_shift_IE_INT <- 0.05;
 	float Prob_shift_IMS_IE <- 0.05;
 	float Prob_shift_IMS_INT <- 0.01;
 	float Prob_shift_INT_IE <- 0.01;
 	float Prob_shift_INT_IMS <- 0.03;
 	float Prob_shift_IE_IMS <- 0.01;
	
	//crop yields  (kg/ha/cycle) 
	int crop_yield_INT_mono <- 5814 parameter: "crop yields intensive Monodon (kg/ha/cycle)" category: "Crop" ; //5814
	int crop_yield_INT_vana <- 8897	parameter: "crop yields intensive Vannamei (kg/ha/cycle)" category: "Crop" ; //8897	
	int crop_yield_IE <- 568 parameter: "crop yields improved extensive (kg/ha/cycle)" category: "Crop" ;	//568
	int crop_yield_IMS <- 308 parameter: "crop yields integrated mangrove (kg/ha/cycle)" category: "Crop" ; //308	

	//crop yields in case of failure (kg/ha/cycle) 
	int crop_yield_fail_INT_mono <- 1431 parameter: "crop yields if disease intensive Monodon (kg/ha/cycle)" category: "Crop" ;
	int crop_yield_fail_INT_vana <- 2853 parameter: "crop yields if disease intensive Vannamei (kg/ha/cycle)" category: "Crop" ;	 		
	int crop_yield_fail_IE <- 134 parameter: "crop yields if disease improved extensive (kg/ha/cycle)" category: "Crop" ;	
	int crop_yield_fail_IMS <- 87 parameter: "crop yields if disease integrated mangrove (kg/ha/cycle)" category: "Crop" ;	

	//crop yields in case of breakeven harvest time due to disease (kg/ha/cycle) 
	int crop_yield_breakeven_INT_mono <- 1431 parameter: "crop yields if disease intensive Monodon (kg/ha/cycle)" category: "Crop" ;
	int crop_yield_breakeven_INT_vana <- 2853 parameter: "crop yields if disease intensive Vannamei (kg/ha/cycle)" category: "Crop" ;	 		
	int crop_yield_breakeven_IE <- 134 parameter: "crop yields if disease improved extensive (kg/ha/cycle)" category: "Crop" ;	
	int crop_yield_breakeven_IMS <- 87 parameter: "crop yields if disease integrated mangrove (kg/ha/cycle)" category: "Crop" ;			
 
 
	//household related income from aquaculture

	float avg_income <- 150.0; //average income for all systems
	//float avg_income_INT_mono <- 214.0;
	//float avg_income_IE <- 49.7;
	//float avg_income_IMS <- 31.4;

	
	// houshold expenses (Mvdn/person/month	
	//float HH_expense_INT_mono <- 2.1;
	//float HH_expense_INT_vana <- 3.1;
	//float HH_expense_IE <- 1.6;
	//float HH_expense_IMS <- 2.1;
	float HH_expenses_avg <- 2.5 parameter: "average household expenses (mVnd/pp/month)" category: "Farm" ;
	float HH_expenses_extra <- 2.5 parameter: "average household expenses (mVnd/pp/month)" category: "Farm";
	// average loan (Mvnd/ha/year (stdev are estimated)
	//float HH_loan_avg_IE <- 116.00;
	//float HH_loan_std_IE <- 35.00;
	//float HH_loan_avg_INT <- 152.00;
	//float HH_loan_std_INT <- 50.00;
	//float HH_loan_avg_IMS <- 91.00;
	//float HH_loan_std_IMS <- 30.00;
	
 	
	//household related income from other sources
	float HH_2ndincome_avg_INT_mono <-12.7/12 parameter: "average sec. income intensive Monodon(mVnd/month)" category: "Farm" ;
	float HH_2ndincome_stddev_INT_mono <- 50.00/12;
	float HH_2ndincome_avg_INT_vana <- 44.2/12 parameter: "average sec. income intensive Vannamei (mVnd/month)" category: "Farm" ;
	float HH_2ndincome_stddev_INT_vana <- 50.00/12;
	float HH_2ndincome_avg_IE <- 11.5/12 parameter: "average sec. income improved extensive (mVnd/month)" category: "Farm" ;
	float HH_2ndincome_stddev_IE <- 15.00/12; 
	float HH_2ndincome_avg_IMS <- 5.6/12 parameter: "average sec. income integrated mangrove (mVnd/month)" category: "Farm" ;
	float HH_2ndincome_stddev_IMS <- 10.00/12 ;
	//basic pond size
 	float min_INT_size <- 0.1 parameter: "min size of intensive ponds (ha.)" category: "Farm" ;
 	float max_INT_size <- 1.8 parameter: "min size of intensive ponds (ha.)" category: "Farm" ;
 	float min_IE_size <- 0.5 parameter: "min size of improved extensive ponds)" category: "Farm" ;
    float max_IE_size <- 2.0 parameter: "max size of improved extensive ponds)" category: "Farm";
    
    //shifting for new pond
    float shift_INT_size <- rnd(0.3,0.5);
    float shift_IE_size <- rnd(0.5,0.8);
    float shift_IMS_size <- rnd(1.0,1.5);
    
    
    //maximum loan for each system (mvnd/ha)
    int max_loan_INT <- 90;
    int max_loan_IE <-150;
    int max_loan_IMS <-50;

    
	
	//cost to seed new shrimp pond (mVnd/ha)
 	float shrimp_init_INT <- 250.00 parameter: "cost to seed new intensive Monondon(mVnd/ha)" category: "Crop" ;
 	float shrimp_init_IE <- 85.00 parameter: "cost to seed new improved extensive(mVnd/ha)" category: "Crop" ;
 	float shrimp_init_IMS <- 79.00 parameter: "cost to seed new integrated mangrove(mVnd/ha)" category: "Crop" ; 	 
 	 			
	//crop costs (Mvnd/ha/month)
	//float cropcost_avg_INT_mono <- 122.0 parameter: "crop costs intensive Monodon (mVnd/ha/month)" category: "Crop" ;
	//float cropcost_avg_INT_vana <- 218.0 parameter: "crop costs intensive Vanamei (mVnd/ha/month)" category: "Crop" ;
	//float cropcost_stddev_INT_vana <- 0.0;	
	//float cropcost_avg_IE <- 17.00 parameter: "crop costs improved extensive (mVnd/ha/month)" category: "Crop" ;
	//float cropcost_stddev_IE <- 0.0;	
	//float cropcost_avg_IMS <- 3.0 parameter: "crop costs integrated mangrove (mVnd/ha/month)" category: "Crop" ;
	//float cropcost_stddev_IMS <- 0.0;
	
	// maintance cost
 	float mantain_cost_INT <- 42.0 parameter: "mantain crop cost intensive (mVnd/ha/cycle)" category: "Crop" ;
 	float mantain_cost_IE <- 13.8 parameter: "mantain crop cost improve extensive (mVnd/ha/cycle)" category: "Crop" ;
 	float mantain_cost_IMS <- 6.5 parameter: "mantain crop cost integrated mangrove shrimp (mVnd/ha/cycle)" category: "Crop" ;
	
	//shrimpfeed per month (milion/month)
	
	float feedcost_INT_mono <- 70.3;
	float feedcost_INT_vana <- 116.4;
	float feedcost_IE <- 5.7;
	float feedcost_IMS <- 1.3;
	
	
	
 	//cost to feed for 1st month cropping (mvnd/ha)
 	float Cost_1st_month_INT_mono <- 121.0 parameter: "1st crop cost intensive Monodon (mVnd/ha)" category: "Crop" ;
 	float cropcost1st_stddev_INT_mono <- 0.0;
 	float Cost_1st_month_INT_vana <- 240.0 parameter: "1st crop cost intensive Monodon (mVnd/ha)" category: "Crop" ;
 	float cropcost1st_stddev_INT_vana <- 0.0;
 	float Cost_1st_month_IE <- 35.0 parameter: "1st crop cost intensive Monodon (mVnd/ha)" category: "Crop" ;
 	float cropcost1st_stddev_IE <- 0.0;
 	float Cost_1st_month_IMS <- 8.0 parameter: "1st crop cost intensive Monodon (mVnd/ha)" category: "Crop" ;
 	float cropcost1st_stddev_IMS <- 0.0;
 	
 	//cost to feed monthly after 1stmonth cropping
 	float Nomal_cost_INT_mono <- 95.0 parameter: "monthly crop cost intensive Monodon (mVnd/ha/month)" category: "Crop" ;
 	float Nomal_cost_stddev_INT_mono <- 0.0;
 	float Nomal_cost_INT_vana <- 169.0 parameter: "monthly crop cost intensive Vanamei (mVnd/ha/month)" category: "Crop" ;
 	float Nomal_cost_stddev_INT_vana <- 0.0;
 	float Nomal_cost_IE <- 7.0 parameter: "monthly crop cost improve extensive (mVnd/ha/month)" category: "Crop" ;
 	float Nomal_cost_stddev_IE <- 0.0;
 	float Nomal_cost_IMS <- 1.0 parameter: "monthly crop cost intergrated mangrove shrimp (mVnd/ha/month)" category: "Crop" ;
 	float Nomal_cost_stddev_IMS <- 0.0;
 	
	// post larvae (pl/m2) **need costs per ha
//	int PL_avg_IE <- 8; 
//	int PL_avg_INT_mono <- 25; 
//	int PL_avg_INT_vana <- 72; 
//	int PL_avg_IMS <- 3;
	
	
	//post larvae cost
	float Postlarvea_cost_INT_mono <- 26.4;
	float Postlarvea_cost_INT_vana <- 26.4;
	float Postlarvea_cost_IE <- 8.6;
	float Postlarvea_cost_IMS <- 4.5;
	
	//investment cost
	float invest_cost_INT_mono <- 234.03;
	float invest_cost_INT_vana <- 291.41;
	float invest_cost_INT <- 260; //average of mono and vana as these are not distinqjui
	float invest_cost_IE <- 85.0;
	float invest_cost_IMS <- 79.05;
	
	
	
	//shrimp prices Mvnc/kg
	float shrimp_price_INT_mono <- 0.25 parameter: "shrimp price intensive Monodon (mVnd/kg)" category: "Market" ;
	float shrimp_price_INT_vana <- 0.12 parameter: "shrimp price intensive Vannamei (mVnd/kg)" category: "Market" ;
	float shrimp_price_IE <- 0.25 parameter: "shrimp price improved extensive (mVnd/kg)" category: "Market" ;
	float shrimp_price_IMS <- 0.25 parameter: "shrimp price integrated mangrove (mVnd/kg)" category: "Market" ;
	
	


	
 	file plot_file <- file ('../includes/LongVinhProvinceCorrectFinal2.shp');
 	}
 	


