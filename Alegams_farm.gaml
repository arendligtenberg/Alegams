/**
* Name: Alemans_farm
* Author: Ligte002
* Description: 
* Tags: Tag1, Tag2, TagN
*/
model Alemans_farm

import "./Alegams_globals.gaml"
import "./Alegams_base.gaml"
species farm
{
//declaration of the farm characteristics
	plot farmPlot;
	list plot ;
	int plotId;
	int nr_Plots;
	int hh_Size;
	float HH_Account;
	float second_Income;
	float bank_Loan;
	int extra_Loan;
	float max_Loan;
	int age; //years
	float interest_Bank;
	float interest_Commercial;
	int nr_Labour;
	int prob_Shift;
	int time; //month
	int grow_Time_INT; //month
	int grow_Time_IE; //month
	int grow_Time_IMS; //month
	int INT_fail_time;
	int IE_fail_time;
	int INT_sucess_time;
	int IE_sucess_time;
	int cycle_INT;
	int cycle_IE;
	int cycle_IMS;
	bool disease_INT;
	bool disease_IE;
	bool disease_IMS;
	bool reduce_INT;
	bool reduce_IE;
	bool INT_shift;
	bool shift_INT_IMS;
	bool shift_INT_IE;
	bool shift_IE_IMS;
	bool shift_IE_INT;
	bool shift_IMS_INT;
	bool shift_IMS_IE;
	bool abandon_INT;
	float crop_cost;
	float income_from_INT_mono;
	float income_from_INT_vana;
	float income_from_IE;
	float income_from_IMS;
	float second_income;
	
	init
	{
		hh_Size <- rnd(1, 5);
		age <- rnd(22, 70);
		time <- 1;
		grow_Time_INT <- rnd(0, time_Harvest_INT_mono);
		grow_Time_IE <- rnd(0, time_Harvest_IE);
		grow_Time_IMS <- rnd(0, time_Harvest_IMS);
		cycle_INT <- rnd(0, max_cycle_INT_mono);
		cycle_INT <- rnd(0, max_cycle_INT_vana);		
		cycle_IE <- rnd(0, max_cycle_IE);
		cycle_IMS <- rnd (0, max_cycle_IMS);				
		bank_Loan <- rnd(50.0, 100.0);
		HH_Account <- rnd((-1 * bank_Loan), (avg_income));
		farmPlot.Neighbour <- int((plot) at_distance 100);// when: farmPlot.area_INT > 0;
		grow_Time_IMS<-0; //month
		INT_fail_time<-0;
		IE_fail_time<-0;
		INT_sucess_time<-0;
		IE_sucess_time<-0;
	}

	reflex grow_Shrimp
	{
	//reset disease indicators
		disease_INT <- false;
		disease_IE <- false;
		disease_IMS <- false;
	// reset reduce
		reduce_INT <- false;
		reduce_IE <- false;
	//reset yield and income
		farmPlot.yield_INT_mono <- 0.0;
		farmPlot.yield_INT_vana <- 0.0;
		farmPlot.yield_IE <- 0.0;
		farmPlot.yield_IMS <- 0.0;
		income_from_INT_mono <- 0.0;
		income_from_INT_vana <- 0.0;
		income_from_IE <- 0.0;
		income_from_IMS <- 0.0;
		second_income <- 0.0;
		
	 //month
		INT_fail_time <- INT_fail_time;
		IE_fail_time<- IE_fail_time;
		INT_sucess_time<-INT_sucess_time;
		IE_sucess_time<-IE_sucess_time;
		if time = 12{
			time <- 0;
			cycle_INT <- 0;
			cycle_IE <- 0;
			cycle_IMS <- 0;
			
			INT_fail_time <-0;
			IE_fail_time <-0;
			INT_sucess_time <-0;
			IE_sucess_time <-0;
		}
		
		

		//reset crop costs
		crop_cost <- 0.0;
		do calc_crop_costs;
		do calc_second_income;
		//start farming actions
		do check_for_disease;
		do check_for_harvest;
		do update_loan_and_bank;
		do cal_reduce_crop;
		do reduce_crop;
		do calc_ability_to_shift;
		do shifting;
		do abandon;
		
		

		//update timers for every cycle
		time <- time + 1;
		grow_Time_INT <- grow_Time_INT + 1;
		grow_Time_IE <- grow_Time_IE + 1;
		grow_Time_IMS <- grow_Time_IMS + 1;
	}
// reflex grow time <- 0 for the systems which has no no area for farming
	reflex reset_Int_time when: farmPlot.area_INT = 0
	{
		grow_Time_INT <- 0;
	}

	reflex reset_IE_time when: farmPlot.area_IE = 0
	{
		grow_Time_IE <- 0;
	}

	reflex reset_IMS_time when: farmPlot.area_IMS = 0
	{
		grow_Time_IMS <- 0;
	}
	
	//calculate cost of maintaining crop based on  area of different farming systems
	// cop cost is calculated based on Standard distribution.
	action calc_crop_costs
	{

		float int_cost <- 0.0;
		float ie_cost <- 0.0;
		float ims_cost <- 0.0;
		if farmPlot.shrimp_Type =1
		{
			if grow_Time_INT <=1 
			{
				int_cost <- gauss({ Cost_1st_month_INT_mono, cropcost1st_stddev_INT_mono }) * farmPlot.area_INT;//crop cost in the first month for intensive farm with monodon;
			}else{int_cost <- gauss({ Nomal_cost_INT_mono, Nomal_cost_stddev_INT_mono }) * farmPlot.area_INT;}//crop cost in the month following 1st month for intensive farm with monodon;
		}
			if farmPlot.shrimp_Type =2  
		{
			if grow_Time_INT <=1 
			{
				int_cost <- gauss({ Cost_1st_month_INT_vana, cropcost1st_stddev_INT_vana }) * farmPlot.area_INT;//crop cost in the first month for intensive farm with vanamei;
			}else{int_cost <- gauss({ Nomal_cost_INT_vana, Nomal_cost_stddev_INT_vana }) * farmPlot.area_INT;}//crop cost in the month following 1st month for intensive farm with vanamei;
		}
		if grow_Time_IE <=1
		{
			ie_cost <- gauss({ Cost_1st_month_IE, cropcost1st_stddev_IE }) * farmPlot.area_IE;//crop cost in the first month for improve extensive farm;
		}else{ie_cost <- gauss({ Nomal_cost_IE, Nomal_cost_stddev_IE }) * farmPlot.area_IE;}//crop cost in the month following 1st month fo improve extensive farm;
		if grow_Time_IMS <= 1
		{
			ims_cost <- gauss({ Cost_1st_month_IMS, cropcost1st_stddev_IMS }) * farmPlot.area_IMS;//crop cost in the first month for integrated mangrove shrimp farm;
		} else{ims_cost <- gauss({ Nomal_cost_IMS, Nomal_cost_stddev_IMS }) * farmPlot.area_IMS;}//crop cost in the month following 1st month for integrated mangrove shrimp farm;
		// calculate total crop cost. If in this month new shrimps are seeded than the montly cost are added to the 
		// seed costs (which is the case when harvest has occured) 
		
			//write "---" + crop_cost;
		crop_cost <- crop_cost + int_cost + ie_cost + ims_cost;//crop cost is calculate by summing all cost between  amount  of intensive cost, improve extensive and integrated mangrove shrimp
			//write "int: " + int_cost;
			//write "ie: " + ie_cost;
			//write "ims: " + ims_cost;
			//write "cropcost: " + crop_cost;
			//write "=======================";
	}//end of action calc_crop_costs
//========

// this action is used to calculate the income from other sources, the income will be differnces each systems and based on Standard distribution and house hold size;

	action calc_second_income
	{
		float int_second <- 0.0;
		float ie_second <- 0.0;
		float ims_second <- 0.0;
		if farmPlot.area_INT > 0
		{
			if farmPlot.shrimp_Type = 1
			{
				int_second <- gauss({ HH_2ndincome_avg_INT_mono, HH_2ndincome_stddev_INT_mono }) * hh_Size;
			} else if farmPlot.shrimp_Type = 2 {
				int_second <- gauss({ HH_2ndincome_avg_INT_vana, HH_2ndincome_stddev_INT_vana }) * hh_Size;
			}
		}
//calculate second income from IE
		if farmPlot.area_IE > 0
		{
			ie_second <- gauss({ HH_2ndincome_avg_IE, HH_2ndincome_stddev_IE }) * hh_Size;
		}
		if farmPlot.area_IMS > 0
		{
			ims_second <- gauss({ HH_2ndincome_avg_IMS, HH_2ndincome_stddev_IMS }) * hh_Size;
		}
		second_Income <- int_second + ie_second + ims_second;
	}

//this action to check if disease occur in the farm. There are 3 type of shrimp farming in a farm. If disease occur model check for harvest in disease case
	action check_for_disease
	{
	//TODO: need to check probabilities in globals. Currently extreme high!	
	//TODO currently probabilities are non-conditional: is this ok?
		if flip(farmPlotFailureRate_INT)
		{
			disease_INT <- true;
		}else {disease_INT <- false;}

		if flip(farmPlotFailureRate_IE)
		{
			disease_IE <- true;
		}else {disease_IE <- false;}

		if flip(farmPlotFailureRate_IMS)
		{
			disease_IMS <- true;
		}else {disease_IMS <- false;}
	}//end of action checking for disease 
// this action is to check for harvest. there are harvest in case of disease and no disease
	action check_for_harvest
	{
	//checking harvest in intensive pond;
	if farmPlot.area_INT > 0 
	{//incase of disease
		if disease_INT 
		{ 	
			if (farmPlot.shrimp_Type = 1)//shrimp type is MONODON
			{	//in case the farm can be harvest at that moment and farming crop time is less than or equal to maximum number of cycle intensive
				if grow_Time_INT<= time_Harvest_fail_INT_mono and cycle_INT <= max_cycle_INT_mono 
				 {
				 	farmPlot.yield_INT_mono <- crop_yield_fail_INT_mono * farmPlot.area_INT;//yield incase of disease
				 	income_from_INT_mono <-  farmPlot.yield_INT_mono * shrimp_price_INT_mono;//income from intensive is based on shrimp price and yield;
				 	if grow_Time_INT > time_Harvest_breakeven_INT_mono {cycle_INT <- cycle_INT +1;}// if grow time higher than 1 the system production cropping time  is added 1 into intensive cycle;
					grow_Time_INT <- 0;
					INT_fail_time <- INT_fail_time +1;
					}
			//incase of the farm was farming until breakeven time
				 if grow_Time_INT= time_Harvest_breakeven_INT_mono and cycle_INT <= max_cycle_INT_mono //in case the farm can be harvest at that moment and farming crop time is less than or equal to maximum number of cycle intensive
				 {
				 	farmPlot.yield_INT_mono <- crop_yield_breakeven_INT_mono * farmPlot.area_INT;//yield incase of disease
				 	income_from_INT_mono <-  farmPlot.yield_INT_mono * shrimp_price_INT_mono;//income from intensive is based on shrimp price and yield;
				 	if grow_Time_INT >= time_Harvest_breakeven_INT_mono {
				 		cycle_INT <- cycle_INT +1;
				 	}// if grow time higher than breakeven time in intensive mono the system production cropping time  is added 1 into intensive cycle;
				 	grow_Time_INT <- 0;
				 	INT_fail_time <- INT_fail_time +1;
				 }
				 if grow_Time_INT>= time_Harvest_INT_mono and cycle_INT <= max_cycle_INT_mono //in case the farm can be harvest at that moment and farming crop time is less than or equal to maximum number of cycle intensive
				 {//incase of the farm was farming until sucessful time
				 	farmPlot.yield_INT_mono <- crop_yield_INT_mono * farmPlot.area_INT;//yield incase of disease
				 	income_from_INT_mono <-  farmPlot.yield_INT_mono * shrimp_price_INT_mono*80/100;//income from intensive is based on shrimp price and yield;
				 		if grow_Time_INT >= time_Harvest_breakeven_INT_mono {cycle_INT <- cycle_INT +1;}// if grow time is harvest time in intensive mono the system production cropping time  is added 1 into intensive cycle;
				 	grow_Time_INT <- 0;
					INT_fail_time <- INT_fail_time +1;
				 }
				 
			}
			else{ //shrimp type is VANAMEI
				if grow_Time_INT= time_Harvest_fail_INT_vana and cycle_INT <= max_cycle_INT_vana //in case the farm can be harvest at that moment and farming crop time is less than or equal to maximum number of cycle intensive
				 {
				 	farmPlot.yield_INT_vana <- crop_yield_fail_INT_vana * farmPlot.area_INT;//yield incase of disease
				 	income_from_INT_vana <-  farmPlot.yield_INT_vana * shrimp_price_INT_vana;//income from intensive is based on shrimp price and yield;
				 	if grow_Time_INT >= time_Harvest_breakeven_INT_vana {cycle_INT <- cycle_INT +1;}// if grow time higher than 1 the system production cropping time  is added 1 into intensive cycle;
				 	grow_Time_INT <- 0;
				 	INT_fail_time <- INT_fail_time +1;
				 }
				 if grow_Time_INT= time_Harvest_breakeven_INT_vana and cycle_INT <= max_cycle_INT_vana //in case the farm can be harvest at that moment and farming crop time is less than or equal to maximum number of cycle intensive
				 {//incase of the farm was farming until breakeven time
				 	farmPlot.yield_INT_vana <- crop_yield_breakeven_INT_vana * farmPlot.area_INT;//yield incase of disease
				 	income_from_INT_vana <-  farmPlot.yield_INT_vana * shrimp_price_INT_vana;//income from intensive is based on shrimp price and yield;
				 	if grow_Time_INT >= time_Harvest_breakeven_INT_vana {cycle_INT <- cycle_INT +1;}// if grow time higher than breakeven time in intensive vanamei the system production cropping time  is added 1 into intensive cycle;
					grow_Time_INT <- 0;
					INT_fail_time <- INT_fail_time +1;
				 }
				 if grow_Time_INT>= time_Harvest_INT_vana and cycle_INT <= max_cycle_INT_vana //in case the farm can be harvest at that moment and farming crop time is less than or equal to maximum number of cycle intensive
				 {//incase of the farm was farming until harvest time
				 	farmPlot.yield_INT_vana <- crop_yield_INT_vana * farmPlot.area_INT;//yield incase of disease
				 	income_from_INT_vana <-  farmPlot.yield_INT_vana * shrimp_price_INT_vana;//income from intensive is based on shrimp price and yield;
				 	if grow_Time_INT >= time_Harvest_breakeven_INT_vana {cycle_INT <- cycle_INT +1;}// if grow time is harvest time in intensive mono the system production cropping time  is added 1 into intensive cycle;
				 	grow_Time_INT <- 0;
				 	INT_fail_time <- INT_fail_time +1;
				 }
			}//end of check for VANAMEI in disease case
			//check for reduce after disease occur in the farm
		}
		else{//check for harvest incase of no disease
			///in case of monodon
				if farmPlot.shrimp_Type = 1
				{
					if grow_Time_INT < time_Harvest_INT_mono 
					{// incase farm can not be harvest
						farmPlot.yield_IE <- 0.0;
						income_from_INT_mono <- 0.0;
						grow_Time_INT <- grow_Time_INT +1;//model will check time for harvest in the next time step
					}
					else{// incase farm can be harvest
						farmPlot.yield_INT_mono <- crop_yield_INT_mono * farmPlot.area_INT;
				 		income_from_INT_mono <-  farmPlot.yield_INT_mono * shrimp_price_INT_mono;
				 		cycle_INT <- cycle_INT +1;
				 		grow_Time_INT <- 0;
				 		INT_sucess_time <- INT_sucess_time +1;
				 	}		
				}
				else{
					//incase of vanamei
					if grow_Time_INT < time_Harvest_INT_vana
					{// incase farm can not be harvest
						farmPlot.yield_IE <- 0.0;
						income_from_INT_vana <- 0.0;
						grow_Time_INT <- grow_Time_INT +1;//model will check time for harvest in the next time step
					}
					else{// incase farm can be harvest
						farmPlot.yield_INT_vana <- crop_yield_INT_vana * farmPlot.area_INT;
						income_from_INT_vana <-  farmPlot.yield_INT_vana * shrimp_price_INT_mono;
						cycle_INT <- cycle_INT +1;
					 	grow_Time_INT <- 0;
						INT_sucess_time <- INT_sucess_time +1;
					}//end of checking harvest in case of no disease
				}
		}//end of checking harvest in intensive pond
	} // END area_INT>0
//check for harvest in improve extensive pond
	if farmPlot.area_IE > 0
	{// incase of disease
		if disease_IE
		{//in case of the farm get disease when the farm can not be harvest at that moment
			if grow_Time_IE <= time_Harvest_fail_IE and cycle_IE <= max_cycle_IE
			{
				farmPlot.yield_IE <- crop_yield_fail_IE * farmPlot.area_IE;
				income_from_IE <-  farmPlot.yield_IE * shrimp_price_IE;
				grow_Time_IE <- 0;
				IE_fail_time <- IE_fail_time +1;
			}
			//incase of the farm was farming until breakeven time
			if grow_Time_IE = time_Harvest_breakeven_IE and cycle_IE <= max_cycle_IE 
			{
			 	farmPlot.yield_IE <- crop_yield_breakeven_IE * farmPlot.area_IE;
			 	income_from_IE <-  farmPlot.yield_IE * shrimp_price_IE;
			 	if grow_Time_IE >= time_Harvest_breakeven_IE {cycle_IE <- cycle_IE +1;}
				grow_Time_IE <- 0;
				IE_fail_time <- IE_fail_time +1;
			}
			if grow_Time_IE >= time_Harvest_IE and cycle_IE <= max_cycle_IE 
			{
			 	farmPlot.yield_IE <- crop_yield_IE * farmPlot.area_IE;
			 	income_from_IE <-  farmPlot.yield_IE * shrimp_price_IE;
			 	if grow_Time_IE >= time_Harvest_IE {cycle_IE <- cycle_IE +1;}
				grow_Time_IE <- 0;
				IE_fail_time <- IE_fail_time +1;
			}
		}//end of checking in case of disease in improve extensive pond
		else{//in case of no disease
			if grow_Time_IE < time_Harvest_IE and cycle_IE <= max_cycle_IE 
				{
				 	income_from_IE <-  0.0;
					grow_Time_IE <- grow_Time_IE +1;
				}else{
					farmPlot.yield_IE <- crop_yield_IE * farmPlot.area_IE;
				 	income_from_IE <-  farmPlot.yield_IE * shrimp_price_IE;
					cycle_IE <- cycle_IE +1;
					IE_sucess_time <- IE_sucess_time +1;
				}
		}//end of checking in case of no disease in improve extensive pond
	}//end of checking harvest in improve extensive pond
	if farmPlot.area_IMS > 0
	{// incase of disease
		if disease_IMS
		{//in case of the farm get disease when the farm can not be harvest at that moment
			if grow_Time_IMS<= time_Harvest_fail_IMS
			{
				farmPlot.yield_IMS <- crop_yield_fail_IMS * farmPlot.area_IMS;
				income_from_IMS <-  farmPlot.yield_IMS * shrimp_price_IMS;
				grow_Time_IMS <- 0;
			}
			else{
			 	farmPlot.yield_IMS <- crop_yield_IMS * farmPlot.area_IMS;
			 	income_from_IMS <-  farmPlot.yield_IMS * shrimp_price_IMS;
				grow_Time_IMS <- 0;
			}
		}//end of checking in case of disease
		else{//in case of no disease
			if grow_Time_IMS < time_Harvest_IMS
			{
				income_from_IMS <-  0.0;
				grow_Time_IMS <- grow_Time_IMS +1;
			}else{
				farmPlot.yield_IMS <- crop_yield_IMS * farmPlot.area_IMS;
			 	income_from_IMS <- farmPlot.yield_IMS * shrimp_price_IMS;
			 }
		}//end of checking in case of no disease		
	}//end of checking harvest inintegrated mangrove shrimp pond
		//TODO: improve efficiency of code	
		//we need to seed new shrimp for the systems that were harvested
//		do update_loan_and_bank;	
//		do shifting;
//		do seed_new_shrimp;
} //end of action check_for_harvest
//
	action update_loan_and_bank
	{
	//update bankaccount and loans based on cropcosts, yield and second income for each month
	//suggestion is to skip calucalating loans explicitely but consider a negative bankaccount as a loan.
	//this saves us a lot of hassle
	//recalculate balance based on costs and income
		let hhcost <- hh_Size * HH_expenses_avg;
		set HH_Account <- (HH_Account - hhcost - crop_cost + second_Income + income_from_INT_mono + income_from_INT_vana + income_from_IE + income_from_IMS);
	}
//	
	action cal_reduce_crop 
	{
		//reduce in intensive pond
		//farmsize of one production system can be reduce when the farm have many failure times but they dont have any sucessful time in the crop
		// reducing appear in case of IE and INT, no reduce in case of IMS
			if (INT_fail_time >= fail_time_to_reduce_INT)//INT reduce case requirement
			and (INT_sucess_time = 0) 
			and (HH_Account < Cost_1st_month_INT_mono*farmPlot.area_INT) 
			and (farmPlot.area_IMS = 0.0)
			{
				if flip(0.9){  // 0.9 will be replaced by a parameter
					reduce_INT <- true;	
				}		
			}else{
				reduce_INT <- false;
				}
			//reduce IE
			if (IE_fail_time >= fail_time_to_reduce_IE)
			and (IE_sucess_time = 0)
			and (HH_Account < Cost_1st_month_IE*farmPlot.area_IE)
			and farmPlot.area_IMS > 0.0
			{
				if flip(0.9){ 
					reduce_IE <- true;
				}
			}else{
				reduce_IE <- false;
			}	
	}//end of action calculation to reduce crop in case disease
	
	action reduce_crop
	{
		if reduce_INT
		{
			if( farmPlot.area_INT- farmPlot.area_INT * 0.5)> min_INT_size 
			{
				farmPlot.area_INT <- farmPlot.area_INT- farmPlot.area_INT * 0.5;
			}else{
				farmPlot.area_INT <- min_INT_size;
			}
		}//end of reduceINT
		
		//reduce_IE
		if reduce_IE
		{
			if( farmPlot.area_IE- farmPlot.area_IE * 0.5)> min_IE_size 
			{
				farmPlot.area_IE <- farmPlot.area_IE- farmPlot.area_IE * 0.5;
			}else{
				farmPlot.area_IE <- min_IE_size;
			}
		}
	}// end of action reduce crop
	
	action calc_ability_to_shift
	{
	//shifting of INT
		if farmPlot.area_INT >0
		and INT_fail_time>=3 
		and INT_sucess_time=0 
		and (HH_Account > Cost_1st_month_INT_mono*farmPlot.area_INT)
		and reduce_IE = false
		and reduce_INT = false
		
		{
			if farmPlot.area_IMS >0
			{
				if farmPlot.area_INT > 0.5
				{
					if flip(Prob_shift_INT_IMS){
						shift_INT_IMS <- true;	
					}
				}else{
					if flip(0.9){
						abandon_INT <- true;
						}
				}
			}else{
				if farmPlot.area_INT > 0.5
				{
					if flip(Prob_shift_INT_IE){
						shift_INT_IE <- true;
						}
					
				}else{
					if flip(0.9){
						abandon_INT <- true;
						}
				}
			}
		}else{
				//nochange
		}
	//shifting of IE	
		if farmPlot.area_IE >0
		{
			 //INT_Neibour_sucess and
			if IE_sucess_time >3 
			and farmPlot.Neighbour>10
			and HH_Account> invest_cost_INT_mono + Cost_1st_month_INT_mono
			and reduce_IE = false
			and reduce_INT = false
			{
				if flip(Prob_shift_IE_INT)
				{
					shift_IE_INT <-true;//add probability for shiftinh ratio in farm plot scale
				}
			}else{
				if IE_fail_time >3 and farmPlot.area_IMS > 0
				{
					shift_IE_IMS <-true;
				}else{
								//nochange
				}
			}
		}else{
				//nochange
		}
	//shifting of IMS	
		if HH_Account > invest_cost_INT_mono and //neibour sucess
			farmPlot.area_IMS > 0
		//and farmPlot.LU_office != "Protection  forest"
		{
			if flip(Prob_shift_IMS_INT)
			{
				shift_IMS_INT <- true;
			}
		}else{
				//nochange
		}
	}//end of action calculation ability to shift
	
	//
	action shifting
	{
		if shift_INT_IE = true
		{
			farmPlot.area_IE <- farmPlot.area_IE +farmPlot.area_INT;
			farmPlot.area_INT <- 0.0;
			shift_INT_IE <- false;
		}

		if shift_INT_IMS = true
		{
			farmPlot.area_IMS <- farmPlot.area_IMS + farmPlot.area_INT;
			farmPlot.area_INT <- 0.0;
			shift_INT_IMS <- false;
		}
		
		if shift_IE_IMS =true
		{
			farmPlot.area_IMS <- farmPlot.area_IMS +farmPlot.area_IE;
			farmPlot.area_IE <- 0.0;
			shift_IE_IMS <- false;
		}
		
		if shift_IE_INT = true
		{
			if (farmPlot.area_IE - shift_INT_size)> 0
			{
				farmPlot.area_INT <- farmPlot.area_INT + shift_INT_size;
				farmPlot.area_IE <- farmPlot.area_IE - shift_INT_size; 
				shift_IE_INT <- false;
			}else
			{
				//no change
			}
		}
		
		if shift_IMS_INT = true
		{
			if ((farmPlot.area_IMS - shift_INT_size >0.0)
			and (farmPlot.area_IMS-farmPlot.tot_Area*0.3 >0.0))
			{
				farmPlot.area_INT <- farmPlot.area_INT + shift_INT_size;
				farmPlot.area_IMS <- farmPlot.area_IMS - shift_INT_size;
				shift_IMS_INT <- false;	
			}
		}
		
		if shift_IMS_IE = true
		{
			if ((farmPlot.area_IMS - shift_IE_size)>0.0 
			and (farmPlot.area_IMS - farmPlot.tot_Area*0.3 >0.0))
			{
				farmPlot.area_IE <- farmPlot.area_IE + shift_IE_size;
				farmPlot.area_IMS <- farmPlot.area_IMS - shift_INT_size;
				shift_IMS_IE <- false;	
			}
		}
	}//end of action shifting to another systems
//

	action abandon
	{
		if abandon_INT =true
		{
			farmPlot.area_INT <-0.0;
			crop_cost <- 0.0;
		}
	}

	aspect default
	{
		draw square(25) color: rgb('white');
	}
} //farm

