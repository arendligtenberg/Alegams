//				 if grow_Time_INT= time_Harvest_breakeven_INT_mono and cycle_INT <= max_cycle_INT_mono //in case the farm can be harvest at that moment and farming crop time is less than or equal to maximum number of cycle intensive
//				 {
//				 	farmPlot.yield_INT_mono <- crop_yield_breakeven_INT_mono * farmPlot.area_INT;//yield incase of disease
//				 	income_from_INT_mono <-  farmPlot.yield_INT_mono * shrimp_price_INT_mono;//income from intensive is based on shrimp price and yield;
//				 	if grow_Time_INT >= time_Harvest_breakeven_INT_mono {cycle_INT <- cycle_INT +1;}// if grow time higher than breakeven time in intensive mono the system production cropping time  is added 1 into intensive cycle;
//				 }
//				 if grow_Time_INT>= time_Harvest_INT_mono and cycle_INT <= max_cycle_INT_mono //in case the farm can be harvest at that moment and farming crop time is less than or equal to maximum number of cycle intensive
//				 {//incase of the farm was farming until sucessful time
//				 	farmPlot.yield_INT_mono <- crop_yield_INT_mono * farmPlot.area_INT;//yield incase of disease
//				 	income_from_INT_mono <-  farmPlot.yield_INT_mono * shrimp_price_INT_mono;//income from intensive is based on shrimp price and yield;
//				 	if grow_Time_INT >= time_Harvest_breakeven_INT_mono {cycle_INT <- cycle_INT +1;}// if grow time is harvest time in intensive mono the system production cropping time  is added 1 into intensive cycle;
//				 }
//		 and disease_IE
//		{
//			if grow_Time_IE >= time_Harvest_fail_IE and cycle_IE < max_cycle_IE{
//				set farmPlot.yield_IE <- crop_yield_fail_IE * farmPlot.area_IE;
//				set income_from_IE <- farmPlot.yield_IE * shrimp_price_IE;
//				set grow_Time_IE <- 0;
//				set cycle_IE <- cycle_IE + 1;
//			} else{
//				set farmPlot.yield_IE <- 0.0;	
//				set grow_Time_IE <- 0;
//			}
//
//		}

//		//IMS
//		if farmPlot.area_IMS > 0 and disease_IMS{
//			if grow_Time_IMS >= time_Harvest_fail_IMS{
//				set grow_Time_IMS <- 0;
//				set farmPlot.yield_IMS <- crop_yield_fail_IMS * farmPlot.area_IMS;
//				set income_from_IMS <-  farmPlot.yield_INT_mono * shrimp_price_IMS;
//			} else{
//				set grow_Time_IMS <- 0;
//				set farmPlot.yield_IMS <- 0.0;
//			}
//
//		}
//
//		//in case of regular harvest
//
//		//INT
//		if farmPlot.area_INT > 0{
//			if (farmPlot.shrimp_Type = 1){			
//				if (grow_Time_INT >= time_Harvest_INT_mono) and (cycle_INT < max_cycle_INT_mono){
//					//write "...harvesting from intensiver monodon";
//					set farmPlot.yield_INT_mono <- crop_yield_INT_mono * farmPlot.area_INT;
//					set income_from_INT_mono <- farmPlot.yield_INT_mono * shrimp_price_INT_mono;
//					set cycle_INT <- cycle_INT + 1;
//					set grow_Time_INT <- 0;
//				}
//			}
//			
//			if (farmPlot.shrimp_Type = 2){				
//				if (grow_Time_INT >= time_Harvest_INT_vana) and (cycle_INT < max_cycle_INT_vana){
//					//write "...harvesting from intensive vanamei";					
//					set farmPlot.yield_INT_vana <- crop_yield_INT_vana * farmPlot.area_INT;
//					set income_from_INT_vana <- farmPlot.yield_INT_vana * shrimp_price_INT_vana;
//					set cycle_INT <- cycle_INT + 1;
//					set grow_Time_INT <- 0;
//				}  
//			}
//		}
//		
//		
//		//IE
//		if farmPlot.area_IE > 0	{
//			if grow_Time_IE >= time_Harvest_IE and (cycle_IE < max_cycle_IE){
//				//write "...harvesting from improved extensive";				
//				set farmPlot.yield_IE <- crop_yield_IE * farmPlot.area_IE;
//				set income_from_IE <- farmPlot.yield_IE * shrimp_price_IE;
//				set cycle_INT <- cycle_IE + 1;
//				set grow_Time_IE <- 0;
//			}
//
//		}
//
//		//IMS
//		if farmPlot.area_IMS > 0
//		{
//			if grow_Time_IMS >= time_Harvest_IMS
//			{
//			//write "...harvesting from integrated monodon";				
//				set grow_Time_IMS <- 0;
//				set farmPlot.yield_IMS <- crop_yield_IMS * farmPlot.area_IMS;
//				set income_from_IMS <- farmPlot.yield_IMS * shrimp_price_IMS;
//				set cycle_IMS <- cycle_IMS + 1;
//				set grow_Time_IMS <- 0;
//				
//				
}

}