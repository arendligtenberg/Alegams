/**
* Name: Alegams_depreciated
* Author: Ligte002
* Description: 
* Tags: Tag1, Tag2, TagN
*/

model Alegams_depreciated

	action init_bankAccount
	{
	//intialize the amount of money  on the bank account(savings and loan) of a farmer as a function of the cropcost
		write "...calculate savings and loan";
		float loan_INT <- 0.0;
		float loan_IE <- 0.0;
		float loan_IMS <- 0.0;
		if farmPlot.area_INT > 0
		{
			loan_INT <- (gauss({ HH_loan_avg_INT, HH_loan_std_INT })) * (farmPlot.tot_Area / farmPlot.area_INT);
		}

		if farmPlot.area_IE > 0
		{
			loan_IE <- (gauss({ HH_loan_avg_IE, HH_loan_std_IE })) * (farmPlot.tot_Area / farmPlot.area_IE);
		}

		if farmPlot.area_IMS > 0
		{
			loan_IMS <- (gauss({ HH_loan_avg_IMS, HH_loan_std_IMS })) * (farmPlot.tot_Area / farmPlot.area_IMS);
		}

		set bank_Loan <- loan_INT + loan_IE + loan_IMS;
		let savings <- (crop_cost * rnd(0.0, 2.0));
		set bank_Account <- bank_Loan + savings;
	}
	
		action update_loan_and_bank
	{
	//update bankaccount and loans based on cropcosts, yield and second income for each month
	
	//suggestion is to skip calucalating loans explicitely but consider a negative bankaccount as a loan.
	//this saves us a lot of hassle
	
		//recalculate balance based on costs and income
		set bank_Account <- bank_Account - crop_cost + second_Income + income_from_mono + income_from_vana ;
		
		//if bankaccount is more than needed for cropcost + hh_expenses + reseedcost then loan
		//is payed back from the surplus
		
		let reseed_cost <- (farmPlot.area_INT * shrimp_init_INT_mono) + (farmPlot.area_IE * shrimp_init_IE)+(farmPlot.area_IMS * shrimp_init_IMS);
		let amount_needed_next_month <- (HH_expenses_avg * hh_Size) + crop_cost + reseed_cost;
		let surplus <- bank_Account - amount_needed_next_month;
		
		if surplus > bank_Loan{
			set bank_Account <- bank_Account - bank_Loan;
			set bank_Loan <- 0.0;
		}else{
			set bank_Account <- bank_Account - surplus;
			set bank_Loan <- bank_Loan - surplus;	
		}
			

		//if bankaccount below 0, farmers need a loan, only if they have a loan below max allowed loan
		if bank_Account < 0
		{
			float possible_loan <- max_Loan - bank_Loan;
			if possible_loan > 0
			{
				if bank_Account < possible_loan
				{
					bank_Loan <- max_Loan;
					bank_Account <- bank_Account + possible_loan;
					
				} else
				{
					bank_Loan <- bank_Loan + bank_Account;
					bank_Account <- 0.0;
					 
				}

			}

		}

	}
	
	
		//Currently a very simple rule to create an initial distribution of production systems, need to be elaborated
	action seed_initial_distribution_of_production_system
	{
		if land_Use = "Protection forest"
		{
			set area_IMS <- tot_Area;
		}

		if land_Use = "Production forest"
		{
			if flip(Prob_INT_IMS)
			{
				set area_INT <- INT_size;
				set area_IMS <- tot_Area - area_INT;
				if area_IMS < 0
				{
					set area_IMS <- 0.0;
				}

			} else if flip(Prob_IE_IMS)
			{
				set area_IE <- IE_size;
				set area_IMS <- tot_Area - area_IE;
				set shrimp_Type <- rnd(1, 2);
			} else
			{
				set area_IMS <- tot_Area;
			}

		}

		if land_Use = "Shrimp"
		{
			if flip(Prob_INT_IE)
			{
				set area_INT <- INT_size;
				set area_IE <- tot_Area - area_INT;
				set shrimp_Type <- rnd(1, 2);
			} else if flip(Prob_IE)
			{
				set area_IE <- tot_Area;
			} else
			{
				set area_INT <- tot_Area; //todo: need to set max size for pure intensive farms.
				set shrimp_Type <- rnd(1, 2);
			}

		}

	} //end seed_initial_distribution_of_production_system
	

