clear
import delimited "C:\Users\Jasim\Desktop\EC4304\Project\data2.csv"
gen t=_n
tsset t

*Generate the variables required for the estimation of the GARCH and EGARCH models
gen gar_varh1=.
gen gar_varh5=.
gen gar_varh20=.
gen egar_varh1=.
gen egar_varh5=.
gen egar_varh20=.

*Generate the columns for the lags of RV and RAV for 5-days mean and and 22-day mean*
gen adjustedrv5=.
gen adjustedrv22=.
gen adjustedrav5=.
gen adjustedrav22=.

*Generate 2 columns for Actual RV and RAV values that is used to compute RMSE and RMSFE for HAR-RV and HAR-RAV
gen actrv=adjustedrv if t>=1222
gen actrav=adjustedrav if t>=1222


***************************************************************************
***Use Hybrid Window to estimate GARCH(1,1) and EGARCH(1,1) for 1,5 and 20
***Step ahead forecast respectively                                        
***************************************************************************
forvalues p=0/5{
	
	*multiplier of 10
	local j= `p'*300
	
	*start of window
	local z= `j'+ 22
	
	*end of window for h=1
	local q=`z'+1199
	
	*GARCH(1,1)
	arch daily_return if t>=`z' & t<=`q', arch(1) garch(1)
	forvalues i=1/300{
		predict temp_var1, variance
		replace gar_varh1=temp_var1 if t==(`q'+`i')
		drop temp_var1
	}
	*EGARCH(1,1)
	arch daily_return if t>=`z' & t<=`q', earch(1) egarch(1)
	forvalues j=1/300{
		predict temp_evar1, variance
		replace egar_varh1=temp_evar1 if t==(`q'+`j')
		drop temp_evar1
	}
}

*h=5
forvalues p=0/5{
	*multiplier for 100 obs
	local j= `p'*300
	
	*start of window
	local k= `j'+22
	
	*end of window
	local h= `k'+1195
	
	*estimate GARCH(1,1)
	arch daily_return if t>=`k' & t<=`h', arch(1) garch(1)
	
	forvalues m=1/300{
		predict temp_var5, variance dynamic(`h'+`m')
		replace gar_varh5=temp_var5 if t==(`h'+`m'+4)
		drop temp_var5
	}
	*estimate EGARCH(1,1)
	arch daily_return if t>=`k' & t<=`h', earch(1) egarch(1)
	
	forvalues n=1/300{
		predict temp_evar5, variance dynamic(`h'+`n')
		replace egar_varh5=temp_evar5 if t==(`h'+`n'+4)
		drop temp_evar5
	}
}


*h=20
forvalues w=0/5{
	*multiplier for 100 obs
	local j= `w'*300
	
	*start of window
	local k= `j'+22
	
	*end of window
	local h= `k'+1180
	
	*estimate GARCH(1,1)
	arch daily_return if t>=`k' & t<=`h', arch(1) garch(1)
	
	forvalues m=1/300{
		predict temp_var20, variance dynamic(`h'+`m')
		replace gar_varh20=temp_var20 if t==(`h'+`m'+19)
		drop temp_var20
	}
	*estimate EGARCH(1,1)
	arch daily_return if t>=`k' & t<=`h', earch(1) egarch(1)
	
	forvalues n=1/300{
		predict temp_evar20, variance dynamic(`h'+`n')
		replace egar_varh20=temp_evar20 if t==(`h'+`n'+19)
		drop temp_evar20
	}
}

***************************************************************************
***End of GARCH(1,1) and EGARCH(1,1) Estimation
***************************************************************************







**************************************
*Generate rolling means for RV and RAV:
***************************************
*5-day means for RV
forvalues p=1/2892 {
qui gen temp=adjustedrv if t>=`p' & t<=`p'+5-1
egen temp2=mean(temp)
qui replace adjustedrv5=temp2 if t==`p'+5-1
drop temp*
}
*22-day means for RV
forvalues p=1/2875 {
qui gen temp=adjustedrv if t>=`p' & t<=`p'+22-1
egen temp2=mean(temp)
qui replace adjustedrv22=temp2 if t==`p'+22-1
drop temp*
}

*5-day means for RAV
forvalues p=1/2892 {
qui gen temp=adjustedrav if t>=`p' & t<=`p'+5-1
egen temp2=mean(temp)
qui replace adjustedrav5=temp2 if t==`p'+5-1
drop temp*
}
*22-day means for RAV
forvalues p=1/2875 {
qui gen temp=adjustedrav if t>=`p' & t<=`p'+22-1
egen temp2=mean(temp)
qui replace adjustedrav22=temp2 if t==`p'+22-1
drop temp*
}

*********************************************
*End of Generate rolling means for RV and RAV:
*********************************************






****************************************************************************
*Estimate the HAR model for RV to produce expanding-window forecasts (Expanding Window Using t>=22 & t<=1222 initially,and continuously expand afterwards):
****************************************************************************

gen adjustedrvfithar_1step = .
gen adjustedrvfithar_5step = .
gen adjustedrvfithar_20step = .
gen adjustedravfithar_1step = .
gen adjustedravfithar_5step = .
gen adjustedravfithar_20step = .

*1-Step Ahead Forecast
forvalues p=1/1674{
	local z=22+1
	local q=`p'+1220
	qui reg adjustedrv L.adjustedrv L.adjustedrv5 L.adjustedrv22 if t >= `z' & t <= `q'
	predict y
	replace adjustedrvfithar_1step=y if t==`q'+1
	drop y
	
	qui reg adjustedrv L.adjustedrav L.adjustedrav5 L.adjustedrav22 if t >= `z' & t <= `q'
	predict y
	replace adjustedravfithar_1step=y if t==`q'+1
	drop y
}
*5-Step Ahead Forecast
forvalues p=1/1674{
	local z=22+5
	local q=`p'+1220
	qui reg adjustedrv L5.adjustedrv L5.adjustedrv5 L5.adjustedrv22 if t >= `z' & t <= `q'
	predict y
	replace adjustedrvfithar_5step=y if t==`q'+1
	drop y
	
	qui reg adjustedrv L5.adjustedrav L5.adjustedrav5 L5.adjustedrav22 if t >= `z' & t <= `q'
	predict y
	replace adjustedravfithar_5step=y if t==`q'+1
	drop y
}
*20-Step Ahead Forecast
forvalues p=1/1674{
	local z=22+20
	local q=`p'+1220
	qui reg adjustedrv L20.adjustedrv L20.adjustedrv5 L20.adjustedrv22 if t >= `z' & t <= `q'
	predict y
	replace adjustedrvfithar_20step=y if t==`q'+1
	drop y
	
	qui reg adjustedrv L20.adjustedrav L20.adjustedrav5 L20.adjustedrav22 if t >= `z' & t <= `q'
	predict y
	replace adjustedravfithar_20step=y if t==`q'+1
	drop y
}

***************************************************************************
*End of Estimate the HAR model for RV to produce expanding-window forecasts
***************************************************************************






**************************************************************
*1-Step,5-Step,20-Step RMSE for GARCH(1,1), EGARCH(1,1), HAR-RAV, HAR-RV
**************************************************************
 
*Produce 1-step forecast RMSE for GARCH, EGARCH, HAR-RAV and HAR-RV:
egen mse_garch_h1=mean((actrv-gar_varh1)^2) 
gen rmse_garch_h1=sqrt(mse_garch_h1)
egen mse_egarch_h1=mean((actrv-egar_varh1)^2)
gen rmse_egarch_h1=sqrt(mse_egarch_h1) 
egen adjustedrav_mse_1step=mean((actrv-adjustedravfithar_1step)^2) 
gen adjustedrav_rmse_1step=sqrt(adjustedrav_mse_1step)
egen adjustedrv_mse_1step=mean((actrv-adjustedrvfithar_1step)^2) 
gen adjustedrv_rmse_1step=sqrt(adjustedrv_mse_1step)

*Produce 5-step forecast RMSE for GARCH, EGARCH, HAR-RAV and HAR-RV:
egen mse_garch_h5=mean((actrv-gar_varh5)^2)
gen rmse_garch_h5=sqrt(mse_garch_h5) 
egen mse_egarch_h5=mean((actrv-egar_varh5)^2)
gen rmse_egarch_h5=sqrt(mse_egarch_h5) 
egen adjustedrav_mse_5step=mean((actrv-adjustedravfithar_5step)^2) 
gen adjustedrav_rmse_5step=sqrt(adjustedrav_mse_5step)
egen adjustedrv_mse_5step=mean((actrv-adjustedrvfithar_5step)^2) 
gen adjustedrv_rmse_5step=sqrt(adjustedrv_mse_5step)

*Produce 20-step forecast RMSE for GARCH, EGARCH, HAR-RAV and HAR-RV:
egen mse_garch_h20=mean((actrv-gar_varh20)^2) 
gen rmse_garch_h20=sqrt(mse_garch_h20)
egen mse_egarch_h20=mean((actrv-egar_varh20)^2)
gen rmse_egarch_h20=sqrt(mse_egarch_h20) 
egen adjustedrav_mse_20step=mean((actrv-adjustedravfithar_20step)^2) 
gen adjustedrav_rmse_20step=sqrt(adjustedrav_mse_20step)
egen adjustedrv_mse_20step=mean((actrv-adjustedrvfithar_20step)^2) 
gen adjustedrv_rmse_20step=sqrt(adjustedrv_mse_20step)

*********************************************************************
*End Of 1-Step,5-Step,20-Step RMSE for GARCH, EGARCH, HAR-RAV, HAR-RV
*********************************************************************






***********************************
*Generating Simple Average Forecast
***********************************

*Generate simple average combined forecast and associated forecast errors for 1-Step Ahead:
gen ycombine_1=(adjustedravfithar_1step+adjustedrvfithar_1step+gar_varh1+egar_varh1)/4
egen ecombine_1 = mean((actrv-ycombine_1)^2)
gen rmsecomb_1=sqrt(ecombine_1)
*Generate simple average combined forecast and associated forecast errors for 5-step ahead:
gen ycombine_5=(adjustedravfithar_5step+adjustedrvfithar_5step+gar_varh5+egar_varh5)/4
egen ecombine_5 = mean((act	rv-ycombine_5)^2)
gen rmsecomb_5=sqrt(ecombine_5)
*Generate simple average combined forecast and associated forecast errors for 20-step ahead:
gen ycombine_20=(adjustedravfithar_20step+adjustedrvfithar_20step+gar_varh20+egar_varh20)/4
egen ecombine_20 = mean((actrv-ycombine_20)^2)
gen rmsecomb_20=sqrt(ecombine_20)

*Compare RMSE's across all forecasts including combined forecast for 1-Step Ahead: + Tze Hao Model
sum adjustedrav_rmse_1step adjustedrv_rmse_1step rmse_garch_h1 rmse_egarch_h1 

*Compare RMSE's across all forecasts including combined forecast for 5-Step Ahead:
sum adjustedrav_rmse_5step adjustedrv_rmse_5step rmse_egarch_h5 rmse_garch_h5

*Compare RMSE's across all forecasts including combined forecast for 20-Step Ahead: 
sum adjustedrav_rmse_20step adjustedrv_rmse_20step rmse_egarch_h20 rmse_garch_h20

******************************************
*End Of Generating Simple Average Forecast
******************************************








*************************************
*Granger-Ramanathan Combined Forecast
*************************************

*** Combined 1-Step Ahead Forecast***

*Unconstrained regression first to see which forecasts are collinear. If find any, drop them(For 1-step ahead): Add Tze Hao Model
reg actrv adjustedravfithar_1step adjustedrvfithar_1step egar_varh1 gar_varh1, noconstant

*Remove egar_varh1, which has negative coefficient and regress again --> There isnt any more negative coefficient after regressing
reg actrv adjustedravfithar_1step adjustedrvfithar_1step gar_varh1, noconstant

*Now set the constraint, and use cnsreg to run the regression:
constraint 1 adjustedravfithar_1step+adjustedrvfithar_1step+gar_varh1=1
cnsreg actrv adjustedravfithar_1step adjustedrvfithar_1 gar_varh1, constraints(1) noconstant

*Now we are down to three forecasts(With coeffiecient all +ve). Use weights to generate combined forecasts:
gen ycombine_1step = adjustedravfithar_1step*0.2572083 + adjustedrvfithar_1step*0.7136624  + gar_varh1*0.0291293
egen ecombine_1step = mean((actrv-ycombine_1step)^2)
gen rmsecombine_1step =sqrt(ecombine_1step)

*Examine RMSE's of 1-Step of Simple Average and Granger-Ramanathan so far:
sum rmsecomb_1 rmsecombine_1step

*** End Of Combined 1-Step Ahead Forecaet***


*** Combined 5-Step Ahead Forecaet***

*Unconstrained regression first to see which forecasts are collinear. If find any, drop them(For 5-step ahead): Add Tze Hao Model
reg actrv adjustedravfithar_5step adjustedrvfithar_5step egar_varh5 gar_varh5, noconstant


*We found 2(adjustedravfithar_5step,adjustedrvfithar_5step), so we remove both and reg again
reg actrv adjustedravfithar_5step gar_varh5, noconstant

*gar_varh5 contains negative weight, so we remove it, keeping adjustedravfithar_5step only
reg actrv adjustedravfithar_5step

*With only one forecasts, we will use adjustedravfithar_5step to forecast only:
gen ycombine_5step= adjustedravfithar_5step
egen ecombine_5step = mean((actrv-ycombine_5step)^2)
gen rmsecombine_5step=sqrt(ecombine_5step)

*Examine RMSE's of 5-Step of Simple Average and Granger-Ramanathan so far:
sum rmsecomb_5 rmsecombine_5step

*** End Of Combined 5-Step Ahead Forecaet***




*Unconstrained regression first to see which forecasts are collinear. If find any, drop them(For 20-step ahead): Add Tze Hao Model
reg actrv adjustedravfithar_20step adjustedrvfithar_20step egar_varh20 gar_varh20, noconstant


*We find 2 models with negative weights, so we remove the two and regress again with the remaining
reg actrv adjustedravfithar_20step gar_varh20, noconstant

*The remaining 2 models has positive weight, so we set the constraint and estimate for the weights
constraint 2 adjustedravfithar_20step+gar_varh20=1
cnsreg actrv adjustedravfithar_20step gar_varh20, constraints(2) noconstant

*Now we are down to two forecasts. Use weights to generate combined forecasts:
gen ycombine_20step= adjustedravfithar_20step*0.9648108  + gar_varh20*0.0246183
egen ecombine_20step = mean((actrv-ycombine_20step)^2)
gen rmsecombine_20step=sqrt(ecombine_20step)

*Examine RMSE's of 20-Step Simple Average and Granger-Ramanathan so far:
sum rmsecomb_20 rmsecombine_20step



********************************************
*End of Granger-Ramanathan Combined Forecast
********************************************


**************************************
*Evaluating the models
**************************************

*Generate forecast errors for GARCH
gen fe_garch_h1=adjustedrv-gar_varh1 
gen fe_garch_h5=adjustedrv-gar_varh5
gen fe_garch_h20=adjustedrv-gar_varh20 

*Generate forecast errors for EGARCH
gen fe_egarch_h1=adjustedrv-egar_varh1 
gen fe_egarch_h5=adjustedrv-egar_varh5
gen fe_egarch_h20=adjustedrv-egar_varh20 

*FE for HAR-RV
gen fe_rv_h1=adjustedrv-adjustedrvfithar_1step
gen fe_rv_h5=adjustedrv-adjustedrvfithar_5step
gen fe_rv_h20=adjustedrv-adjustedrvfithar_20step

*FE for HAR-RAV
gen fe_rav_h1=adjustedrv-adjustedravfithar_1step
gen fe_rav_h5=adjustedrv-adjustedravfithar_5step
gen fe_rav_h20=adjustedrv-adjustedravfithar_20step

*FE for Simple Average
gen fe_simpavg_h1=adjustedrv-ycombine_1
gen fe_simpavg_h5=adjustedrv-ycombine_5
gen fe_simpavg_h20=adjustedrv-ycombine_20

*FE for Granger-Ramanathan
gen fe_gr_h1=adjustedrv-ycombine_1step
gen fe_gr_h5=adjustedrv-ycombine_5step
gen fe_gr_h20=adjustedrv-ycombine_20step

*examine bias and RMSE
sum fe_garch_h1 fe_egarch_h1
sum fe_garch_h5 fe_egarch_h5
sum fe_garch_h20 fe_egarch_h20

sum fe_garch_h1 fe_rv_h1
sum fe_garch_h5 fe_rv_h5
sum fe_garch_h20 fe_rv_h20

sum fe_garch_h1 fe_rav_h1
sum fe_garch_h5 fe_rav_h5
sum fe_garch_h20 fe_rav_h20

sum fe_garch_h1 fe_simpavg_h1
sum fe_garch_h5 fe_simpavg_h5
sum fe_garch_h20 fe_simpavg_h20

sum fe_garch_h1 fe_gr_h1
sum fe_garch_h5 fe_gr_h5
sum fe_garch_h20 fe_gr_h20

*DM Test (MSE)

*Comparing GARCH and EGARCH*

gen d_egarch_1=fe_garch_h1^2-fe_egarch_h1^2
gen d_egarch_5=fe_garch_h5^2-fe_egarch_h5^2
gen d_egarch_20=fe_garch_h20^2-fe_egarch_h20^2

newey d_egarch_1, lag(17)
newey d_egarch_5, lag(18)
newey d_egarch_20, lag(17)

*Comparing GARCH and HAR-RV*

gen d_rv_1=fe_garch_h1^2-fe_rv_h1^2
gen d_rv_5=fe_garch_h5^2-fe_rv_h5^2
gen d_rv_20=fe_garch_h20^2-fe_rv_h20^2

newey d_rv_1, lag(11)
newey d_rv_5, lag(11)
newey d_rv_20, lag(11)

*Comparing GARCH and HAR-RAV*

gen d_rav_1=fe_garch_h1^2-fe_rav_h1^2
gen d_rav_5=fe_garch_h5^2-fe_rav_h5^2
gen d_rav_20=fe_garch_h20^2-fe_rav_h20^2

newey d_rav_1, lag(11)
newey d_rav_5, lag(11)
newey d_rav_20, lag(11)

*Comparing GARCH and simple average*

gen d_simpavg_1=fe_garch_h1^2-fe_simpavg_h1^2
gen d_simpavg_5=fe_garch_h5^2-fe_simpavg_h5^2
gen d_simpavg_20=fe_garch_h20^2-fe_simpavg_h20^2

newey d_simpavg_1, lag(11)
newey d_simpavg_5, lag(11)
newey d_simpavg_20, lag(11)

*Comparing GARCH and Granger-Ramanathan*

gen d_gr_1=fe_garch_h1^2-fe_gr_h1^2
gen d_gr_5=fe_garch_h5^2-fe_gr_h5^2
gen d_gr_20=fe_garch_h20^2-fe_gr_h20^2

newey d_gr_1, lag(11)
newey d_gr_5, lag(11)
newey d_gr_20, lag(11)

*Comparing Simple Average and Granger-Ramanathan*

gen d_smgr_1=fe_simpavg_h1^2-fe_gr_h1^2
gen d_smgr_5=fe_simpavg_h5^2-fe_gr_h5^2
gen d_smgr_20=fe_simpavg_h20^2-fe_gr_h20^2

newey d_smgr_1, lag(11)
newey d_smgr_5, lag(11)
newey d_smgr_20, lag(11)

*Examine QLIKE loss
gen l_garch_h1=(adjustedrv/gar_varh1)-log(adjustedrv/gar_varh1)-1
gen l_garch_h5=(adjustedrv/gar_varh5)-log(adjustedrv/gar_varh5)-1
gen l_garch_h20=(adjustedrv/gar_varh20)-log(adjustedrv/gar_varh20)-1

gen l_egarch_h1=(adjustedrv/egar_varh1)-log(adjustedrv/egar_varh1)-1
gen l_egarch_h5=(adjustedrv/egar_varh5)-log(adjustedrv/egar_varh5)-1
gen l_egarch_h20=(adjustedrv/egar_varh20)-log(adjustedrv/egar_varh20)-1

gen l_rv_h1=(adjustedrv/adjustedrvfithar_1step)-log(adjustedrv/adjustedrvfithar_1step)-1
gen l_rv_h5=(adjustedrv/adjustedrvfithar_5step)-log(adjustedrv/adjustedrvfithar_5step)-1
gen l_rv_h20=(adjustedrv/adjustedrvfithar_20step)-log(adjustedrv/adjustedrvfithar_20step)-1

gen l_rav_h1=(adjustedrv/adjustedravfithar_1step)-log(adjustedrv/adjustedravfithar_1step)-1
gen l_rav_h5=(adjustedrv/adjustedravfithar_5step)-log(adjustedrv/adjustedravfithar_5step)-1
gen l_rav_h20=(adjustedrv/adjustedravfithar_20step)-log(adjustedrv/adjustedravfithar_20step)-1

gen l_simpavg_h1=(adjustedrv/ycombine_1)-log(adjustedrv/ycombine_1)-1
gen l_simpavg_h5=(adjustedrv/ycombine_5)-log(adjustedrv/ycombine_5)-1
gen l_simpavg_h20=(adjustedrv/ycombine_20)-log(adjustedrv/ycombine_20)-1

gen l_gr_h1=(adjustedrv/ycombine_1step)-log(adjustedrv/ycombine_1step)-1
gen l_gr_h5=(adjustedrv/ycombine_5step)-log(adjustedrv/ycombine_5step)-1
gen l_gr_h20=(adjustedrv/ycombine_20step)-log(adjustedrv/ycombine_20step)-1

*Examine expected loss (look at the mean) 
sum l_garch_h1 l_egarch_h1
sum l_garch_h5 l_egarch_h5
sum l_garch_h20 l_egarch_h20

sum l_garch_h1 l_rv_h1
sum l_garch_h5 l_rv_h1
sum l_garch_h20 l_rv_h1

sum l_garch_h1 l_rav_h1
sum l_garch_h5 l_rav_h5
sum l_garch_h20 l_rav_h20

sum l_garch_h1 l_simpavg_h1
sum l_garch_h5 l_simpavg_h5
sum l_garch_h20 l_simpavg_h20

sum l_garch_h1 l_gr_h1
sum l_garch_h5 l_gr_h5
sum l_garch_h20 l_gr_h20

*Perform Diebold-Mariano test (QLIKE):
gen egar_h1 = l_garch_h1-l_egarch_h1
gen egar_h5 = l_garch_h5-l_egarch_h5
gen egar_h20 = l_garch_h20-l_egarch_h20

gen rv_h1 = l_garch_h1-l_rv_h1
gen rv_h5 = l_garch_h5-l_rv_h5
gen rv_h20 = l_garch_h20-l_rv_h20

gen rav_h1 = l_garch_h1-l_rav_h1
gen rav_h5 = l_garch_h5-l_rav_h5
gen rav_h20 = l_garch_h20-l_rav_h20

gen simpavg_h1 = l_garch_h1-l_simpavg_h1
gen simpavg_h5 = l_garch_h5-l_simpavg_h5
gen simpavg_h20 = l_garch_h20-l_simpavg_h20

gen gr_h1 = l_garch_h1-l_gr_h1
gen gr_h5 = l_garch_h5-l_gr_h5
gen gr_h20 = l_garch_h20-l_gr_h20

gen smgr_h1 = l_simpavg_h1-l_gr_h1
gen smgr_h5 = l_simpavg_h5-l_gr_h5
gen smgr_h20 = l_simpavg_h20-l_gr_h20

newey egar_h1, lag(11)
newey egar_h5, lag(11)
newey egar_h20, lag(11)

newey rv_h1, lag(11)
newey rv_h5, lag(11)
newey rv_h20, lag(11)

newey rav_h1, lag(11) force
newey rav_h5, lag(11)
newey rav_h20, lag(11)

newey simpavg_h1, lag(11)
newey simpavg_h5, lag(11)
newey simpavg_h20, lag(11)

newey gr_h1, lag(11)
newey gr_h5, lag(11)
newey gr_h20, lag(11)

newey smgr_h1, lag(11)
newey smgr_h5, lag(11)
newey smgr_h20, lag(11)
















