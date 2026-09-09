# load R packages

setwd("/Users/hanlinshang/Dropbox/Todos/FANOVA_FFM_CDF/code")
source("load_packages.R")
source("auxiliary_point.R")
source("auxiliary_interval.R")

# setwd("~/Dropbox/Todos/FANOVA_FFM_CDF/code/CDF/point_forecast/Gap_modeling")
setwd("/Users/hanlinshang/Dropbox/Todos/FANOVA_FFM_CDF/code/CDF/point_forecast/Gap_modeling")

# N_M_CDF: national male CDF
# N_F_CDF: national female CDF
# year_index: year index
# region_index: regional index
# fh: forecast horizon
# fmethod: univariate time-series forecasting method

region_gap_fore <- function(N_M_CDF, N_F_CDF, year_index, region_index, fh, fmethod)
{
    N_nrow = nrow(N_M_CDF)
    N_ncol = ncol(N_M_CDF)
    
    # ensure national CDF >= 0
    
    N_F_CDF[N_F_CDF <= 0] <- 1e-8
    N_M_CDF[N_M_CDF <= 0] <- 1e-8
    
    fts_obj = fts(ages[1:(N_nrow - 1)], logit(N_F_CDF[1:(N_nrow - 1), 1:year_index]))
    Japan_female_pop_CDF_forecast = c(invlogit(forecast(ftsm(fts_obj), h = fh, method = fmethod)$mean$y[,fh]), 1)
    rm(fts_obj)
    
    fts_obj = fts(ages[1:(N_nrow - 1)], logit(N_M_CDF[1:(N_nrow - 1), 1:year_index]))
    Japan_male_pop_CDF_forecast = c(invlogit(forecast(ftsm(fts_obj), h = fh, method = fmethod)$mean$y[,fh]), 1)
    rm(fts_obj)
    
    # region gap via Fisher Z transformation
    
    fts_obj = fts(ages, FisherZ(subnational_region_F_diff_CDF[[region_index]][,1:year_index]))
    subnational_region_F_diff_CDF_forecast = FisherZInv(forecast(ftsm(fts_obj), h = fh, method = fmethod)$mean$y[,fh])
    rm(fts_obj)
    
    fts_obj = fts(ages, FisherZ(subnational_region_M_diff_CDF[[region_index]][,1:year_index]))
    subnational_region_M_diff_CDF_forecast = FisherZInv(forecast(ftsm(fts_obj), h = fh, method = fmethod)$mean$y[,fh])
    rm(fts_obj)
    
    # region gap + national (M or F) forecasts
    
    subnational_female_pop_CDF_forecast = subnational_region_F_diff_CDF_forecast + Japan_female_pop_CDF_forecast
    subnational_male_pop_CDF_forecast = subnational_region_M_diff_CDF_forecast + Japan_male_pop_CDF_forecast
    
    # ensure CDF between 0 and 1
    
    subnational_female_pop_CDF_forecast <- pmin(pmax(subnational_female_pop_CDF_forecast, 10^-8), 1)
    subnational_male_pop_CDF_forecast   <- pmin(pmax(subnational_male_pop_CDF_forecast, 10^-8), 1)
    
    # from CDF to PDF via first-order differencing
    
    subnational_female_pop_PDF_forecast = c(subnational_female_pop_CDF_forecast[1], diff(subnational_female_pop_CDF_forecast))
    subnational_male_pop_PDF_forecast   = c(subnational_male_pop_CDF_forecast[1],   diff(subnational_male_pop_CDF_forecast))
    
    # ensure PDF within its range
    
    subnational_female_pop_PDF_forecast = pmax(subnational_female_pop_PDF_forecast, 0) * 10^5
    subnational_male_pop_PDF_forecast = pmax(subnational_male_pop_PDF_forecast, 0) * 10^5
    return(list(male_fore = subnational_male_pop_PDF_forecast, female_fore = subnational_female_pop_PDF_forecast))
}

# N_M_CDF_data: national male CDFs
# N_F_CDF_data: national female CDFs
# region_index_val: index 
# PDF_M_holdout_data: male holdout densities
# PDF_F_holdout_data: female holdout densities
# horizon: forecast horizon
# fore_method: univariate time-series forecasting method
# level_sig: level of significance

region_gap_fore_fun_int <- function(N_M_CDF_data, N_F_CDF_data, region_index_val, 
                                    PDF_M_holdout_data, PDF_F_holdout_data, 
            									      horizon, fore_method, level_sig)
{
  	n_row = nrow(N_M_CDF_data)
  	n_col = ncol(N_M_CDF_data)
  	
  	forecast_val_M = forecast_val_F = matrix(NA, n_row, (22 - horizon))
  	for(ik in 1:(22 - horizon))
  	{
    		dum <- region_gap_fore(N_M_CDF = N_M_CDF_data, 
    		                       N_F_CDF = N_F_CDF_data, 
    		                       year_index = (n_col - 42 + ik),
    								           region_index = region_index_val,
    								           fh = horizon, fmethod = fore_method)
        forecast_val_M[,ik] = dum$male_fore
        forecast_val_F[,ik] = dum$female_fore								   
        rm(ik); rm(dum)
  	}
  	
  	# holdout data
  	
  	PDF_M = matrix(t(male_prefecture_dx[[region_index_val]][(n_year - 41 + horizon):(n_year - 20),]), n_row, (22 - horizon))
  	PDF_F = matrix(t(female_prefecture_dx[[region_index_val]][(n_year - 41 + horizon):(n_year - 20),]), n_row, (22 - horizon))
  	
  	# compute residuals
  	
  	resi_mat_F = PDF_F - forecast_val_F
  	resi_mat_M = PDF_M - forecast_val_M
  	
  	# compute standard deviation of residuals
  	
  	sd_val_F = apply(resi_mat_F, 1, sd)
  	sd_val_M = apply(resi_mat_M, 1, sd)
  	
  	# find the optimal tuning parameter for female data
  	
  	tune_para_find_val_F_1 = optimise(f = tune_para_find_function, interval = c(0, 1),
  	                                  resi_mat = resi_mat_F, sd_val_input = sd_val_F,
  	                                  alpha_level = level_sig)
  	
  	tune_para_find_val_F_2 = optimise(f = tune_para_find_function, interval = c(0, 5),
  	                                  resi_mat = resi_mat_F, sd_val_input = sd_val_F,
  	                                  alpha_level = level_sig)
  	
  	tune_para_find_val_F_3 = optimise(f = tune_para_find_function, interval = c(0, 10),
  	                                  resi_mat = resi_mat_F, sd_val_input = sd_val_F,
  	                                  alpha_level = level_sig)
  	
  	tune_para_find_val_F_4 = optimise(f = tune_para_find_function, interval = c(0, 20),
  	                                  resi_mat = resi_mat_F, sd_val_input = sd_val_F,
  	                                  alpha_level = level_sig)
  	
  	tune_para_find_val_F_5 = optim(par = 1, fn = tune_para_find_function, lower = 0, method = "L-BFGS-B",
  	                               resi_mat = resi_mat_F, sd_val_input = sd_val_F,
  	                               alpha_level = level_sig)
  	
  	tune_para_find_val_F_6 = optim(par = 1, fn = tune_para_find_function,
  	                               resi_mat = resi_mat_F, sd_val_input = sd_val_F,
  	                               alpha_level = level_sig)
  	
  	obj_val = c(tune_para_find_val_F_1$objective, 
  	            tune_para_find_val_F_2$objective, 
  	            tune_para_find_val_F_3$objective, 
  	            tune_para_find_val_F_4$objective, 
  	            tune_para_find_val_F_5$value, 
  	            tune_para_find_val_F_6$value)
  	obj_val_min_F = min(obj_val)
  	
  	tune_para_find_F = c(tune_para_find_val_F_1$minimum, 
  	                     tune_para_find_val_F_2$minimum,
  	                     tune_para_find_val_F_3$minimum,
  	                     tune_para_find_val_F_4$minimum,
  	                     tune_para_find_val_F_5$par,
  	                     tune_para_find_val_F_6$par)[which.min(obj_val)]
  	rm(obj_val)
  	
  	# find the optimal tuning parameter for male data
  	
  	tune_para_find_val_M_1 = optimise(f = tune_para_find_function, interval = c(0, 1),
  	                                  resi_mat = resi_mat_M, sd_val_input = sd_val_M,
  	                                  alpha_level = level_sig)
  	
  	tune_para_find_val_M_2 = optimise(f = tune_para_find_function, interval = c(0, 5),
  	                                  resi_mat = resi_mat_M, sd_val_input = sd_val_M,
  	                                  alpha_level = level_sig)
  	
  	tune_para_find_val_M_3 = optimise(f = tune_para_find_function, interval = c(0, 10),
  	                                  resi_mat = resi_mat_M, sd_val_input = sd_val_M,
  	                                  alpha_level = level_sig)
  	
  	tune_para_find_val_M_4 = optimise(f = tune_para_find_function, interval = c(0, 20),
  	                                  resi_mat = resi_mat_M, sd_val_input = sd_val_M,
  	                                  alpha_level = level_sig)
  	
  	tune_para_find_val_M_5 = optim(par = 1, fn = tune_para_find_function, lower = 0, method = "L-BFGS-B",
  	                               resi_mat = resi_mat_M, sd_val_input = sd_val_M,
  	                               alpha_level = level_sig)
  	
  	tune_para_find_val_M_6 = optim(par = 1, fn = tune_para_find_function, 
  	                               resi_mat = resi_mat_M, sd_val_input = sd_val_M,
  	                               alpha_level = level_sig)
  	
  	obj_val = c(tune_para_find_val_M_1$objective, 
  	            tune_para_find_val_M_2$objective, 
  	            tune_para_find_val_M_3$objective, 
  	            tune_para_find_val_M_4$objective, 
  	            tune_para_find_val_M_5$value, 
  	            tune_para_find_val_M_6$value)
  	obj_val_min_M = min(obj_val)
  	
  	tune_para_find_M = c(tune_para_find_val_M_1$minimum, 
  	                     tune_para_find_val_M_2$minimum, 
  	                     tune_para_find_val_M_3$minimum, 
  	                     tune_para_find_val_M_4$minimum, 
                         tune_para_find_val_M_5$par,
                         tune_para_find_val_M_6$par)[which.min(obj_val)]
    rm(obj_val); rm(PDF_F); rm(PDF_M)
    
	
    # forecasts for the testing period
    
    forecast_test_F = forecast_test_F_lb = forecast_test_F_ub = matrix(NA, n_row, (21 - horizon))
    forecast_test_M = forecast_test_M_lb = forecast_test_M_ub = matrix(NA, n_row, (21 - horizon))
    for(ik in 1:(21 - horizon))
    {
        dum <- region_gap_fore(N_M_CDF = N_M_CDF_data, 
                               N_F_CDF = N_F_CDF_data, 
                               year_index = (n_col - 21 + ik),
                               region_index = region_index_val,
                               fh = horizon, fmethod = fore_method)
        forecast_test_M[,ik] = dum$male_fore
        forecast_test_M_lb[,ik] = forecast_test_M[,ik] - tune_para_find_M * sd_val_M
        forecast_test_M_ub[,ik] = forecast_test_M[,ik] + tune_para_find_M * sd_val_M
        
        forecast_test_F[,ik] = dum$female_fore			
        forecast_test_F_lb[,ik] = forecast_test_F[,ik] - tune_para_find_F * sd_val_F
        forecast_test_F_ub[,ik] = forecast_test_F[,ik] + tune_para_find_F * sd_val_F
        rm(ik); rm(dum)
    }
    
    # holdout data
    
    PDF_M = matrix(t(male_prefecture_dx[[region_index_val]][(n_year - 20 + horizon):n_year,]), n_row, length((n_year - 20 + horizon):n_year))
    PDF_F = matrix(t(female_prefecture_dx[[region_index_val]][(n_year - 20 + horizon):n_year,]), n_row, length((n_year - 20 + horizon):n_year))
    
    # compute CPD and MIS
    
    int_F_err = interval_score(holdout = PDF_F, lb = forecast_test_F_lb, ub = forecast_test_F_ub, alpha = (1 - level_sig))
    int_M_err = interval_score(holdout = PDF_M, lb = forecast_test_M_lb, ub = forecast_test_M_ub, alpha = (1 - level_sig))
    
    return(list(int_F_err = int_F_err, tune_para_find_F = tune_para_find_F, tune_para_find_F_obj = obj_val_min_F,
                int_M_err = int_M_err, tune_para_find_M = tune_para_find_M, tune_para_find_M_obj = obj_val_min_M))
      
}

###############
## alpha = 0.2
###############

# fmethod = "ets"

region_gap_ets_int_F = region_gap_ets_int_M = array(NA, dim = c(n_state, 20, 3), 
                                                    dimnames = list(state, 1:20, c("ECP", "CPD", "MIS")))
for(iwj in 1:n_state)
{
    for(iwk in 1:20)
    {
        dum = region_gap_fore_fun_int(N_M_CDF_data = Japan_male_pop_CDF,
                                      N_F_CDF_data = Japan_female_pop_CDF,
                                      region_index_val = iwj,
                                      PDF_M_holdout_data = male_prefecture_dx,
                                      PDF_F_holdout_data = female_prefecture_dx,
                                      horizon = iwk, fore_method = "ets", level_sig = 0.8)
        region_gap_ets_int_F[iwj,iwk,] = dum$int_F_err
        region_gap_ets_int_M[iwj,iwk,] = dum$int_M_err
        rm(iwk)
    }
    print(iwj); rm(iwj)
}

horizon_region_gap_ets_int_F_err_mean = apply(region_gap_ets_int_F, c(2, 3), mean)
horizon_region_gap_ets_int_M_err_mean = apply(region_gap_ets_int_M, c(2, 3), mean)

region_gap_ets_int_F_err_mean = apply(region_gap_ets_int_F, c(1, 3), mean)
region_gap_ets_int_M_err_mean = apply(region_gap_ets_int_M, c(1, 3), mean)

# fmethod = "arima"

region_gap_arima_int_F = region_gap_arima_int_M = array(NA, dim = c(n_state, 20, 3), 
                                                    dimnames = list(state, 1:20, c("ECP", "CPD", "MIS")))
for(iwj in 1:n_state)
{
    for(iwk in 1:20)
    {
        dum = region_gap_fore_fun_int(N_M_CDF_data = Japan_male_pop_CDF,
                                      N_F_CDF_data = Japan_female_pop_CDF,
                                      region_index_val = iwj,
                                      PDF_M_holdout_data = male_prefecture_dx,
                                      PDF_F_holdout_data = female_prefecture_dx,
                                      horizon = iwk, fore_method = "arima", level_sig = 0.8)
        region_gap_arima_int_F[iwj,iwk,] = dum$int_F_err
        region_gap_arima_int_M[iwj,iwk,] = dum$int_M_err
        rm(iwk)
    }
    print(iwj); rm(iwj)
}

horizon_region_gap_arima_int_F_err_mean = apply(region_gap_arima_int_F, c(2, 3), mean)
horizon_region_gap_arima_int_M_err_mean = apply(region_gap_arima_int_M, c(2, 3), mean)

region_gap_arima_int_F_err_mean = apply(region_gap_arima_int_F, c(1, 3), mean)
region_gap_arima_int_M_err_mean = apply(region_gap_arima_int_M, c(1, 3), mean)


################
## alpha = 0.05
################

# fmethod = "ets"

region_gap_ets_int_F_95 = region_gap_ets_int_M_95 = array(NA, dim = c(n_state, 20, 3), 
                                                    dimnames = list(state, 1:20, c("ECP", "CPD", "MIS")))
for(iwj in 1:n_state)
{
    for(iwk in 1:20)
    {
        dum = region_gap_fore_fun_int(N_M_CDF_data = Japan_male_pop_CDF,
                                      N_F_CDF_data = Japan_female_pop_CDF,
                                      region_index_val = iwj,
                                      PDF_M_holdout_data = male_prefecture_dx,
                                      PDF_F_holdout_data = female_prefecture_dx,
                                      horizon = iwk, fore_method = "ets", level_sig = 0.95)
        region_gap_ets_int_F_95[iwj,iwk,] = dum$int_F_err
        region_gap_ets_int_M_95[iwj,iwk,] = dum$int_M_err
        rm(iwk)
    }
    print(iwj); rm(iwj)
}

horizon_region_gap_ets_int_F_95_err_mean = apply(region_gap_ets_int_F_95, c(2, 3), mean)
horizon_region_gap_ets_int_M_95_err_mean = apply(region_gap_ets_int_M_95, c(2, 3), mean)

region_gap_ets_int_F_95_err_mean = apply(region_gap_ets_int_F_95, c(1, 3), mean)
region_gap_ets_int_M_95_err_mean = apply(region_gap_ets_int_M_95, c(1, 3), mean)

# fmethod = "arima"

region_gap_arima_int_F_95 = region_gap_arima_int_M_95 = array(NA, dim = c(n_state, 20, 3), 
                                                        dimnames = list(state, 1:20, c("ECP", "CPD", "MIS")))
for(iwj in 1:n_state)
{
    for(iwk in 1:20)
    {
        dum = region_gap_fore_fun_int(N_M_CDF_data = Japan_male_pop_CDF,
                                      N_F_CDF_data = Japan_female_pop_CDF,
                                      region_index_val = iwj,
                                      PDF_M_holdout_data = male_prefecture_dx,
                                      PDF_F_holdout_data = female_prefecture_dx,
                                      horizon = iwk, fore_method = "arima", level_sig = 0.95)
        region_gap_arima_int_F_95[iwj,iwk,] = dum$int_F_err
        region_gap_arima_int_M_95[iwj,iwk,] = dum$int_M_err
        rm(iwk)
    }
    print(iwj); rm(iwj)
}

horizon_region_gap_arima_int_F_95_err_mean = apply(region_gap_arima_int_F_95, c(2, 3), mean)
horizon_region_gap_arima_int_M_95_err_mean = apply(region_gap_arima_int_M_95, c(2, 3), mean)

region_gap_arima_int_F_95_err_mean = apply(region_gap_arima_int_F_95, c(1, 3), mean)
region_gap_arima_int_M_95_err_mean = apply(region_gap_arima_int_M_95, c(1, 3), mean)

