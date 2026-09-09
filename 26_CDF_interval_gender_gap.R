#########################################
# interval forecasts based on gender gap
#########################################

# CDF_M_data: cumulative distribution function for male data
# CDF_F_data: cumulative distribution function for female data
# PDF_M_holdout_data: age distribution of male death counts
# PDF_F_holdout_data: age distribution of female death counts
# horizon: forecast horizon
# fore_method: forecasting method
# level_sig: level of significance

gender_gap_fore_fun_int <- function(CDF_M_data, CDF_F_data, PDF_M_holdout_data, PDF_F_holdout_data, 
                                    horizon, fore_method, level_sig)
{
    n_col = ncol(CDF_M_data)
    n_row = nrow(CDF_M_data)
    
    forecast_val_M = forecast_val_F = matrix(NA, n_row, (22 - horizon))
    for(ik in 1:(22 - horizon))
    {
        dum <- gender_gap_fore(male_CDF = CDF_M_data[,1:(n_col - 42 + ik)], 
                               female_CDF = CDF_F_data[,1:(n_col - 42 + ik)], 
                               fh = horizon, fmethod = fore_method)
        forecast_val_M[,ik] = dum$male_fore
        forecast_val_F[,ik] = dum$female_fore
        rm(ik); rm(dum)
    }
    
    # holdout data
    
    PDF_M = matrix(t(PDF_M_holdout_data[(n_year - 41 + horizon):(n_year - 20),]), n_row, (22 - horizon))
    PDF_F = matrix(t(PDF_F_holdout_data[(n_year - 41 + horizon):(n_year - 20),]), n_row, (22 - horizon))
  
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
        dum <- gender_gap_fore(male_CDF = CDF_M_data[,1:(n_col - 21 + ik)], 
                               female_CDF = CDF_F_data[,1:(n_col - 21 + ik)], 
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
    
    PDF_M = matrix(t(PDF_M_holdout_data[(n_year - 20 + horizon):n_year,]), n_row, length((n_year - 20 + horizon):n_year))
    PDF_F = matrix(t(PDF_F_holdout_data[(n_year - 20 + horizon):n_year,]), n_row, length((n_year - 20 + horizon):n_year))
    
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

gender_gap_ets_int_F = gender_gap_ets_int_M = array(NA, dim = c(n_state, 20, 3), dimnames = list(state, 1:20, c("ECP", "CPD", "MIS")))
for(iwj in 1:n_state)
{
    for(iwk in 1:20)
    {
        dum = gender_gap_fore_fun_int(CDF_M_data = subnational_male_prefecture_dx_CDF[[iwj]], 
                                      CDF_F_data = subnational_female_prefecture_dx_CDF[[iwj]],
                                      PDF_M_holdout_data = male_prefecture_dx[[iwj]], 
                                      PDF_F_holdout_data = female_prefecture_dx[[iwj]],
                                      horizon = iwk, fore_method = "ets", level_sig = 0.8)
        gender_gap_ets_int_F[iwj,iwk,] = dum$int_F_err
        gender_gap_ets_int_M[iwj,iwk,] = dum$int_M_err
        rm(iwk)
    }
    print(iwj); rm(iwj)
}

horizon_gender_gap_ets_int_F_err_mean = apply(gender_gap_ets_int_F, c(2, 3), mean)
horizon_gender_gap_ets_int_M_err_mean = apply(gender_gap_ets_int_M, c(2, 3), mean)

gender_gap_ets_int_F_err_mean = apply(gender_gap_ets_int_F, c(1, 3), mean)
gender_gap_ets_int_M_err_mean = apply(gender_gap_ets_int_M, c(1, 3), mean)

# fmethod = "arima"

gender_gap_arima_int_F = gender_gap_arima_int_M = array(NA, dim = c(n_state, 20, 3), dimnames = list(state, 1:20, c("ECP", "CPD", "MIS")))
for(iwj in 1:n_state)
{
    for(iwk in 1:20)
    {
        dum = gender_gap_fore_fun_int(CDF_M_data = subnational_male_prefecture_dx_CDF[[iwj]], 
                                      CDF_F_data = subnational_female_prefecture_dx_CDF[[iwj]],
                                      PDF_M_holdout_data = male_prefecture_dx[[iwj]], 
                                      PDF_F_holdout_data = female_prefecture_dx[[iwj]],
                                      horizon = iwk, fore_method = "arima", level_sig = 0.8)
        gender_gap_arima_int_F[iwj,iwk,] = dum$int_F_err
        gender_gap_arima_int_M[iwj,iwk,] = dum$int_M_err
        rm(iwk)
    }
    print(iwj); rm(iwj)
}

horizon_gender_gap_arima_int_F_err_mean = apply(gender_gap_arima_int_F, c(2, 3), mean)
horizon_gender_gap_arima_int_M_err_mean = apply(gender_gap_arima_int_M, c(2, 3), mean)

gender_gap_arima_int_F_err_mean = apply(gender_gap_arima_int_F, c(1, 3), mean)
gender_gap_arima_int_M_err_mean = apply(gender_gap_arima_int_M, c(1, 3), mean)

################
## alpha = 0.05
################

# fmethod = "ets"

gender_gap_ets_int_F_95 = gender_gap_ets_int_M_95 = array(NA, dim = c(n_state, 20, 3), 
                                                          dimnames = list(state, 1:20, c("ECP", "CPD", "MIS")))
for(iwj in 1:n_state)
{
    for(iwk in 1:20)
    {
        dum = gender_gap_fore_fun_int(CDF_M_data = subnational_male_prefecture_dx_CDF[[iwj]], 
                                      CDF_F_data = subnational_female_prefecture_dx_CDF[[iwj]],
                                      PDF_M_holdout_data = male_prefecture_dx[[iwj]], 
                                      PDF_F_holdout_data = female_prefecture_dx[[iwj]],
                                      horizon = iwk, fore_method = "ets", level_sig = 0.95)
        gender_gap_ets_int_F_95[iwj,iwk,] = dum$int_F_err
        gender_gap_ets_int_M_95[iwj,iwk,] = dum$int_M_err
        rm(iwk)
    }
    print(iwj); rm(iwj)
}

horizon_gender_gap_ets_int_F_95_err_mean = apply(gender_gap_ets_int_F_95, c(2, 3), mean)
horizon_gender_gap_ets_int_M_95_err_mean = apply(gender_gap_ets_int_M_95, c(2, 3), mean)

gender_gap_ets_int_F_95_err_mean = apply(gender_gap_ets_int_F_95, c(1, 3), mean)
gender_gap_ets_int_M_95_err_mean = apply(gender_gap_ets_int_M_95, c(1, 3), mean)

# fmethod = "arima"

gender_gap_arima_int_F_95 = gender_gap_arima_int_M_95 = array(NA, dim = c(n_state, 20, 3), dimnames = list(state, 1:20, c("ECP", "CPD", "MIS")))
for(iwj in 1:n_state)
{
    for(iwk in 1:20)
    {
        dum = gender_gap_fore_fun_int(CDF_M_data = subnational_male_prefecture_dx_CDF[[iwj]], 
                                      CDF_F_data = subnational_female_prefecture_dx_CDF[[iwj]],
                                      PDF_M_holdout_data = male_prefecture_dx[[iwj]], 
                                      PDF_F_holdout_data = female_prefecture_dx[[iwj]],
                                      horizon = iwk, fore_method = "arima", level_sig = 0.95)
        gender_gap_arima_int_F_95[iwj,iwk,] = dum$int_F_err
        gender_gap_arima_int_M_95[iwj,iwk,] = dum$int_M_err
        rm(iwk)
    }
    print(iwj); rm(iwj)
}

horizon_gender_gap_arima_int_F_95_err_mean = apply(gender_gap_arima_int_F_95, c(2, 3), mean)
horizon_gender_gap_arima_int_M_95_err_mean = apply(gender_gap_arima_int_M_95, c(2, 3), mean)

gender_gap_arima_int_F_95_err_mean = apply(gender_gap_arima_int_F_95, c(1, 3), mean)
gender_gap_arima_int_M_95_err_mean = apply(gender_gap_arima_int_M_95, c(1, 3), mean)

