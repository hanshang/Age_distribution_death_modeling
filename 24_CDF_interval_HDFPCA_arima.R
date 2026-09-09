# load R packages

source("load_packages.R")
source("auxiliary_interval.R")
source("hdfpca_fun.R")

# fdata_F: female data
# fdata_M: male data
# horizon: forecast horizon
# first_order: number of components
# second_order: number of components
# transformation: type of transformation
# level_sig: level of significance
# uni_fore_method: univariate time-series forecasting method

int_hdfpca_fun <- function(fdata_F, fdata_M, horizon, first_order, second_order, transformation, level_sig, uni_fore_method)
{
    n_year = nrow(fdata_F)
    n_age = ncol(fdata_F)
    forecast_validation_F = forecast_validation_M = matrix(NA, ncol(fdata_F), (22 - horizon))
    if(transformation == "direct")
    {
        for(ij in 1:(22 - horizon))
        {
            data_F = fdata_F[1:(n_year - 42 + ij),]
            data_M = fdata_M[1:(n_year - 42 + ij),]
            data_comb = list()
            data_comb[[1]] = t(data_F)
            data_comb[[2]] = t(data_M)
            
            fore_val = forecast.hdfpca(hdfpca(y = data_comb, order = first_order, r = second_order), h = horizon, fmethod = uni_fore_method)$forecast
            forecast_validation_F[,ij] = (fore_val[[1]])[,horizon]
            forecast_validation_M[,ij] = (fore_val[[2]])[,horizon]
            rm(ij); rm(fore_val)
        }
    }
    else if(transformation == "CLR")
    {
        for(ij in 1:(22 - horizon))
        {
            data_F = as.matrix(clr(fdata_F[1:(n_year - 42 + ij),]))
            data_M = as.matrix(clr(fdata_M[1:(n_year - 42 + ij),]))
            data_comb = list()
            data_comb[[1]] = t(data_F)
            data_comb[[2]] = t(data_M)
            
            fore_val = forecast.hdfpca(hdfpca(y = data_comb, order = first_order, r = second_order), h = horizon, fmethod = uni_fore_method)$forecast
            forecast_validation_F[,ij] = as.numeric(clrInv((fore_val[[1]])[,horizon])) * 10^5
            forecast_validation_M[,ij] = as.numeric(clrInv((fore_val[[2]])[,horizon])) * 10^5
            rm(ij); rm(fore_val)
        }	
    }
    else if(transformation == "CDF")
    {
        for(ijk in 1:(22 - horizon))
        {
            data_F = fdata_F[1:(n_year - 42 + ijk),]/10^5
            data_M = fdata_M[1:(n_year - 42 + ijk),]/10^5
            
            data_F_cumsum_dum = data_M_cumsum_dum = matrix(NA, nrow(data_F), ncol(data_F))
            for(iw in 1:nrow(data_F))
            {
                data_F_cumsum_dum[iw,] = cumsum(data_F[iw,])
                data_M_cumsum_dum[iw,] = cumsum(data_M[iw,])
                rm(iw)
            }
            
            # check if any cumsum values equal to 0
            if(any(data_F_cumsum_dum == 0))
            {
                data_F_cumsum = replace(data_F_cumsum_dum, which(data_F_cumsum_dum == 0), 10^-5)
            }
            else
            {
                data_F_cumsum = data_F_cumsum_dum
            }
            
            if(any(data_M_cumsum_dum == 0))
            {
                data_M_cumsum = replace(data_M_cumsum_dum, which(data_M_cumsum_dum == 0), 10^-5)
            }
            else
            {
                data_M_cumsum = data_M_cumsum_dum
            }
            rm(data_F_cumsum_dum); rm(data_M_cumsum_dum)
            
            # logit transformation
            
            data_F_cumsum_logit = data_M_cumsum_logit = matrix(NA, nrow(data_F), (ncol(data_F) - 1))
            for(ij in 1:nrow(data_F))
            {
                data_F_cumsum_logit[ij,] = logit(data_F_cumsum[ij, 1:(ncol(data_F) - 1)])
                data_M_cumsum_logit[ij,] = logit(data_M_cumsum[ij, 1:(ncol(data_M) - 1)])
                rm(ij)
            }
            
            data_comb = list()
            data_comb[[1]] = t(data_F_cumsum_logit)
            data_comb[[2]] = t(data_M_cumsum_logit)
            
            fore_val = forecast.hdfpca(hdfpca(y = data_comb, order = first_order, r = second_order), h = horizon, fmethod = uni_fore_method)$forecast
            fore_val_F = (fore_val[[1]])[,horizon]
            fore_val_M = (fore_val[[2]])[,horizon]
            
            data_cumsum_logit_fore_add_F = c(invlogit(fore_val_F), 1)
            data_cumsum_logit_fore_add_M = c(invlogit(fore_val_M), 1)
            
            data_cumsum_logit_fore_add_diff_F = c(data_cumsum_logit_fore_add_F[1], diff(data_cumsum_logit_fore_add_F))
            data_cumsum_logit_fore_add_diff_M = c(data_cumsum_logit_fore_add_M[1], diff(data_cumsum_logit_fore_add_M))
            
            forecast_validation_F[,ijk] = data_cumsum_logit_fore_add_diff_F * 10^5
            forecast_validation_M[,ijk] = data_cumsum_logit_fore_add_diff_M * 10^5
            rm(ijk); rm(data_F); rm(data_M); rm(fore_val)
        }
    }
    else
    {
        warning("none, CLR, CDF transformation allowed only.")
    }
    
    # holdout validation data
    
    holdout_validation_F = t(matrix(fdata_F[(n_year - 41 + horizon):(n_year - 20),], length((n_year - 41 + horizon):(n_year - 20)), ncol(fdata_F)))
    holdout_validation_M = t(matrix(fdata_M[(n_year - 41 + horizon):(n_year - 20),], length((n_year - 41 + horizon):(n_year - 20)), ncol(fdata_M)))
    resi_mat_F = holdout_validation_F - forecast_validation_F
    resi_mat_M = holdout_validation_M - forecast_validation_M
    
    # compute standard deviation of residuals
    
    sd_val_F = apply(resi_mat_F, 1, sd)
    sd_val_M = apply(resi_mat_M, 1, sd)
    
    # find the optimal tuning parameter
    
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
    
    # male
    
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
    rm(obj_val)
    
    forecast_test_F = forecast_test_F_lb = forecast_test_F_ub = matrix(NA, ncol(fdata_F), (21 - horizon))
    forecast_test_M = forecast_test_M_lb = forecast_test_M_ub = matrix(NA, ncol(fdata_M), (21 - horizon))
    if(transformation == "direct")
    {
        for(ij in 1:(21 - horizon))
        {
            data_F = fdata_F[1:(n_year - 21 + ij),]
            data_M = fdata_M[1:(n_year - 21 + ij),]
            data_comb = list()
            data_comb[[1]] = t(data_F)
            data_comb[[2]] = t(data_M)
            
            fore_val = forecast.hdfpca(hdfpca(y = data_comb, order = first_order, r = second_order), h = horizon, fmethod = uni_fore_method)$forecast
            forecast_test_F[,ij] = (fore_val[[1]])[,horizon]
            forecast_test_F_lb[,ij] = forecast_test_F[,ij] - tune_para_find_F * sd_val_F
            forecast_test_F_ub[,ij] = forecast_test_F[,ij] + tune_para_find_F * sd_val_F
            
            forecast_test_M[,ij] = (fore_val[[2]])[,horizon]
            forecast_test_M_lb[,ij] = forecast_test_M[,ij] - tune_para_find_M * sd_val_M
            forecast_test_M_ub[,ij] = forecast_test_M[,ij] + tune_para_find_M * sd_val_M
            rm(ij)
        }		
    }
    else if(transformation == "CLR")
    {
        for(ij in 1:(21 - horizon))
        {
            data_F = as.matrix(clr(fdata_F[1:(n_year - 21 + ij),]))
            data_M = as.matrix(clr(fdata_M[1:(n_year - 21 + ij),]))
            data_comb = list()
            data_comb[[1]] = t(data_F)
            data_comb[[2]] = t(data_M)
            
            fore_val = forecast.hdfpca(hdfpca(y = data_comb, order = first_order, r = second_order), h = horizon, fmethod = uni_fore_method)$forecast
            forecast_test_F[,ij] = as.numeric(clrInv((fore_val[[1]])[,horizon])) * 10^5
            forecast_test_F_lb[,ij] = forecast_test_F[,ij] - tune_para_find_F * sd_val_F
            forecast_test_F_ub[,ij] = forecast_test_F[,ij] + tune_para_find_F * sd_val_F
            
            forecast_test_M[,ij] = as.numeric(clrInv((fore_val[[2]])[,horizon])) * 10^5
            forecast_test_M_lb[,ij] = forecast_test_M[,ij] - tune_para_find_M * sd_val_M
            forecast_test_M_ub[,ij] = forecast_test_M[,ij] + tune_para_find_M * sd_val_M
            rm(ij); rm(fore_val)
        }	
    }
    else if(transformation == "CDF")
    {
        for(ijk in 1:(21 - horizon))
        {
            data_F = fdata_F[1:(n_year - 21 + ijk),]/10^5
            data_M = fdata_M[1:(n_year - 21 + ijk),]/10^5
            
            data_F_cumsum_dum = data_M_cumsum_dum = matrix(NA, nrow(data_F), ncol(data_F))
            for(iw in 1:nrow(data_F))
            {
                data_F_cumsum_dum[iw,] = cumsum(data_F[iw,])
                data_M_cumsum_dum[iw,] = cumsum(data_M[iw,])
                rm(iw)
            }
            
            # check if any cumsum values equal to 0
            if(any(data_F_cumsum_dum == 0))
            {
                data_F_cumsum = replace(data_F_cumsum_dum, which(data_F_cumsum_dum == 0), 10^-5)
            }
            else
            {
                data_F_cumsum = data_F_cumsum_dum
            }
            
            if(any(data_M_cumsum_dum == 0))
            {
                data_M_cumsum = replace(data_M_cumsum_dum, which(data_M_cumsum_dum == 0), 10^-5)
            }
            else
            {
                data_M_cumsum = data_M_cumsum_dum
            }
            rm(data_F_cumsum_dum); rm(data_M_cumsum_dum)
            
            # logit transformation
            
            data_F_cumsum_logit = data_M_cumsum_logit = matrix(NA, nrow(data_F), (ncol(data_F) - 1))
            for(ij in 1:nrow(data_F))
            {
                data_F_cumsum_logit[ij,] = logit(data_F_cumsum[ij, 1:(ncol(data_F) - 1)])
                data_M_cumsum_logit[ij,] = logit(data_M_cumsum[ij, 1:(ncol(data_M) - 1)])
                rm(ij)
            }
            
            data_comb = list()
            data_comb[[1]] = t(data_F_cumsum_logit)
            data_comb[[2]] = t(data_M_cumsum_logit)
            
            fore_val = forecast.hdfpca(hdfpca(y = data_comb, order = first_order, r = second_order), h = horizon, fmethod = uni_fore_method)$forecast
            fore_val_F = (fore_val[[1]])[,horizon]
            fore_val_M = (fore_val[[2]])[,horizon]
            
            data_cumsum_logit_fore_add_F = c(invlogit(fore_val_F), 1)
            data_cumsum_logit_fore_add_M = c(invlogit(fore_val_M), 1)
            
            data_cumsum_logit_fore_add_diff_F = c(data_cumsum_logit_fore_add_F[1], diff(data_cumsum_logit_fore_add_F))
            data_cumsum_logit_fore_add_diff_M = c(data_cumsum_logit_fore_add_M[1], diff(data_cumsum_logit_fore_add_M))
            
            forecast_test_F[,ijk] = data_cumsum_logit_fore_add_diff_F * 10^5
            forecast_test_F_lb[,ijk] = forecast_test_F[,ijk] - tune_para_find_F * sd_val_F
            forecast_test_F_ub[,ijk] = forecast_test_F[,ijk] + tune_para_find_F * sd_val_F
            
            forecast_test_M[,ijk] = data_cumsum_logit_fore_add_diff_M * 10^5
            forecast_test_M_lb[,ijk] = forecast_test_M[,ijk] - tune_para_find_M * sd_val_M
            forecast_test_M_ub[,ijk] = forecast_test_M[,ijk] + tune_para_find_M * sd_val_M
            rm(ijk); rm(data_F); rm(data_M)
        }
    }	
    
    # holdout testing data
    
    holdout_val_F = t(matrix(fdata_F[(n_year - 20 + horizon):n_year,], length((n_year - 20 + horizon):n_year), ncol(fdata_F)))
    holdout_val_M = t(matrix(fdata_M[(n_year - 20 + horizon):n_year,], length((n_year - 20 + horizon):n_year), ncol(fdata_M)))
    
    int_F_err = interval_score(holdout = holdout_val_F, lb = forecast_test_F_lb, ub = forecast_test_F_ub, alpha = (1 - level_sig))
    int_M_err = interval_score(holdout = holdout_val_M, lb = forecast_test_M_lb, ub = forecast_test_M_ub, alpha = (1 - level_sig))

    return(list(int_F_err = int_F_err, tune_para_find_F = tune_para_find_F, 
                tune_para_find_F_obj = obj_val_min_F,
                
                int_M_err = int_M_err, tune_para_find_M = tune_para_find_M,
                tune_para_find_M_obj = obj_val_min_M))    
}

###############################
## level of significance = 0.8
###############################

hdfpca_int_fore_subnational_err_F_EVR_ARIMA = hdfpca_int_fore_subnational_err_M_EVR_ARIMA = array(NA, dim = c(n_state, 20, 3), dimnames = list(state, 1:20, c("ECP", "CPD", "score")))
hdfpca_int_fore_subnational_err_F_EVR_ARIMA_tune_para = hdfpca_int_fore_subnational_err_F_EVR_ARIMA_tune_para_obj = 
hdfpca_int_fore_subnational_err_M_EVR_ARIMA_tune_para = hdfpca_int_fore_subnational_err_M_EVR_ARIMA_tune_para_obj = matrix(NA, n_state, 20)
for(ij in 1:n_state)
{
    for(iw in 1:20)
    {
        dum = int_hdfpca_fun(fdata_F = female_prefecture_dx[[ij]], fdata_M = male_prefecture_dx[[ij]], 
                             horizon = iw, first_order = 6, second_order = 2, transformation = "CDF", 
                             level_sig = 0.8, uni_fore_method = "arima")
        hdfpca_int_fore_subnational_err_F_EVR_ARIMA[ij,iw,] = dum$int_F_err
        hdfpca_int_fore_subnational_err_F_EVR_ARIMA_tune_para[ij,iw] = dum$tune_para_find_F
        hdfpca_int_fore_subnational_err_F_EVR_ARIMA_tune_para_obj[ij,iw] = dum$tune_para_find_F_obj
        
        hdfpca_int_fore_subnational_err_M_EVR_ARIMA[ij,iw,] = dum$int_M_err
        hdfpca_int_fore_subnational_err_M_EVR_ARIMA_tune_para[ij,iw] = dum$tune_para_find_M
        hdfpca_int_fore_subnational_err_M_EVR_ARIMA_tune_para_obj[ij,iw] = dum$tune_para_find_M_obj
        rm(dum); rm(iw)
    }
    print(ij); rm(ij)
}

horizon_specific_hdfpca_int_fore_subnational_err_F_EVR_ARIMA = apply(hdfpca_int_fore_subnational_err_F_EVR_ARIMA, c(2, 3), mean)
horizon_specific_hdfpca_int_fore_subnational_err_M_EVR_ARIMA = apply(hdfpca_int_fore_subnational_err_M_EVR_ARIMA, c(2, 3), mean)

horizon_specific_hdfpca_int_fore_subnational_err_F_EVR_ARIMA_prefecture = apply(hdfpca_int_fore_subnational_err_F_EVR_ARIMA, c(1, 3), mean)
horizon_specific_hdfpca_int_fore_subnational_err_M_EVR_ARIMA_prefecture = apply(hdfpca_int_fore_subnational_err_M_EVR_ARIMA, c(1, 3), mean)


################################
## level of significance = 0.95
################################

hdfpca_int_fore_subnational_err_F_EVR_ARIMA_alpha_0.95 = hdfpca_int_fore_subnational_err_M_EVR_ARIMA_alpha_0.95 = array(NA, dim = c(n_state, 20, 3), dimnames = list(state, 1:20, c("ECP", "CPD", "score")))
hdfpca_int_fore_subnational_err_F_EVR_ARIMA_tune_para_alpha_0.95 = hdfpca_int_fore_subnational_err_F_EVR_ARIMA_tune_para_obj_alpha_0.95 = 
hdfpca_int_fore_subnational_err_M_EVR_ARIMA_tune_para_alpha_0.95 = hdfpca_int_fore_subnational_err_M_EVR_ARIMA_tune_para_obj_alpha_0.95 = matrix(NA, n_state, 20)
for(ij in 1:n_state)
{
    for(iw in 1:20)
    {
        dum = int_hdfpca_fun(fdata_F = female_prefecture_dx[[ij]], fdata_M = male_prefecture_dx[[ij]], 
                             horizon = iw, first_order = 6, second_order = 2, transformation = "CDF", 
                             level_sig = 0.95, uni_fore_method = "arima")
        hdfpca_int_fore_subnational_err_F_EVR_ARIMA_alpha_0.95[ij,iw,] = dum$int_F_err
        hdfpca_int_fore_subnational_err_F_EVR_ARIMA_tune_para_alpha_0.95[ij,iw] = dum$tune_para_find_F
        hdfpca_int_fore_subnational_err_F_EVR_ARIMA_tune_para_obj_alpha_0.95[ij,iw] = dum$tune_para_find_F_obj
        
        hdfpca_int_fore_subnational_err_M_EVR_ARIMA_alpha_0.95[ij,iw,] = dum$int_M_err
        hdfpca_int_fore_subnational_err_M_EVR_ARIMA_tune_para_alpha_0.95[ij,iw] = dum$tune_para_find_M
        hdfpca_int_fore_subnational_err_M_EVR_ARIMA_tune_para_obj_alpha_0.95[ij,iw] = dum$tune_para_find_M_obj
        rm(dum); print(iw); rm(iw)
    }
    print(ij); rm(ij)
}

horizon_specific_hdfpca_int_fore_subnational_err_F_EVR_ARIMA_alpha_0.95 = apply(hdfpca_int_fore_subnational_err_F_EVR_ARIMA_alpha_0.95, c(2, 3), mean)
horizon_specific_hdfpca_int_fore_subnational_err_M_EVR_ARIMA_alpha_0.95 = apply(hdfpca_int_fore_subnational_err_M_EVR_ARIMA_alpha_0.95, c(2, 3), mean)

horizon_specific_hdfpca_int_fore_subnational_err_F_EVR_ARIMA_alpha_0.95_prefecture = apply(hdfpca_int_fore_subnational_err_F_EVR_ARIMA_alpha_0.95, c(1, 3), mean)
horizon_specific_hdfpca_int_fore_subnational_err_M_EVR_ARIMA_alpha_0.95_prefecture = apply(hdfpca_int_fore_subnational_err_M_EVR_ARIMA_alpha_0.95, c(1, 3), mean)

