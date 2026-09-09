#####################################
# train_set: 1:11 (1973:1983)
# validation_set: 12:32 (1984:2004)
# test_set: 33:52 (2005:2024)
#####################################

# fdata: functional data n by p
# method_ncomp: EVR or K = 6
# horizon: forecast horizon; when h = 1, there are 21 years; when h = 20, there are 2 years
# fore_method: transformation
# uni_fore_method: forecasting method
# level_sig: level of significance

interval_fore_subnational_cdf <- function(fdata, method_ncomp, horizon, fore_method, uni_fore_method, 
                                          level_sig)
{
    n_year = nrow(fdata)
    n_age = ncol(fdata)
    fore_validation = matrix(NA, ncol(fdata), (22 - horizon))
    if(fore_method == "CDF")
    {
        for(ij in 1:(22 - horizon))
        {
            fore_validation[,ij] = fore_national_cdf(data_set = fdata[1:(n_year - 42 + ij),], 
                                                     ncomp_method = method_ncomp,
                                                     fh = horizon, fmethod = uni_fore_method)
            rm(ij)            
        }
    }
    else if(fore_method == "CLR")
    {
        for(ij in 1:(22 - horizon))
        {
            fore_validation[,ij] = as.numeric(clr_fun(fdata = fdata[1:(n_year - 42 + ij),], 
                                                      ncomp_selection = method_ncomp,
                                                      fh = horizon, fore_method = uni_fore_method)$fore_count)
            rm(ij)
        }
    }
    else
    {
      warning("forecasting method must either be CDF or CLR.")
    }
    
    # holdout validation data
    
    holdout_validation_dum = t(matrix(fdata[(n_year - 41 + horizon):(n_year - 20),], length((n_year - 41 + horizon):(n_year - 20)), ncol(fdata)))
    resi_mat = holdout_validation_dum - fore_validation
    
    # compute standard deviation of residuals (require at least two years of observations)
    
    sd_val_input = apply(resi_mat, 1, sd)
    
    # find the optimal tuning parameter
    
    tune_para_find_val_1 = optimise(f = tune_para_find_function, interval = c(0, 1), 
                                    resi_mat = resi_mat, sd_val_input = sd_val_input, 
                                    alpha_level = level_sig)
    
    tune_para_find_val_2 = optimise(f = tune_para_find_function, interval = c(0, 5), 
                                    resi_mat = resi_mat, sd_val_input = sd_val_input, 
                                    alpha_level = level_sig)
    
    tune_para_find_val_3 = optimise(f = tune_para_find_function, interval = c(0, 10), 
                                    resi_mat = resi_mat, sd_val_input = sd_val_input, 
                                    alpha_level = level_sig)
    
    tune_para_find_val_4 = optimise(f = tune_para_find_function, interval = c(0, 20), 
                                    resi_mat = resi_mat, sd_val_input = sd_val_input, 
                                    alpha_level = level_sig)
    
    tune_para_find_val_5 = optim(par = 1, fn = tune_para_find_function, lower = 0, method = "L-BFGS-B", 
                                 resi_mat = resi_mat, sd_val_input = sd_val_input, 
                                 alpha_level = level_sig)
    
    tune_para_find_val_6 = optim(par = 1, fn = tune_para_find_function, method = "Nelder-Mead",
                                 resi_mat = resi_mat, sd_val_input = sd_val_input, 
                                 alpha_level = level_sig)
    
    obj_val = c(tune_para_find_val_1$objective, 
                tune_para_find_val_2$objective, 
                tune_para_find_val_3$objective,
                tune_para_find_val_4$objective,
                tune_para_find_val_5$value,
                tune_para_find_val_6$value)
    obj_val_min = min(obj_val)
    
    tune_para_find = c(tune_para_find_val_1$minimum, 
                       tune_para_find_val_2$minimum,
                       tune_para_find_val_3$minimum,
                       tune_para_find_val_4$minimum,
                       tune_para_find_val_5$par,
                       tune_para_find_val_6$par)[which.min(obj_val)]
    
    fore_val = fore_val_lb = fore_val_ub = matrix(NA, ncol(fdata), (21 - horizon))
    if(fore_method == "CDF")
    {
        for(ij in 1:(21 - horizon))
        {
            fore_val[,ij] <- fore_national_cdf(data_set = fdata[1:(n_year - 21 + ij),], ncomp_method = method_ncomp,
                                               fh = horizon, fmethod = uni_fore_method)
            fore_val_lb[,ij] = fore_val[,ij] - tune_para_find * sd_val_input
            fore_val_ub[,ij] = fore_val[,ij] + tune_para_find * sd_val_input
            rm(ij)
        }
    }
    else if(fore_method == "CLR")
    {
        for(ij in 1:(21 - horizon))
        {
            fore_val[,ij] = as.numeric(clr_fun(fdata = fdata[1:(n_year - 21 + ij),], ncomp_selection = method_ncomp,
                                               fh = horizon, fore_method = uni_fore_method)$fore_count)
            fore_val_lb[,ij] = fore_val[,ij] - tune_para_find * sd_val_input
            fore_val_ub[,ij] = fore_val[,ij] + tune_para_find * sd_val_input
            rm(ij)
        }
    }
    
    # holdout testing data
    
    holdout_val_dum = t(matrix(fdata[(n_year - 20 + horizon):n_year,], length((n_year - 20 + horizon):n_year), ncol(fdata)))
    
    int_err = interval_score(holdout = holdout_val_dum, lb = fore_val_lb, ub = fore_val_ub, alpha = (1 - level_sig))
    return(list(int_err = int_err, tune_para_find = tune_para_find, tune_para_find_obj = obj_val_min))
}


#########
### clr
#########

## level_sig = 0.8

# EVR

int_fore_subnational_err_F_EVR_ETS_CLR = int_fore_subnational_err_M_EVR_ETS_CLR = array(NA, dim = c(n_state, 20, 3), dimnames = list(state, 1:20, c("ECP", "CPD", "score")))
int_fore_subnational_err_F_EVR_ETS_tune_para_CLR = int_fore_subnational_err_F_EVR_ETS_tune_para_obj_CLR = 
int_fore_subnational_err_M_EVR_ETS_tune_para_CLR = int_fore_subnational_err_M_EVR_ETS_tune_para_obj_CLR = matrix(NA, n_state, 20)
for(ij in 1:n_state)
{
    female_data = replace(female_prefecture_dx[[ij]], which(female_prefecture_dx[[ij]] == 0), 10^-5)
    male_data   = replace(male_prefecture_dx[[ij]],   which(male_prefecture_dx[[ij]] == 0),   10^-5)
    for(iw in 1:20)
    {
        ## (F)
        
        dum = interval_fore_subnational_cdf(fdata = female_data, method_ncomp = "EVR",
                                            horizon = iw, fore_method = "CLR", uni_fore_method = "ETS",
                                            level_sig = 0.8)
        int_fore_subnational_err_F_EVR_ETS_CLR[ij,iw,] = dum$int_err
        int_fore_subnational_err_F_EVR_ETS_tune_para_CLR[ij,iw] = dum$tune_para_find
        int_fore_subnational_err_F_EVR_ETS_tune_para_obj_CLR[ij,iw] = dum$tune_para_find_obj
        rm(dum)
        
        ## (M)
        
        dum = interval_fore_subnational_cdf(fdata = male_data, method_ncomp = "EVR",
                                            horizon = iw, fore_method = "CLR", uni_fore_method = "ETS",
                                            level_sig = 0.8)
        int_fore_subnational_err_M_EVR_ETS_CLR[ij,iw,] = dum$int_err
        int_fore_subnational_err_M_EVR_ETS_tune_para_CLR[ij,iw] = dum$tune_para_find
        int_fore_subnational_err_M_EVR_ETS_tune_para_obj_CLR[ij,iw] = dum$tune_para_find_obj
        rm(dum); rm(iw)
    }
    print(ij); rm(ij); rm(female_data); rm(male_data)
}

horizon_specific_int_fore_subnational_err_F_EVR_ETS_CLR = apply(int_fore_subnational_err_F_EVR_ETS_CLR, c(2, 3), mean)
horizon_specific_int_fore_subnational_err_M_EVR_ETS_CLR = apply(int_fore_subnational_err_M_EVR_ETS_CLR, c(2, 3), mean)

horizon_specific_int_fore_subnational_err_F_EVR_ETS_CLR_prefecture = apply(int_fore_subnational_err_F_EVR_ETS_CLR, c(1, 3), mean)
horizon_specific_int_fore_subnational_err_M_EVR_ETS_CLR_prefecture = apply(int_fore_subnational_err_M_EVR_ETS_CLR, c(1, 3), mean)

# K = 6

int_fore_subnational_err_F_K6_ETS_CLR = int_fore_subnational_err_M_K6_ETS_CLR = array(NA, dim = c(n_state, 20, 3), dimnames = list(state, 1:20, c("ECP", "CPD", "score")))
int_fore_subnational_err_F_K6_ETS_tune_para_CLR = int_fore_subnational_err_F_K6_ETS_tune_para_obj_CLR = 
int_fore_subnational_err_M_K6_ETS_tune_para_CLR = int_fore_subnational_err_M_K6_ETS_tune_para_obj_CLR = matrix(NA, n_state, 20)
for(ij in 1:n_state)
{
    female_data = replace(female_prefecture_dx[[ij]], which(female_prefecture_dx[[ij]] == 0), 10^-5)
    male_data   = replace(male_prefecture_dx[[ij]],   which(male_prefecture_dx[[ij]] == 0),   10^-5)
    for(iw in 1:20)
    {
        ## (F)
        
        dum = interval_fore_subnational_cdf(fdata = female_data, method_ncomp = "fixed",
                                            horizon = iw, fore_method = "CLR", uni_fore_method = "ETS",
                                            level_sig = 0.8)
        int_fore_subnational_err_F_K6_ETS_CLR[ij,iw,] = dum$int_err
        int_fore_subnational_err_F_K6_ETS_tune_para_CLR[ij,iw] = dum$tune_para_find
        int_fore_subnational_err_F_K6_ETS_tune_para_obj_CLR[ij,iw] = dum$tune_para_find_obj
        rm(dum)
        
        ## (M)
        
        dum = interval_fore_subnational_cdf(fdata = male_data, method_ncomp = "fixed",
                                            horizon = iw, fore_method = "CLR", uni_fore_method = "ETS",
                                            level_sig = 0.8)
        int_fore_subnational_err_M_K6_ETS_CLR[ij,iw,] = dum$int_err
        int_fore_subnational_err_M_K6_ETS_tune_para_CLR[ij,iw] = dum$tune_para_find
        int_fore_subnational_err_M_K6_ETS_tune_para_obj_CLR[ij,iw] = dum$tune_para_find_obj
        rm(dum); rm(iw)
    }
    print(ij); rm(ij); rm(female_data); rm(male_data)
}

horizon_specific_int_fore_subnational_err_F_K6_ETS_CLR = apply(int_fore_subnational_err_F_K6_ETS_CLR, c(2, 3), mean)
horizon_specific_int_fore_subnational_err_M_K6_ETS_CLR = apply(int_fore_subnational_err_M_K6_ETS_CLR, c(2, 3), mean)

horizon_specific_int_fore_subnational_err_F_K6_ETS_CLR_prefecture = apply(int_fore_subnational_err_F_K6_ETS_CLR, c(1, 3), mean)
horizon_specific_int_fore_subnational_err_M_K6_ETS_CLR_prefecture = apply(int_fore_subnational_err_M_K6_ETS_CLR, c(1, 3), mean)

## level_sig = 0.95

# EVR

int_fore_subnational_err_F_EVR_ETS_CLR_alpha_0.95 = int_fore_subnational_err_M_EVR_ETS_CLR_alpha_0.95 = array(NA, dim = c(47, 20, 3), dimnames = list(state, 1:20, c("ECP", "CPD", "score")))
int_fore_subnational_err_F_EVR_ETS_tune_para_CLR_alpha_0.95 = int_fore_subnational_err_F_EVR_ETS_tune_para_obj_CLR_alpha_0.95 = 
int_fore_subnational_err_M_EVR_ETS_tune_para_CLR_alpha_0.95 = int_fore_subnational_err_M_EVR_ETS_tune_para_obj_CLR_alpha_0.95 = matrix(NA, 47, 20)
for(ij in 1:n_state)
{
    female_data = replace(female_prefecture_dx[[ij]], which(female_prefecture_dx[[ij]] == 0), 10^-5)
    male_data   = replace(male_prefecture_dx[[ij]],   which(male_prefecture_dx[[ij]] == 0),   10^-5)
    for(iw in 1:20)
    {
        ## (F)
        
        dum = interval_fore_subnational_cdf(fdata = female_data, method_ncomp = "EVR",
                                            horizon = iw, fore_method = "CLR", uni_fore_method = "ETS",
                                            level_sig = 0.95)
        int_fore_subnational_err_F_EVR_ETS_CLR_alpha_0.95[ij,iw,] = dum$int_err
        int_fore_subnational_err_F_EVR_ETS_tune_para_CLR_alpha_0.95[ij,iw] = dum$tune_para_find
        int_fore_subnational_err_F_EVR_ETS_tune_para_obj_CLR_alpha_0.95[ij,iw] = dum$tune_para_find_obj
        rm(dum)
        
        ## (M)
        
        dum = interval_fore_subnational_cdf(fdata = male_data, method_ncomp = "EVR",
                                            horizon = iw, fore_method = "CLR", uni_fore_method = "ETS",
                                            level_sig = 0.95)
        int_fore_subnational_err_M_EVR_ETS_CLR_alpha_0.95[ij,iw,] = dum$int_err
        int_fore_subnational_err_M_EVR_ETS_tune_para_CLR_alpha_0.95[ij,iw] = dum$tune_para_find
        int_fore_subnational_err_M_EVR_ETS_tune_para_obj_CLR_alpha_0.95[ij,iw] = dum$tune_para_find_obj
        rm(dum); rm(iw)
    }
    print(ij); rm(ij); rm(female_data); rm(male_data)
}

horizon_specific_int_fore_subnational_err_F_EVR_ETS_CLR_alpha_0.95 = apply(int_fore_subnational_err_F_EVR_ETS_CLR_alpha_0.95, c(2, 3), mean)
horizon_specific_int_fore_subnational_err_M_EVR_ETS_CLR_alpha_0.95 = apply(int_fore_subnational_err_M_EVR_ETS_CLR_alpha_0.95, c(2, 3), mean)

horizon_specific_int_fore_subnational_err_F_EVR_ETS_CLR_alpha_0.95_prefecture = apply(int_fore_subnational_err_F_EVR_ETS_CLR_alpha_0.95, c(1, 3), mean)
horizon_specific_int_fore_subnational_err_M_EVR_ETS_CLR_alpha_0.95_prefecture = apply(int_fore_subnational_err_M_EVR_ETS_CLR_alpha_0.95, c(1, 3), mean)

# K = 6

int_fore_subnational_err_F_K6_ETS_CLR_alpha_0.95 = int_fore_subnational_err_M_K6_ETS_CLR_alpha_0.95 = array(NA, dim = c(47, 20, 3), dimnames = list(state, 1:20, c("ECP", "CPD", "score")))
int_fore_subnational_err_F_K6_ETS_tune_para_CLR_alpha_0.95 = int_fore_subnational_err_F_K6_ETS_tune_para_obj_CLR_alpha_0.95 = 
int_fore_subnational_err_M_K6_ETS_tune_para_CLR_alpha_0.95 = int_fore_subnational_err_M_K6_ETS_tune_para_obj_CLR_alpha_0.95 = matrix(NA, 47, 20)
for(ij in 1:n_state)
{
    female_data = replace(female_prefecture_dx[[ij]], which(female_prefecture_dx[[ij]] == 0), 10^-5)
    male_data   = replace(male_prefecture_dx[[ij]],   which(male_prefecture_dx[[ij]] == 0),   10^-5)
    for(iw in 1:20)
    {
        ## (F)
        
        dum = interval_fore_subnational_cdf(fdata = female_data, method_ncomp = "fixed",
                                            horizon = iw, fore_method = "CLR", uni_fore_method = "ETS",
                                            level_sig = 0.95)
        int_fore_subnational_err_F_K6_ETS_CLR_alpha_0.95[ij,iw,] = dum$int_err
        int_fore_subnational_err_F_K6_ETS_tune_para_CLR_alpha_0.95[ij,iw] = dum$tune_para_find
        int_fore_subnational_err_F_K6_ETS_tune_para_obj_CLR_alpha_0.95[ij,iw] = dum$tune_para_find_obj
        rm(dum)
        
        ## (M)
        
        dum = interval_fore_subnational_cdf(fdata = male_data, method_ncomp = "fixed",
                                            horizon = iw, fore_method = "CLR", uni_fore_method = "ETS",
                                            level_sig = 0.95)
        int_fore_subnational_err_M_K6_ETS_CLR_alpha_0.95[ij,iw,] = dum$int_err
        int_fore_subnational_err_M_K6_ETS_tune_para_CLR_alpha_0.95[ij,iw] = dum$tune_para_find
        int_fore_subnational_err_M_K6_ETS_tune_para_obj_CLR_alpha_0.95[ij,iw] = dum$tune_para_find_obj
        rm(dum); rm(iw)
    }
    print(ij); rm(ij); rm(female_data); rm(male_data)
}

horizon_specific_int_fore_subnational_err_F_K6_ETS_CLR_K6_alpha_0.95 = apply(int_fore_subnational_err_F_K6_ETS_CLR_alpha_0.95, c(2, 3), mean)
horizon_specific_int_fore_subnational_err_M_K6_ETS_CLR_K6_alpha_0.95 = apply(int_fore_subnational_err_M_K6_ETS_CLR_alpha_0.95, c(2, 3), mean)

horizon_specific_int_fore_subnational_err_F_K6_ETS_CLR_K6_alpha_0.95_prefecture = apply(int_fore_subnational_err_F_K6_ETS_CLR_alpha_0.95, c(1, 3), mean)
horizon_specific_int_fore_subnational_err_M_K6_ETS_CLR_K6_alpha_0.95_prefecture = apply(int_fore_subnational_err_M_K6_ETS_CLR_alpha_0.95, c(1, 3), mean)

