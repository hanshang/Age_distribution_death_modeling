#########
### clr
#########

## level_sig = 0.8

# EVR

int_fore_subnational_err_F_EVR_ARIMA_CLR = int_fore_subnational_err_M_EVR_ARIMA_CLR = array(NA, dim = c(n_state, 20, 3), dimnames = list(state, 1:20, c("ECP", "CPD", "score")))
int_fore_subnational_err_F_EVR_ARIMA_tune_para_CLR = int_fore_subnational_err_F_EVR_ARIMA_tune_para_obj_CLR = 
int_fore_subnational_err_M_EVR_ARIMA_tune_para_CLR = int_fore_subnational_err_M_EVR_ARIMA_tune_para_obj_CLR = matrix(NA, n_state, 20)
for(ij in 1:n_state)
{
    female_data = replace(female_prefecture_dx[[ij]], which(female_prefecture_dx[[ij]] == 0), 10^-5)
    male_data   = replace(male_prefecture_dx[[ij]],   which(male_prefecture_dx[[ij]] == 0),   10^-5)
    for(iw in 1:20)
    {
        ## (F)
        
        dum = interval_fore_subnational_cdf(fdata = female_data, method_ncomp = "EVR",
                                            horizon = iw, fore_method = "CLR", uni_fore_method = "ARIMA",
                                            level_sig = 0.8)
        int_fore_subnational_err_F_EVR_ARIMA_CLR[ij,iw,] = dum$int_err
        int_fore_subnational_err_F_EVR_ARIMA_tune_para_CLR[ij,iw] = dum$tune_para_find
        int_fore_subnational_err_F_EVR_ARIMA_tune_para_obj_CLR[ij,iw] = dum$tune_para_find_obj
        rm(dum)
        
        ## (M)
        
        dum = interval_fore_subnational_cdf(fdata = male_data, method_ncomp = "EVR",
                                            horizon = iw, fore_method = "CLR", uni_fore_method = "ARIMA",
                                            level_sig = 0.8)
        int_fore_subnational_err_M_EVR_ARIMA_CLR[ij,iw,] = dum$int_err
        int_fore_subnational_err_M_EVR_ARIMA_tune_para_CLR[ij,iw] = dum$tune_para_find
        int_fore_subnational_err_M_EVR_ARIMA_tune_para_obj_CLR[ij,iw] = dum$tune_para_find_obj
        rm(dum); rm(iw)
    }
    print(ij); rm(ij); rm(female_data); rm(male_data)
}

horizon_specific_int_fore_subnational_err_F_EVR_ARIMA_CLR = apply(int_fore_subnational_err_F_EVR_ARIMA_CLR, c(2, 3), mean)
horizon_specific_int_fore_subnational_err_M_EVR_ARIMA_CLR = apply(int_fore_subnational_err_M_EVR_ARIMA_CLR, c(2, 3), mean)

horizon_specific_int_fore_subnational_err_F_EVR_ARIMA_CLR_prefecture = apply(int_fore_subnational_err_F_EVR_ARIMA_CLR, c(1, 3), mean)
horizon_specific_int_fore_subnational_err_M_EVR_ARIMA_CLR_prefecture = apply(int_fore_subnational_err_M_EVR_ARIMA_CLR, c(1, 3), mean)

# K = 6

int_fore_subnational_err_F_K6_ARIMA_CLR = int_fore_subnational_err_M_K6_ARIMA_CLR = array(NA, dim = c(n_state, 20, 3), dimnames = list(state, 1:20, c("ECP", "CPD", "score")))
int_fore_subnational_err_F_K6_ARIMA_tune_para_CLR = int_fore_subnational_err_F_K6_ARIMA_tune_para_obj_CLR = 
int_fore_subnational_err_M_K6_ARIMA_tune_para_CLR = int_fore_subnational_err_M_K6_ARIMA_tune_para_obj_CLR = matrix(NA, n_state, 20)
for(ij in 1:n_state)
{
    female_data = replace(female_prefecture_dx[[ij]], which(female_prefecture_dx[[ij]] == 0), 10^-5)
    male_data   = replace(male_prefecture_dx[[ij]],   which(male_prefecture_dx[[ij]] == 0),   10^-5)
    for(iw in 1:20)
    {
        ## (F)
        
        dum = interval_fore_subnational_cdf(fdata = female_data, method_ncomp = "fixed",
                                            horizon = iw, fore_method = "CLR", uni_fore_method = "ARIMA",
                                            level_sig = 0.8)
        int_fore_subnational_err_F_K6_ARIMA_CLR[ij,iw,] = dum$int_err
        int_fore_subnational_err_F_K6_ARIMA_tune_para_CLR[ij,iw] = dum$tune_para_find
        int_fore_subnational_err_F_K6_ARIMA_tune_para_obj_CLR[ij,iw] = dum$tune_para_find_obj
        rm(dum)
        
        ## (M)
        
        dum = interval_fore_subnational_cdf(fdata = male_data, method_ncomp = "fixed",
                                            horizon = iw, fore_method = "CLR", uni_fore_method = "ARIMA",
                                            level_sig = 0.8)
        int_fore_subnational_err_M_K6_ARIMA_CLR[ij,iw,] = dum$int_err
        int_fore_subnational_err_M_K6_ARIMA_tune_para_CLR[ij,iw] = dum$tune_para_find
        int_fore_subnational_err_M_K6_ARIMA_tune_para_obj_CLR[ij,iw] = dum$tune_para_find_obj
        rm(dum); rm(iw)
    }
    print(ij); rm(ij); rm(female_data); rm(male_data)
}

horizon_specific_int_fore_subnational_err_F_K6_ARIMA_CLR = apply(int_fore_subnational_err_F_K6_ARIMA_CLR, c(2, 3), mean)
horizon_specific_int_fore_subnational_err_M_K6_ARIMA_CLR = apply(int_fore_subnational_err_M_K6_ARIMA_CLR, c(2, 3), mean)

horizon_specific_int_fore_subnational_err_F_K6_ARIMA_CLR_prefecture = apply(int_fore_subnational_err_F_K6_ARIMA_CLR, c(1, 3), mean)
horizon_specific_int_fore_subnational_err_M_K6_ARIMA_CLR_prefecture = apply(int_fore_subnational_err_M_K6_ARIMA_CLR, c(1, 3), mean)

## level_sig = 0.95

## EVR

int_fore_subnational_err_F_EVR_ARIMA_CLR_alpha_0.95 = int_fore_subnational_err_M_EVR_ARIMA_CLR_alpha_0.95 = array(NA, dim = c(n_state, 20, 3), dimnames = list(state, 1:20, c("ECP", "CPD", "score")))
int_fore_subnational_err_F_EVR_ARIMA_tune_para_CLR_alpha_0.95 = int_fore_subnational_err_F_EVR_ARIMA_tune_para_obj_CLR_alpha_0.95 = 
int_fore_subnational_err_M_EVR_ARIMA_tune_para_CLR_alpha_0.95 = int_fore_subnational_err_M_EVR_ARIMA_tune_para_obj_CLR_alpha_0.95 = matrix(NA, n_state, 20)
for(ij in 1:n_state)
{
    female_data = replace(female_prefecture_dx[[ij]], which(female_prefecture_dx[[ij]] == 0), 10^-5)
    male_data   = replace(male_prefecture_dx[[ij]],   which(male_prefecture_dx[[ij]] == 0),   10^-5)
    for(iw in 1:20)
    {
        ## (F)
        
        dum = interval_fore_subnational_cdf(fdata = female_data, method_ncomp = "EVR",
                                            horizon = iw, fore_method = "CLR", uni_fore_method = "ARIMA",
                                            level_sig = 0.95)
        int_fore_subnational_err_F_EVR_ARIMA_CLR_alpha_0.95[ij,iw,] = dum$int_err
        int_fore_subnational_err_F_EVR_ARIMA_tune_para_CLR_alpha_0.95[ij,iw] = dum$tune_para_find
        int_fore_subnational_err_F_EVR_ARIMA_tune_para_obj_CLR_alpha_0.95[ij,iw] = dum$tune_para_find_obj
        rm(dum)
        
        ## (M)
        
        dum = interval_fore_subnational_cdf(fdata = male_data, method_ncomp = "EVR",
                                            horizon = iw, fore_method = "CLR", uni_fore_method = "ARIMA",
                                            level_sig = 0.95)
        int_fore_subnational_err_M_EVR_ARIMA_CLR_alpha_0.95[ij,iw,] = dum$int_err
        int_fore_subnational_err_M_EVR_ARIMA_tune_para_CLR_alpha_0.95[ij,iw] = dum$tune_para_find
        int_fore_subnational_err_M_EVR_ARIMA_tune_para_obj_CLR_alpha_0.95[ij,iw] = dum$tune_para_find_obj
        rm(dum); rm(iw)
    }
    print(ij); rm(ij); rm(female_data); rm(male_data)
}

# by horizon

horizon_specific_int_fore_subnational_err_F_EVR_ARIMA_CLR_alpha_0.95 = apply(int_fore_subnational_err_F_EVR_ARIMA_CLR_alpha_0.95, c(2, 3), mean)
horizon_specific_int_fore_subnational_err_M_EVR_ARIMA_CLR_alpha_0.95 = apply(int_fore_subnational_err_M_EVR_ARIMA_CLR_alpha_0.95, c(2, 3), mean)

# by prefecture

horizon_specific_int_fore_subnational_err_F_EVR_ARIMA_CLR_alpha_0.95_prefecture = apply(int_fore_subnational_err_F_EVR_ARIMA_CLR_alpha_0.95, c(1, 3), mean)
horizon_specific_int_fore_subnational_err_M_EVR_ARIMA_CLR_alpha_0.95_prefecture = apply(int_fore_subnational_err_M_EVR_ARIMA_CLR_alpha_0.95, c(1, 3), mean)

## K = 6

int_fore_subnational_err_F_K6_ARIMA_CLR_alpha_0.95 = int_fore_subnational_err_M_K6_ARIMA_CLR_alpha_0.95 = array(NA, dim = c(47, 20, 3), dimnames = list(state, 1:20, c("ECP", "CPD", "score")))
int_fore_subnational_err_F_K6_ARIMA_tune_para_CLR_alpha_0.95 = int_fore_subnational_err_F_K6_ARIMA_tune_para_obj_CLR_alpha_0.95 = 
int_fore_subnational_err_M_K6_ARIMA_tune_para_CLR_alpha_0.95 = int_fore_subnational_err_M_K6_ARIMA_tune_para_obj_CLR_alpha_0.95 = matrix(NA, 47, 20)
for(ij in 1:n_state)
{
    female_data = replace(female_prefecture_dx[[ij]], which(female_prefecture_dx[[ij]] == 0), 10^-5)
    male_data   = replace(male_prefecture_dx[[ij]],   which(male_prefecture_dx[[ij]] == 0),   10^-5)
    for(iw in 1:20)
    {
        ## (F)
        
        dum = interval_fore_subnational_cdf(fdata = female_data, method_ncomp = "fixed",
                                            horizon = iw, fore_method = "CLR", uni_fore_method = "ARIMA",
                                            level_sig = 0.95)
        int_fore_subnational_err_F_K6_ARIMA_CLR_alpha_0.95[ij,iw,] = dum$int_err
        int_fore_subnational_err_F_K6_ARIMA_tune_para_CLR_alpha_0.95[ij,iw] = dum$tune_para_find
        int_fore_subnational_err_F_K6_ARIMA_tune_para_obj_CLR_alpha_0.95[ij,iw] = dum$tune_para_find_obj
        rm(dum)
        
        ## (M)
        
        dum = interval_fore_subnational_cdf(fdata = male_data, method_ncomp = "fixed",
                                            horizon = iw, fore_method = "CLR", uni_fore_method = "ARIMA",
                                            level_sig = 0.95)
        int_fore_subnational_err_M_K6_ARIMA_CLR_alpha_0.95[ij,iw,] = dum$int_err
        int_fore_subnational_err_M_K6_ARIMA_tune_para_CLR_alpha_0.95[ij,iw] = dum$tune_para_find
        int_fore_subnational_err_M_K6_ARIMA_tune_para_obj_CLR_alpha_0.95[ij,iw] = dum$tune_para_find_obj
        rm(dum); rm(iw)
    }
    print(ij); rm(ij); rm(female_data); rm(male_data)
}

# by horizon

horizon_specific_int_fore_subnational_err_F_K6_ARIMA_CLR_alpha_0.95 = apply(int_fore_subnational_err_F_K6_ARIMA_CLR_K6_alpha_0.95, c(2, 3), mean)
horizon_specific_int_fore_subnational_err_M_K6_ARIMA_CLR_alpha_0.95 = apply(int_fore_subnational_err_M_K6_ARIMA_CLR_K6_alpha_0.95, c(2, 3), mean)

# by prefecture

horizon_specific_int_fore_subnational_err_F_K6_ARIMA_CLR_alpha_0.95_prefecture = apply(int_fore_subnational_err_F_K6_ARIMA_CLR_K6_alpha_0.95, c(1, 3), mean)
horizon_specific_int_fore_subnational_err_M_K6_ARIMA_CLR_alpha_0.95_prefecture = apply(int_fore_subnational_err_M_K6_ARIMA_CLR_K6_alpha_0.95, c(1, 3), mean)

