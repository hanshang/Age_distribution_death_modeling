########
### CDF
########

## level_sig = 0.8

# EVR

int_fore_subnational_err_F_EVR_ARIMA = int_fore_subnational_err_M_EVR_ARIMA = array(NA, dim = c(20, 3, n_state), dimnames = list(1:20, c("ECP", "CPD", "MIS"), state))
int_fore_subnational_err_F_EVR_ARIMA_tune_para = int_fore_subnational_err_F_EVR_ARIMA_tune_para_obj = 
int_fore_subnational_err_M_EVR_ARIMA_tune_para = int_fore_subnational_err_M_EVR_ARIMA_tune_para_obj = matrix(NA, 20, n_state)
for(ij in 1:n_state)
{
    for(iw in 1:20)
    {
        ## Female
        
        dum = interval_fore_subnational_cdf(fdata = female_prefecture_dx[[ij]], method_ncomp = "EVR",
                                            horizon = iw, fore_method = "CDF", uni_fore_method = "arima",
                                            level_sig = 0.8)
        int_fore_subnational_err_F_EVR_ARIMA[iw,,ij] = dum$int_err
        int_fore_subnational_err_F_EVR_ARIMA_tune_para[iw,ij] = dum$tune_para_find
        int_fore_subnational_err_F_EVR_ARIMA_tune_para_obj[iw,ij] = dum$tune_para_find_obj
        rm(dum)
        
        ## Male
        
        dum = interval_fore_subnational_cdf(fdata = male_prefecture_dx[[ij]], method_ncomp = "EVR",
                                            horizon = iw, fore_method = "CDF", uni_fore_method = "arima",
                                            level_sig = 0.8)
        int_fore_subnational_err_M_EVR_ARIMA[iw,,ij] = dum$int_err
        int_fore_subnational_err_M_EVR_ARIMA_tune_para[iw,ij] = dum$tune_para_find
        int_fore_subnational_err_M_EVR_ARIMA_tune_para_obj[iw,ij] = dum$tune_para_find_obj
        rm(dum); print(iw); rm(iw)
    }
    print(ij); rm(ij)
}

int_fore_subnational_err_F_EVR_ARIMA_mean = apply(int_fore_subnational_err_F_EVR_ARIMA, c(1, 2), mean)
int_fore_subnational_err_M_EVR_ARIMA_mean = apply(int_fore_subnational_err_M_EVR_ARIMA, c(1, 2), mean)

colnames(int_fore_subnational_err_F_EVR_ARIMA_mean) = colnames(int_fore_subnational_err_M_EVR_ARIMA_mean) = c("ECP", "CPD", "score")
rownames(int_fore_subnational_err_F_EVR_ARIMA_mean) = rownames(int_fore_subnational_err_M_EVR_ARIMA_mean) = 1:20

int_fore_subnational_err_F_EVR_ARIMA_mean_prefecture = t(apply(int_fore_subnational_err_F_EVR_ARIMA, c(2, 3), mean))
int_fore_subnational_err_M_EVR_ARIMA_mean_prefecture = t(apply(int_fore_subnational_err_M_EVR_ARIMA, c(2, 3), mean))


# K = 6

int_fore_subnational_err_F_K6_ARIMA = int_fore_subnational_err_M_K6_ARIMA = array(NA, dim = c(20, 3, n_state), dimnames = list(1:20, c("ECP", "CPD", "MIS"), state))
int_fore_subnational_err_F_K6_ARIMA_tune_para = int_fore_subnational_err_F_K6_ARIMA_tune_para_obj = 
int_fore_subnational_err_M_K6_ARIMA_tune_para = int_fore_subnational_err_M_K6_ARIMA_tune_para_obj = matrix(NA, 20, n_state)
for(ij in 1:n_state)
{
    for(iw in 1:20)
    {
        ## Female
        
        dum = interval_fore_subnational_cdf(fdata = female_prefecture_dx[[ij]], method_ncomp = "provide",
                                            horizon = iw, fore_method = "CDF", uni_fore_method = "arima",
                                            level_sig = 0.8)
        int_fore_subnational_err_F_K6_ARIMA[iw,,ij] = dum$int_err
        int_fore_subnational_err_F_K6_ARIMA_tune_para[iw,ij] = dum$tune_para_find
        int_fore_subnational_err_F_K6_ARIMA_tune_para_obj[iw,ij] = dum$tune_para_find_obj
        rm(dum)
        
        ## Male
        
        dum = interval_fore_subnational_cdf(fdata = male_prefecture_dx[[ij]], method_ncomp = "provide",
                                            horizon = iw, fore_method = "CDF", uni_fore_method = "arima",
                                            level_sig = 0.8)
        int_fore_subnational_err_M_K6_ARIMA[iw,,ij] = dum$int_err
        int_fore_subnational_err_M_K6_ARIMA_tune_para[iw,ij] = dum$tune_para_find
        int_fore_subnational_err_M_K6_ARIMA_tune_para_obj[iw,ij] = dum$tune_para_find_obj
        rm(dum); print(iw); rm(iw)
    }
    print(ij); rm(ij)
}

int_fore_subnational_err_F_K6_ARIMA_mean = apply(int_fore_subnational_err_F_K6_ARIMA, c(1, 2), mean)
int_fore_subnational_err_M_K6_ARIMA_mean = apply(int_fore_subnational_err_M_K6_ARIMA, c(1, 2), mean)

colnames(int_fore_subnational_err_F_K6_ARIMA_mean) = colnames(int_fore_subnational_err_M_K6_ARIMA_mean) = c("ECP", "CPD", "score")
rownames(int_fore_subnational_err_F_K6_ARIMA_mean) = rownames(int_fore_subnational_err_M_K6_ARIMA_mean) = 1:20

int_fore_subnational_err_F_K6_ARIMA_mean_prefecture = t(apply(int_fore_subnational_err_F_K6_ARIMA, c(2, 3), mean))
int_fore_subnational_err_M_K6_ARIMA_mean_prefecture = t(apply(int_fore_subnational_err_M_K6_ARIMA, c(2, 3), mean))


## level_sig = 0.95

# EVR

int_fore_subnational_err_F_EVR_ARIMA_alpha_0.95 = int_fore_subnational_err_M_EVR_ARIMA_alpha_0.95 = array(NA, dim = c(20, 3, n_state), dimnames = list(1:20, c("ECP", "CPD", "MIS"), state))
int_fore_subnational_err_F_EVR_ARIMA_tune_para_alpha_0.95 = int_fore_subnational_err_F_EVR_ARIMA_tune_para_obj_alpha_0.95 = 
int_fore_subnational_err_M_EVR_ARIMA_tune_para_alpha_0.95 = int_fore_subnational_err_M_EVR_ARIMA_tune_para_obj_alpha_0.95 = matrix(NA, 20, n_state)
for(ij in 1:n_state)
{
    for(iw in 1:20)
    {
        ## Female
        
        dum = interval_fore_subnational_cdf(fdata = female_prefecture_dx[[ij]], method_ncomp = "EVR",
                                            horizon = iw, fore_method = "CDF", uni_fore_method = "arima",
                                            level_sig = 0.95)
        int_fore_subnational_err_F_EVR_ARIMA_alpha_0.95[iw,,ij] = dum$int_err
        int_fore_subnational_err_F_EVR_ARIMA_tune_para_alpha_0.95[iw,ij] = dum$tune_para_find
        int_fore_subnational_err_F_EVR_ARIMA_tune_para_obj_alpha_0.95[iw,ij] = dum$tune_para_find_obj
        rm(dum)
        
        ## Male
        
        dum = interval_fore_subnational_cdf(fdata = male_prefecture_dx[[ij]], method_ncomp = "EVR",
                                            horizon = iw, fore_method = "CDF", uni_fore_method = "arima",
                                            level_sig = 0.95)
        int_fore_subnational_err_M_EVR_ARIMA_alpha_0.95[iw,,ij] = dum$int_err
        int_fore_subnational_err_M_EVR_ARIMA_tune_para_alpha_0.95[iw,ij] = dum$tune_para_find
        int_fore_subnational_err_M_EVR_ARIMA_tune_para_obj_alpha_0.95[iw,ij] = dum$tune_para_find_obj
        rm(dum); print(iw); rm(iw)
    }
    print(ij); rm(ij)
}

int_fore_subnational_err_F_EVR_ARIMA_alpha_0.95_mean = apply(int_fore_subnational_err_F_EVR_ARIMA_alpha_0.95, c(1, 2), mean)
int_fore_subnational_err_M_EVR_ARIMA_alpha_0.95_mean = apply(int_fore_subnational_err_M_EVR_ARIMA_alpha_0.95, c(1, 2), mean)

colnames(int_fore_subnational_err_F_EVR_ARIMA_alpha_0.95_mean) = colnames(int_fore_subnational_err_M_EVR_ARIMA_alpha_0.95_mean) = c("ECP", "CPD", "score")
rownames(int_fore_subnational_err_F_EVR_ARIMA_alpha_0.95_mean) = rownames(int_fore_subnational_err_M_EVR_ARIMA_alpha_0.95_mean) = 1:20

int_fore_subnational_err_F_EVR_ARIMA_alpha_0.95_mean_prefecture = t(apply(int_fore_subnational_err_F_EVR_ARIMA_alpha_0.95, c(2, 3), mean))
int_fore_subnational_err_M_EVR_ARIMA_alpha_0.95_mean_prefecture = t(apply(int_fore_subnational_err_M_EVR_ARIMA_alpha_0.95, c(2, 3), mean))

# K = 6

int_fore_subnational_err_F_K6_ARIMA_alpha_0.95 = int_fore_subnational_err_M_K6_ARIMA_alpha_0.95 = array(NA, dim = c(20, 3, n_state), dimnames = list(1:20, c("ECP", "CPD", "MIS"), state))
int_fore_subnational_err_F_K6_ARIMA_tune_para_alpha_0.95 = int_fore_subnational_err_F_K6_ARIMA_tune_para_obj_alpha_0.95 = 
int_fore_subnational_err_M_K6_ARIMA_tune_para_alpha_0.95 = int_fore_subnational_err_M_K6_ARIMA_tune_para_obj_alpha_0.95 = matrix(NA, 20, n_state)
for(ij in 1:n_state)
{
    for(iw in 1:20)
    {
        ## Female
        
        dum = interval_fore_subnational_cdf(fdata = female_prefecture_dx[[ij]], method_ncomp = "provide",
                                            horizon = iw, fore_method = "CDF", uni_fore_method = "arima",
                                            level_sig = 0.95)
        int_fore_subnational_err_F_K6_ARIMA_alpha_0.95[iw,,ij] = dum$int_err
        int_fore_subnational_err_F_K6_ARIMA_tune_para_alpha_0.95[iw,ij] = dum$tune_para_find
        int_fore_subnational_err_F_K6_ARIMA_tune_para_obj_alpha_0.95[iw,ij] = dum$tune_para_find_obj
        rm(dum)
        
        ## Male
        
        dum = interval_fore_subnational_cdf(fdata = male_prefecture_dx[[ij]], method_ncomp = "provide",
                                            horizon = iw, fore_method = "CDF", uni_fore_method = "arima",
                                            level_sig = 0.95)
        int_fore_subnational_err_M_K6_ARIMA_alpha_0.95[iw,,ij] = dum$int_err
        int_fore_subnational_err_M_K6_ARIMA_tune_para_alpha_0.95[iw,ij] = dum$tune_para_find
        int_fore_subnational_err_M_K6_ARIMA_tune_para_obj_alpha_0.95[iw,ij] = dum$tune_para_find_obj
        rm(dum); print(iw); rm(iw)
    }
    print(ij); rm(ij)
}

int_fore_subnational_err_F_K6_ARIMA_alpha_0.95_mean = apply(int_fore_subnational_err_F_K6_ARIMA_alpha_0.95, c(1, 2), mean)
int_fore_subnational_err_M_K6_ARIMA_alpha_0.95_mean = apply(int_fore_subnational_err_M_K6_ARIMA_alpha_0.95, c(1, 2), mean)

colnames(int_fore_subnational_err_F_K6_ARIMA_alpha_0.95_mean) = colnames(int_fore_subnational_err_M_K6_ARIMA_alpha_0.95_mean) = c("ECP", "CPD", "score")
rownames(int_fore_subnational_err_F_K6_ARIMA_alpha_0.95_mean) = rownames(int_fore_subnational_err_M_K6_ARIMA_alpha_0.95_mean) = 1:20

int_fore_subnational_err_F_K6_ARIMA_alpha_0.95_mean_prefecture = t(apply(int_fore_subnational_err_F_K6_ARIMA_alpha_0.95, c(2, 3), mean))
int_fore_subnational_err_M_K6_ARIMA_alpha_0.95_mean_prefecture = t(apply(int_fore_subnational_err_M_K6_ARIMA_alpha_0.95, c(2, 3), mean))

