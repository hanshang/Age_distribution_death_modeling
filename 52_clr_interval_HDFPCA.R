##################
# load R packages
##################

setwd("~/Library/CloudStorage/Dropbox/Todos/FANOVA_FFM_CDF/code")
load("read_data.RData")
source("load_packages.R")
source("auxiliary_interval.R")
source("hdfpca_fun.R")

###############################
## level of significance = 0.8
###############################

# uni_fore_method = "ets"

hdfpca_int_fore_subnational_err_F_EVR_ETS_CLR = hdfpca_int_fore_subnational_err_M_EVR_ETS_CLR = array(NA, dim = c(n_state, 20, 3), dimnames = list(state, 1:20, c("ECP", "CPD", "score")))
hdfpca_int_fore_subnational_err_F_EVR_ETS_tune_para_CLR = hdfpca_int_fore_subnational_err_F_EVR_ETS_tune_para_obj_CLR = 
hdfpca_int_fore_subnational_err_M_EVR_ETS_tune_para_CLR = hdfpca_int_fore_subnational_err_M_EVR_ETS_tune_para_obj_CLR = matrix(NA, n_state, 20)
for(ij in 1:n_state)
{
    for(iw in 1:20)
    {
        dum = int_hdfpca_fun(fdata_F = female_prefecture_dx[[ij]], fdata_M = male_prefecture_dx[[ij]], 
                             horizon = iw, first_order = 6, second_order = 2, transformation = "CLR", 
                             level_sig = 0.8, uni_fore_method = "ets")
        hdfpca_int_fore_subnational_err_F_EVR_ETS_CLR[ij,iw,] = dum$int_F_err
        hdfpca_int_fore_subnational_err_F_EVR_ETS_tune_para_CLR[ij,iw] = dum$tune_para_find_F
        hdfpca_int_fore_subnational_err_F_EVR_ETS_tune_para_obj_CLR[ij,iw] = dum$tune_para_find_F_obj
        
        hdfpca_int_fore_subnational_err_M_EVR_ETS_CLR[ij,iw,] = dum$int_M_err
        hdfpca_int_fore_subnational_err_M_EVR_ETS_tune_para_CLR[ij,iw] = dum$tune_para_find_M
        hdfpca_int_fore_subnational_err_M_EVR_ETS_tune_para_obj_CLR[ij,iw] = dum$tune_para_find_M_obj
        rm(dum); rm(iw)
    }
    print(ij); rm(ij)
}

horizon_specific_hdfpca_int_fore_subnational_err_F_EVR_ETS_CLR = apply(hdfpca_int_fore_subnational_err_F_EVR_ETS_CLR, c(2, 3), mean)
horizon_specific_hdfpca_int_fore_subnational_err_M_EVR_ETS_CLR = apply(hdfpca_int_fore_subnational_err_M_EVR_ETS_CLR, c(2, 3), mean)

horizon_specific_hdfpca_int_fore_subnational_err_F_EVR_ETS_CLR_prefecture = apply(hdfpca_int_fore_subnational_err_F_EVR_ETS_CLR, c(1, 3), mean)
horizon_specific_hdfpca_int_fore_subnational_err_M_EVR_ETS_CLR_prefecture = apply(hdfpca_int_fore_subnational_err_M_EVR_ETS_CLR, c(1, 3), mean)

# uni_fore_method = "arima"

hdfpca_int_fore_subnational_err_F_EVR_ARIMA_CLR = hdfpca_int_fore_subnational_err_M_EVR_ARIMA_CLR = array(NA, dim = c(n_state, 20, 3), dimnames = list(state, 1:20, c("ECP", "CPD", "score")))
hdfpca_int_fore_subnational_err_F_EVR_ARIMA_tune_para_CLR = hdfpca_int_fore_subnational_err_F_EVR_ARIMA_tune_para_obj_CLR = 
hdfpca_int_fore_subnational_err_M_EVR_ARIMA_tune_para_CLR = hdfpca_int_fore_subnational_err_M_EVR_ARIMA_tune_para_obj_CLR = matrix(NA, n_state, 20)
for(ij in 1:n_state)
{
    for(iw in 1:20)
    {
        dum = int_hdfpca_fun(fdata_F = female_prefecture_dx[[ij]], fdata_M = male_prefecture_dx[[ij]], 
                             horizon = iw, first_order = 6, second_order = 2, transformation = "CLR", 
                             level_sig = 0.8, uni_fore_method = "arima")
        hdfpca_int_fore_subnational_err_F_EVR_ARIMA_CLR[ij,iw,] = dum$int_F_err
        hdfpca_int_fore_subnational_err_F_EVR_ARIMA_tune_para_CLR[ij,iw] = dum$tune_para_find_F
        hdfpca_int_fore_subnational_err_F_EVR_ARIMA_tune_para_obj_CLR[ij,iw] = dum$tune_para_find_F_obj
        
        hdfpca_int_fore_subnational_err_M_EVR_ARIMA_CLR[ij,iw,] = dum$int_M_err
        hdfpca_int_fore_subnational_err_M_EVR_ARIMA_tune_para_CLR[ij,iw] = dum$tune_para_find_M
        hdfpca_int_fore_subnational_err_M_EVR_ARIMA_tune_para_obj_CLR[ij,iw] = dum$tune_para_find_M_obj
        rm(dum); rm(iw)
    }
    print(ij); rm(ij)
}

horizon_specific_hdfpca_int_fore_subnational_err_F_EVR_ARIMA_CLR = apply(hdfpca_int_fore_subnational_err_F_EVR_ARIMA_CLR, c(2, 3), mean)
horizon_specific_hdfpca_int_fore_subnational_err_M_EVR_ARIMA_CLR = apply(hdfpca_int_fore_subnational_err_M_EVR_ARIMA_CLR, c(2, 3), mean)

horizon_specific_hdfpca_int_fore_subnational_err_F_EVR_ARIMA_CLR_prefecture = apply(hdfpca_int_fore_subnational_err_F_EVR_ARIMA_CLR, c(1, 3), mean)
horizon_specific_hdfpca_int_fore_subnational_err_M_EVR_ARIMA_CLR_prefecture = apply(hdfpca_int_fore_subnational_err_M_EVR_ARIMA_CLR, c(1, 3), mean)


################################
## level of significance = 0.95
################################

# uni_fore_method = "ets"

hdfpca_int_fore_subnational_err_F_EVR_ETS_alpha_0.95_CLR = hdfpca_int_fore_subnational_err_M_EVR_ETS_alpha_0.95_CLR = array(NA, dim = c(n_state, 20, 3), dimnames = list(state, 1:20, c("ECP", "CPD", "score")))
hdfpca_int_fore_subnational_err_F_EVR_ETS_tune_para_alpha_0.95_CLR = hdfpca_int_fore_subnational_err_F_EVR_ETS_tune_para_obj_alpha_0.95_CLR = 
hdfpca_int_fore_subnational_err_M_EVR_ETS_tune_para_alpha_0.95_CLR = hdfpca_int_fore_subnational_err_M_EVR_ETS_tune_para_obj_alpha_0.95_CLR = matrix(NA, n_state, 20)
for(ij in 1:n_state)
{
    for(iw in 1:20)
    {
        dum = int_hdfpca_fun(fdata_F = female_prefecture_dx[[ij]], fdata_M = male_prefecture_dx[[ij]], 
                             horizon = iw, first_order = 6, second_order = 2, transformation = "CLR", 
                             level_sig = 0.95, uni_fore_method = "ets")
        hdfpca_int_fore_subnational_err_F_EVR_ETS_alpha_0.95_CLR[ij,iw,] = dum$int_F_err
        hdfpca_int_fore_subnational_err_F_EVR_ETS_tune_para_alpha_0.95_CLR[ij,iw] = dum$tune_para_find_F
        hdfpca_int_fore_subnational_err_F_EVR_ETS_tune_para_obj_alpha_0.95_CLR[ij,iw] = dum$tune_para_find_F_obj
        
        hdfpca_int_fore_subnational_err_M_EVR_ETS_alpha_0.95_CLR[ij,iw,] = dum$int_M_err
        hdfpca_int_fore_subnational_err_M_EVR_ETS_tune_para_alpha_0.95_CLR[ij,iw] = dum$tune_para_find_M
        hdfpca_int_fore_subnational_err_M_EVR_ETS_tune_para_obj_alpha_0.95_CLR[ij,iw] = dum$tune_para_find_M_obj
        rm(dum); rm(iw)
    }
    print(ij); rm(ij)
}

horizon_specific_hdfpca_int_fore_subnational_err_F_EVR_ETS_alpha_0.95_CLR = apply(hdfpca_int_fore_subnational_err_F_EVR_ETS_alpha_0.95_CLR, c(2, 3), mean)
horizon_specific_hdfpca_int_fore_subnational_err_M_EVR_ETS_alpha_0.95_CLR = apply(hdfpca_int_fore_subnational_err_M_EVR_ETS_alpha_0.95_CLR, c(2, 3), mean)

horizon_specific_hdfpca_int_fore_subnational_err_F_EVR_ETS_alpha_0.95_CLR_prefecture = apply(hdfpca_int_fore_subnational_err_F_EVR_ETS_alpha_0.95_CLR, c(1, 3), mean)
horizon_specific_hdfpca_int_fore_subnational_err_M_EVR_ETS_alpha_0.95_CLR_prefecture = apply(hdfpca_int_fore_subnational_err_M_EVR_ETS_alpha_0.95_CLR, c(1, 3), mean)

# uni_fore_method = "arima"

hdfpca_int_fore_subnational_err_F_EVR_ARIMA_alpha_0.95_CLR = hdfpca_int_fore_subnational_err_M_EVR_ARIMA_alpha_0.95_CLR = array(NA, dim = c(n_state, 20, 3), dimnames = list(state, 1:20, c("ECP", "CPD", "score")))
hdfpca_int_fore_subnational_err_F_EVR_ARIMA_tune_para_alpha_0.95_CLR = hdfpca_int_fore_subnational_err_F_EVR_ARIMA_tune_para_obj_alpha_0.95_CLR = 
hdfpca_int_fore_subnational_err_M_EVR_ARIMA_tune_para_alpha_0.95_CLR = hdfpca_int_fore_subnational_err_M_EVR_ARIMA_tune_para_obj_alpha_0.95_CLR = matrix(NA, n_state, 20)
for(ij in 1:n_state)
{
    for(iw in 1:20)
    {
        dum = int_hdfpca_fun(fdata_F = female_prefecture_dx[[ij]], fdata_M = male_prefecture_dx[[ij]], 
                             horizon = iw, first_order = 6, second_order = 2, transformation = "CLR", 
                             level_sig = 0.95, uni_fore_method = "arima")
        hdfpca_int_fore_subnational_err_F_EVR_ARIMA_alpha_0.95_CLR[ij,iw,] = dum$int_F_err
        hdfpca_int_fore_subnational_err_F_EVR_ARIMA_tune_para_alpha_0.95_CLR[ij,iw] = dum$tune_para_find_F
        hdfpca_int_fore_subnational_err_F_EVR_ARIMA_tune_para_obj_alpha_0.95_CLR[ij,iw] = dum$tune_para_find_F_obj
        
        hdfpca_int_fore_subnational_err_M_EVR_ARIMA_alpha_0.95_CLR[ij,iw,] = dum$int_M_err
        hdfpca_int_fore_subnational_err_M_EVR_ARIMA_tune_para_alpha_0.95_CLR[ij,iw] = dum$tune_para_find_M
        hdfpca_int_fore_subnational_err_M_EVR_ARIMA_tune_para_obj_alpha_0.95_CLR[ij,iw] = dum$tune_para_find_M_obj
        rm(dum); rm(iw)
    }
    print(ij); rm(ij)
}

horizon_specific_hdfpca_int_fore_subnational_err_F_EVR_ARIMA_alpha_0.95_CLR = apply(hdfpca_int_fore_subnational_err_F_EVR_ARIMA_alpha_0.95_CLR, c(2, 3), mean)
horizon_specific_hdfpca_int_fore_subnational_err_M_EVR_ARIMA_alpha_0.95_CLR = apply(hdfpca_int_fore_subnational_err_M_EVR_ARIMA_alpha_0.95_CLR, c(2, 3), mean)

horizon_specific_hdfpca_int_fore_subnational_err_F_EVR_ARIMA_alpha_0.95_CLR_prefecture = apply(hdfpca_int_fore_subnational_err_F_EVR_ARIMA_alpha_0.95_CLR, c(1, 3), mean)
horizon_specific_hdfpca_int_fore_subnational_err_M_EVR_ARIMA_alpha_0.95_CLR_prefecture = apply(hdfpca_int_fore_subnational_err_M_EVR_ARIMA_alpha_0.95_CLR, c(1, 3), mean)

