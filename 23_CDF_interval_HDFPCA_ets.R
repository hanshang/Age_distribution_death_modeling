# load R packages

source("load_packages.R")
source("auxiliary_interval.R")
source("hdfpca_fun.R")

###############################
## level of significance = 0.8
###############################

hdfpca_int_fore_subnational_err_F_EVR_ETS = hdfpca_int_fore_subnational_err_M_EVR_ETS = array(NA, dim = c(n_state, 20, 3), dimnames = list(state, 1:20, c("ECP", "CPD", "score")))
hdfpca_int_fore_subnational_err_F_EVR_ETS_tune_para = hdfpca_int_fore_subnational_err_F_EVR_ETS_tune_para_obj = 
hdfpca_int_fore_subnational_err_M_EVR_ETS_tune_para = hdfpca_int_fore_subnational_err_M_EVR_ETS_tune_para_obj = matrix(NA, n_state, 20)
for(ij in 1:n_state)
{
    for(iw in 1:20)
    {
        dum = int_hdfpca_fun(fdata_F = female_prefecture_dx[[ij]], fdata_M = male_prefecture_dx[[ij]], 
                             horizon = iw, first_order = 6, second_order = 2, transformation = "CDF", 
                             level_sig = 0.8, uni_fore_method = "ets")
        hdfpca_int_fore_subnational_err_F_EVR_ETS[ij,iw,] = dum$int_F_err
        hdfpca_int_fore_subnational_err_F_EVR_ETS_tune_para[ij,iw] = dum$tune_para_find_F
        hdfpca_int_fore_subnational_err_F_EVR_ETS_tune_para_obj[ij,iw] = dum$tune_para_find_F_obj
        
        hdfpca_int_fore_subnational_err_M_EVR_ETS[ij,iw,] = dum$int_M_err
        hdfpca_int_fore_subnational_err_M_EVR_ETS_tune_para[ij,iw] = dum$tune_para_find_M
        hdfpca_int_fore_subnational_err_M_EVR_ETS_tune_para_obj[ij,iw] = dum$tune_para_find_M_obj
        rm(dum); rm(iw)
    }
    print(ij); rm(ij)
}

horizon_specific_hdfpca_int_fore_subnational_err_F_EVR_ETS = apply(hdfpca_int_fore_subnational_err_F_EVR_ETS, c(2, 3), mean)
horizon_specific_hdfpca_int_fore_subnational_err_M_EVR_ETS = apply(hdfpca_int_fore_subnational_err_M_EVR_ETS, c(2, 3), mean)

horizon_specific_hdfpca_int_fore_subnational_err_F_EVR_ETS_prefecture = apply(hdfpca_int_fore_subnational_err_F_EVR_ETS, c(1, 3), mean)
horizon_specific_hdfpca_int_fore_subnational_err_M_EVR_ETS_prefecture = apply(hdfpca_int_fore_subnational_err_M_EVR_ETS, c(1, 3), mean)

################################
## level of significance = 0.95
################################

hdfpca_int_fore_subnational_err_F_EVR_ETS_alpha_0.95 = hdfpca_int_fore_subnational_err_M_EVR_ETS_alpha_0.95 = array(NA, dim = c(n_state, 20, 3), dimnames = list(state, 1:20, c("ECP", "CPD", "score")))
hdfpca_int_fore_subnational_err_F_EVR_ETS_tune_para_alpha_0.95 = hdfpca_int_fore_subnational_err_F_EVR_ETS_tune_para_obj_alpha_0.95 = 
hdfpca_int_fore_subnational_err_M_EVR_ETS_tune_para_alpha_0.95 = hdfpca_int_fore_subnational_err_M_EVR_ETS_tune_para_obj_alpha_0.95 = matrix(NA, n_state, 20)
for(ij in 1:n_state)
{
    for(iw in 1:20)
    {
        dum = int_hdfpca_fun(fdata_F = female_prefecture_dx[[ij]], fdata_M = male_prefecture_dx[[ij]], 
                             horizon = iw, first_order = 6, second_order = 2, transformation = "CDF", 
                             level_sig = 0.95, uni_fore_method = "ets")
        hdfpca_int_fore_subnational_err_F_EVR_ETS_alpha_0.95[ij,iw,] = dum$int_F_err
        hdfpca_int_fore_subnational_err_F_EVR_ETS_tune_para_alpha_0.95[ij,iw] = dum$tune_para_find_F
        hdfpca_int_fore_subnational_err_F_EVR_ETS_tune_para_obj_alpha_0.95[ij,iw] = dum$tune_para_find_F_obj
        
        hdfpca_int_fore_subnational_err_M_EVR_ETS_alpha_0.95[ij,iw,] = dum$int_M_err
        hdfpca_int_fore_subnational_err_M_EVR_ETS_tune_para_alpha_0.95[ij,iw] = dum$tune_para_find_M
        hdfpca_int_fore_subnational_err_M_EVR_ETS_tune_para_obj_alpha_0.95[ij,iw] = dum$tune_para_find_M_obj
        rm(dum); rm(iw)
    }
    print(ij); rm(ij)
}

horizon_specific_hdfpca_int_fore_subnational_err_F_EVR_ETS_alpha_0.95 = apply(hdfpca_int_fore_subnational_err_F_EVR_ETS_alpha_0.95, c(2, 3), mean)
horizon_specific_hdfpca_int_fore_subnational_err_M_EVR_ETS_alpha_0.95 = apply(hdfpca_int_fore_subnational_err_M_EVR_ETS_alpha_0.95, c(2, 3), mean)

horizon_specific_hdfpca_int_fore_subnational_err_F_EVR_ETS_alpha_0.95_prefecture = apply(hdfpca_int_fore_subnational_err_F_EVR_ETS_alpha_0.95, c(1, 3), mean)
horizon_specific_hdfpca_int_fore_subnational_err_M_EVR_ETS_alpha_0.95_prefecture = apply(hdfpca_int_fore_subnational_err_M_EVR_ETS_alpha_0.95, c(1, 3), mean)

