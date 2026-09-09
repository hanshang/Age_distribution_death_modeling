setwd("~/Library/CloudStorage/Dropbox/Todos/FANOVA_FFM_CDF/code")
source("load_packages.R")
source("auxiliary_point.R")
source("auxiliary_interval.R")

## level of significance = 0.8

# EVR

MLFTS_int_fore_subnational_err_F_EVR_ETS_CLR = MLFTS_int_fore_subnational_err_M_EVR_ETS_CLR = array(NA, dim = c(n_state, 20, 3), dimnames = list(state, 1:20, c("ECP", "CPD", "score")))
MLFTS_int_fore_subnational_err_F_EVR_ETS_tune_para_CLR = MLFTS_int_fore_subnational_err_F_EVR_ETS_tune_para_obj_CLR =
MLFTS_int_fore_subnational_err_M_EVR_ETS_tune_para_CLR = MLFTS_int_fore_subnational_err_M_EVR_ETS_tune_para_obj_CLR = matrix(NA, n_state, 20)
for(ij in 1:n_state)
{
    female_data = replace(female_prefecture_dx[[ij]], which(female_prefecture_dx[[ij]] == 0), 10^-5)
    male_data   = replace(male_prefecture_dx[[ij]],   which(male_prefecture_dx[[ij]] == 0),   10^-5)
    for(iw in 1:20)
    {
        dum = interval_fore_subnational_cdf_MLFTS(fdata_F = female_data,
                                                  fdata_M = male_data,
                                                  fdata_common = NULL,
                                                  fore_method = "CLR",
                                                  horizon = iw, way_ncomp = "EVR",
                                                  level_sig = 0.8, uni_fore_method = "ets")
        MLFTS_int_fore_subnational_err_F_EVR_ETS_CLR[ij,iw,] = dum$int_F_err
        MLFTS_int_fore_subnational_err_F_EVR_ETS_tune_para_CLR[ij,iw] = dum$tune_para_find_F
        MLFTS_int_fore_subnational_err_F_EVR_ETS_tune_para_obj_CLR[ij,iw] = dum$tune_para_find_F_obj
        
        MLFTS_int_fore_subnational_err_M_EVR_ETS_CLR[ij,iw,] = dum$int_M_err
        MLFTS_int_fore_subnational_err_M_EVR_ETS_tune_para_CLR[ij,iw] = dum$tune_para_find_M
        MLFTS_int_fore_subnational_err_M_EVR_ETS_tune_para_obj_CLR[ij,iw] = dum$tune_para_find_M_obj
        rm(dum); rm(iw)
    }
    print(ij); rm(ij); rm(female_data); rm(male_data)
}

horizon_specific_MLFTS_int_fore_subnational_err_F_EVR_ETS_CLR = apply(MLFTS_int_fore_subnational_err_F_EVR_ETS_CLR, c(2, 3), mean)
horizon_specific_MLFTS_int_fore_subnational_err_M_EVR_ETS_CLR = apply(MLFTS_int_fore_subnational_err_M_EVR_ETS_CLR, c(2, 3), mean)

horizon_specific_MLFTS_int_fore_subnational_err_F_EVR_ETS_CLR_prefecture = apply(MLFTS_int_fore_subnational_err_F_EVR_ETS_CLR, c(1, 3), mean)
horizon_specific_MLFTS_int_fore_subnational_err_M_EVR_ETS_CLR_prefecture = apply(MLFTS_int_fore_subnational_err_M_EVR_ETS_CLR, c(1, 3), mean)

# K = 6

MLFTS_int_fore_subnational_err_F_K6_ETS_CLR = MLFTS_int_fore_subnational_err_M_K6_ETS_CLR = array(NA, dim = c(n_state, 20, 3), dimnames = list(state, 1:20, c("ECP", "CPD", "score")))
MLFTS_int_fore_subnational_err_F_K6_ETS_tune_para_CLR = MLFTS_int_fore_subnational_err_F_K6_ETS_tune_para_obj_CLR =
MLFTS_int_fore_subnational_err_M_K6_ETS_tune_para_CLR = MLFTS_int_fore_subnational_err_M_K6_ETS_tune_para_obj_CLR = matrix(NA, n_state, 20)
for(ij in 1:47)
{
    female_data = replace(female_prefecture_dx[[ij]], which(female_prefecture_dx[[ij]] == 0), 10^-5)
    male_data   = replace(male_prefecture_dx[[ij]],   which(male_prefecture_dx[[ij]] == 0),   10^-5)
    for(iw in 1:20)
    {
        dum = interval_fore_subnational_cdf_MLFTS(fdata_F = female_data,
                                               fdata_M = male_data,
                                               fdata_common = NULL,
                                               fore_method = "CLR",
                                               horizon = iw, way_ncomp = "provide",
                                               level_sig = 0.8, uni_fore_method = "ets")
        MLFTS_int_fore_subnational_err_F_K6_ETS_CLR[ij,iw,] = dum$int_F_err
        MLFTS_int_fore_subnational_err_F_K6_ETS_tune_para_CLR[ij,iw] = dum$tune_para_find_F
        MLFTS_int_fore_subnational_err_F_K6_ETS_tune_para_obj_CLR[ij,iw] = dum$tune_para_find_F_obj
        
        MLFTS_int_fore_subnational_err_M_K6_ETS_CLR[ij,iw,] = dum$int_M_err
        MLFTS_int_fore_subnational_err_M_K6_ETS_tune_para_CLR[ij,iw] = dum$tune_para_find_M
        MLFTS_int_fore_subnational_err_M_K6_ETS_tune_para_obj_CLR[ij,iw] = dum$tune_para_find_M_obj
        rm(dum); rm(iw)
    }
    print(ij); rm(ij); rm(female_data); rm(male_data)
}

horizon_specific_MLFTS_int_fore_subnational_err_F_K6_ETS_CLR = apply(MLFTS_int_fore_subnational_err_F_K6_ETS_CLR, c(2, 3), mean)
horizon_specific_MLFTS_int_fore_subnational_err_M_K6_ETS_CLR = apply(MLFTS_int_fore_subnational_err_M_K6_ETS_CLR, c(2, 3), mean)

horizon_specific_MLFTS_int_fore_subnational_err_F_K6_ETS_CLR_prefecture = apply(MLFTS_int_fore_subnational_err_F_K6_ETS_CLR, c(1, 3), mean)
horizon_specific_MLFTS_int_fore_subnational_err_M_K6_ETS_CLR_prefecture = apply(MLFTS_int_fore_subnational_err_M_K6_ETS_CLR, c(1, 3), mean)

## level of significance = 0.95

# EVR

MLFTS_int_fore_subnational_err_F_EVR_ETS_CLR_95 = MLFTS_int_fore_subnational_err_M_EVR_ETS_CLR_95 = array(NA, dim = c(n_state, 20, 3), dimnames = list(state, 1:20, c("ECP", "CPD", "score")))
MLFTS_int_fore_subnational_err_F_EVR_ETS_tune_para_CLR_95 = MLFTS_int_fore_subnational_err_F_EVR_ETS_tune_para_obj_CLR_95 =
MLFTS_int_fore_subnational_err_M_EVR_ETS_tune_para_CLR_95 = MLFTS_int_fore_subnational_err_M_EVR_ETS_tune_para_obj_CLR_95 = matrix(NA, n_state, 20)
for(ij in 1:n_state)
{
    female_data = replace(female_prefecture_dx[[ij]], which(female_prefecture_dx[[ij]] == 0), 10^-5)
    male_data   = replace(male_prefecture_dx[[ij]],   which(male_prefecture_dx[[ij]] == 0),   10^-5)
    for(iw in 1:20)
    {
        dum = interval_fore_subnational_cdf_MLFTS(fdata_F = female_data,
                                                  fdata_M = male_data,
                                                  fdata_common = NULL,
                                                  fore_method = "CLR",
                                                  horizon = iw, way_ncomp = "EVR",
                                                  level_sig = 0.95, uni_fore_method = "ets")
        MLFTS_int_fore_subnational_err_F_EVR_ETS_CLR_95[ij,iw,] = dum$int_F_err
        MLFTS_int_fore_subnational_err_F_EVR_ETS_tune_para_CLR_95[ij,iw] = dum$tune_para_find_F
        MLFTS_int_fore_subnational_err_F_EVR_ETS_tune_para_obj_CLR_95[ij,iw] = dum$tune_para_find_F_obj
        
        MLFTS_int_fore_subnational_err_M_EVR_ETS_CLR_95[ij,iw,] = dum$int_M_err
        MLFTS_int_fore_subnational_err_M_EVR_ETS_tune_para_CLR_95[ij,iw] = dum$tune_para_find_M
        MLFTS_int_fore_subnational_err_M_EVR_ETS_tune_para_obj_CLR_95[ij,iw] = dum$tune_para_find_M_obj
        rm(dum); rm(iw)
    }
    print(ij); rm(ij); rm(female_data); rm(male_data)
}

horizon_specific_MLFTS_int_fore_subnational_err_F_EVR_ETS_CLR_95 = apply(MLFTS_int_fore_subnational_err_F_EVR_ETS_CLR_95, c(2, 3), mean)
horizon_specific_MLFTS_int_fore_subnational_err_M_EVR_ETS_CLR_95 = apply(MLFTS_int_fore_subnational_err_M_EVR_ETS_CLR_95, c(2, 3), mean)

horizon_specific_MLFTS_int_fore_subnational_err_F_EVR_ETS_CLR_prefecture_95 = apply(MLFTS_int_fore_subnational_err_F_EVR_ETS_CLR_95, c(1, 3), mean)
horizon_specific_MLFTS_int_fore_subnational_err_M_EVR_ETS_CLR_prefecture_95 = apply(MLFTS_int_fore_subnational_err_M_EVR_ETS_CLR_95, c(1, 3), mean)

# K = 6

MLFTS_int_fore_subnational_err_F_K6_ETS_CLR_95 = MLFTS_int_fore_subnational_err_M_K6_ETS_CLR_95 = array(NA, dim = c(n_state, 20, 3), dimnames = list(state, 1:20, c("ECP", "CPD", "score")))
MLFTS_int_fore_subnational_err_F_K6_ETS_tune_para_CLR_95 = MLFTS_int_fore_subnational_err_F_K6_ETS_tune_para_obj_CLR_95 =
MLFTS_int_fore_subnational_err_M_K6_ETS_tune_para_CLR_95 = MLFTS_int_fore_subnational_err_M_K6_ETS_tune_para_obj_CLR_95 = matrix(NA, n_state, 20)
for(ij in 1:n_state)
{
    female_data = replace(female_prefecture_dx[[ij]], which(female_prefecture_dx[[ij]] == 0), 10^-5)
    male_data   = replace(male_prefecture_dx[[ij]],   which(male_prefecture_dx[[ij]] == 0),   10^-5)
    for(iw in 1:20)
    {
        dum = interval_fore_subnational_cdf_MLFTS(fdata_F = female_data,
                                                  fdata_M = male_data,
                                                  fdata_common = NULL,
                                                  fore_method = "CLR",
                                                  horizon = iw, way_ncomp = "provide",
                                                  level_sig = 0.95, uni_fore_method = "ets")
        MLFTS_int_fore_subnational_err_F_K6_ETS_CLR_95[ij,iw,] = dum$int_F_err
        MLFTS_int_fore_subnational_err_F_K6_ETS_tune_para_CLR_95[ij,iw] = dum$tune_para_find_F
        MLFTS_int_fore_subnational_err_F_K6_ETS_tune_para_obj_CLR_95[ij,iw] = dum$tune_para_find_F_obj
        
        MLFTS_int_fore_subnational_err_M_K6_ETS_CLR_95[ij,iw,] = dum$int_M_err
        MLFTS_int_fore_subnational_err_M_K6_ETS_tune_para_CLR_95[ij,iw] = dum$tune_para_find_M
        MLFTS_int_fore_subnational_err_M_K6_ETS_tune_para_obj_CLR_95[ij,iw] = dum$tune_para_find_M_obj
        rm(dum); rm(iw)
    }
    print(ij); rm(ij); rm(female_data); rm(male_data)
}

horizon_specific_MLFTS_int_fore_subnational_err_F_K6_ETS_CLR_95 = apply(MLFTS_int_fore_subnational_err_F_K6_ETS_CLR_95, c(2, 3), mean)
horizon_specific_MLFTS_int_fore_subnational_err_M_K6_ETS_CLR_95 = apply(MLFTS_int_fore_subnational_err_M_K6_ETS_CLR_95, c(2, 3), mean)

horizon_specific_MLFTS_int_fore_subnational_err_F_K6_ETS_CLR_95_prefecture = apply(MLFTS_int_fore_subnational_err_F_K6_ETS_CLR_95, c(1, 3), mean)
horizon_specific_MLFTS_int_fore_subnational_err_M_K6_ETS_CLR_95_prefecture = apply(MLFTS_int_fore_subnational_err_M_K6_ETS_CLR_95, c(1, 3), mean)

