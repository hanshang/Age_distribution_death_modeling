###############################
## level of significance = 0.8
###############################

# EVR

MFTS_int_fore_subnational_err_F_EVR_ARIMA_CLR = MFTS_int_fore_subnational_err_M_EVR_ARIMA_CLR = array(NA, dim = c(n_state, 20, 3), dimnames = list(state, 1:20, c("ECP", "CPD", "score")))
MFTS_int_fore_subnational_err_F_EVR_ARIMA_tune_para_CLR = MFTS_int_fore_subnational_err_F_EVR_ARIMA_tune_para_obj_CLR = 
MFTS_int_fore_subnational_err_M_EVR_ARIMA_tune_para_CLR = MFTS_int_fore_subnational_err_M_EVR_ARIMA_tune_para_obj_CLR = matrix(NA, n_state, 20)
for(ij in 1:n_state)
{
    female_data = replace(female_prefecture_dx[[ij]], which(female_prefecture_dx[[ij]] == 0), 10^-5)
    male_data   = replace(male_prefecture_dx[[ij]],   which(male_prefecture_dx[[ij]] == 0),   10^-5)
    for(iw in 1:20)
    {
        dum = interval_fore_subnational_cdf_MFTS(fdata_F = female_data, 
                                                 fdata_M = male_data, 
                                                 fore_method = "CLR", 
                                                 horizon = iw, way_ncomp = "EVR", uni_fore_method = "arima",
                                                 level_sig = 0.8)
        MFTS_int_fore_subnational_err_F_EVR_ARIMA_CLR[ij,iw,] = dum$int_F_err
        MFTS_int_fore_subnational_err_F_EVR_ARIMA_tune_para_CLR[ij,iw] = dum$tune_para_find_F
        MFTS_int_fore_subnational_err_F_EVR_ARIMA_tune_para_obj_CLR[ij,iw] = dum$tune_para_find_F_obj
        
        MFTS_int_fore_subnational_err_M_EVR_ARIMA_CLR[ij,iw,] = dum$int_M_err
        MFTS_int_fore_subnational_err_M_EVR_ARIMA_tune_para_CLR[ij,iw] = dum$tune_para_find_M
        MFTS_int_fore_subnational_err_M_EVR_ARIMA_tune_para_obj_CLR[ij,iw] = dum$tune_para_find_M_obj
        rm(dum); rm(iw)
    }
    print(ij); rm(ij); rm(female_data); rm(male_data)
}

horizon_specific_MFTS_int_fore_subnational_err_F_EVR_ARIMA_CLR = apply(MFTS_int_fore_subnational_err_F_EVR_ARIMA_CLR, c(2, 3), mean)
horizon_specific_MFTS_int_fore_subnational_err_M_EVR_ARIMA_CLR = apply(MFTS_int_fore_subnational_err_M_EVR_ARIMA_CLR, c(2, 3), mean)

horizon_specific_MFTS_int_fore_subnational_err_F_EVR_ARIMA_CLR_prefecture = apply(MFTS_int_fore_subnational_err_F_EVR_ARIMA_CLR, c(1, 3), mean)
horizon_specific_MFTS_int_fore_subnational_err_M_EVR_ARIMA_CLR_prefecture = apply(MFTS_int_fore_subnational_err_M_EVR_ARIMA_CLR, c(1, 3), mean)

# K = 6

MFTS_int_fore_subnational_err_F_K6_ARIMA_CLR = MFTS_int_fore_subnational_err_M_K6_ARIMA_CLR = array(NA, dim = c(n_state, 20, 3), dimnames = list(state, 1:20, c("ECP", "CPD", "score")))
MFTS_int_fore_subnational_err_F_K6_ARIMA_tune_para_CLR = MFTS_int_fore_subnational_err_F_K6_ARIMA_tune_para_obj_CLR = 
MFTS_int_fore_subnational_err_M_K6_ARIMA_tune_para_CLR = MFTS_int_fore_subnational_err_M_K6_ARIMA_tune_para_obj_CLR = matrix(NA, n_state, 20)
for(ij in 1:n_state)
{
    female_data = replace(female_prefecture_dx[[ij]], which(female_prefecture_dx[[ij]] == 0), 10^-5)
    male_data   = replace(male_prefecture_dx[[ij]],   which(male_prefecture_dx[[ij]] == 0),   10^-5)
    for(iw in 1:20)
    {
        dum = interval_fore_subnational_cdf_MFTS(fdata_F = female_data, 
                                                 fdata_M = male_data, 
                                                 fore_method = "CLR", 
                                                 horizon = iw, way_ncomp = "provide", uni_fore_method = "arima",
                                                 level_sig = 0.8)
        MFTS_int_fore_subnational_err_F_K6_ARIMA_CLR[ij,iw,] = dum$int_F_err
        MFTS_int_fore_subnational_err_F_K6_ARIMA_tune_para_CLR[ij,iw] = dum$tune_para_find_F
        MFTS_int_fore_subnational_err_F_K6_ARIMA_tune_para_obj_CLR[ij,iw] = dum$tune_para_find_F_obj
        
        MFTS_int_fore_subnational_err_M_K6_ARIMA_CLR[ij,iw,] = dum$int_M_err
        MFTS_int_fore_subnational_err_M_K6_ARIMA_tune_para_CLR[ij,iw] = dum$tune_para_find_M
        MFTS_int_fore_subnational_err_M_K6_ARIMA_tune_para_obj_CLR[ij,iw] = dum$tune_para_find_M_obj
        rm(dum); rm(iw)
    }
    print(ij); rm(ij); rm(female_data); rm(male_data)
}

horizon_specific_MFTS_int_fore_subnational_err_F_K6_ARIMA_CLR = apply(MFTS_int_fore_subnational_err_F_K6_ARIMA_CLR, c(2, 3), mean)
horizon_specific_MFTS_int_fore_subnational_err_M_K6_ARIMA_CLR = apply(MFTS_int_fore_subnational_err_M_K6_ARIMA_CLR, c(2, 3), mean)

horizon_specific_MFTS_int_fore_subnational_err_F_K6_ARIMA_CLR_prefecture = apply(MFTS_int_fore_subnational_err_F_K6_ARIMA_CLR, c(1, 3), mean)
horizon_specific_MFTS_int_fore_subnational_err_M_K6_ARIMA_CLR_prefecture = apply(MFTS_int_fore_subnational_err_M_K6_ARIMA_CLR, c(1, 3), mean)

## level of significance = 0.95

# EVR

MFTS_int_fore_subnational_err_F_EVR_ARIMA_CLR_95 = MFTS_int_fore_subnational_err_M_EVR_ARIMA_CLR_95 = array(NA, dim = c(n_state, 20, 3), dimnames = list(state, 1:20, c("ECP", "CPD", "score")))
MFTS_int_fore_subnational_err_F_EVR_ARIMA_tune_para_CLR_95 = MFTS_int_fore_subnational_err_F_EVR_ARIMA_tune_para_obj_CLR_95 = 
MFTS_int_fore_subnational_err_M_EVR_ARIMA_tune_para_CLR_95 = MFTS_int_fore_subnational_err_M_EVR_ARIMA_tune_para_obj_CLR_95 = matrix(NA, n_state, 20)
for(ij in 1:n_state)
{
    female_data = replace(female_prefecture_dx[[ij]], which(female_prefecture_dx[[ij]] == 0), 10^-5)
    male_data   = replace(male_prefecture_dx[[ij]],   which(male_prefecture_dx[[ij]] == 0),   10^-5)
    for(iw in 1:20)
    {
        dum = interval_fore_subnational_cdf_MFTS(fdata_F = female_data, 
                                                 fdata_M = male_data, 
                                                 fore_method = "CLR", 
                                                 horizon = iw, way_ncomp = "EVR", uni_fore_method = "arima",
                                                 level_sig = 0.95)
        MFTS_int_fore_subnational_err_F_EVR_ARIMA_CLR_95[ij,iw,] = dum$int_F_err
        MFTS_int_fore_subnational_err_F_EVR_ARIMA_tune_para_CLR_95[ij,iw] = dum$tune_para_find_F
        MFTS_int_fore_subnational_err_F_EVR_ARIMA_tune_para_obj_CLR_95[ij,iw] = dum$tune_para_find_F_obj
        
        MFTS_int_fore_subnational_err_M_EVR_ARIMA_CLR_95[ij,iw,] = dum$int_M_err
        MFTS_int_fore_subnational_err_M_EVR_ARIMA_tune_para_CLR_95[ij,iw] = dum$tune_para_find_M
        MFTS_int_fore_subnational_err_M_EVR_ARIMA_tune_para_obj_CLR_95[ij,iw] = dum$tune_para_find_M_obj
        rm(dum); rm(iw)
    }
    print(ij); rm(ij); rm(female_data); rm(male_data)
}

horizon_specific_MFTS_int_fore_subnational_err_F_EVR_ARIMA_CLR_95 = apply(MFTS_int_fore_subnational_err_F_EVR_ARIMA_CLR_95, c(2, 3), mean)
horizon_specific_MFTS_int_fore_subnational_err_M_EVR_ARIMA_CLR_95 = apply(MFTS_int_fore_subnational_err_M_EVR_ARIMA_CLR_95, c(2, 3), mean)

horizon_specific_MFTS_int_fore_subnational_err_F_EVR_ARIMA_CLR_95_prefecture = apply(MFTS_int_fore_subnational_err_F_EVR_ARIMA_CLR_95, c(1, 3), mean)
horizon_specific_MFTS_int_fore_subnational_err_M_EVR_ARIMA_CLR_95_prefecture = apply(MFTS_int_fore_subnational_err_M_EVR_ARIMA_CLR_95, c(1, 3), mean)

# K = 6

MFTS_int_fore_subnational_err_F_K6_ARIMA_CLR_95 = MFTS_int_fore_subnational_err_M_K6_ARIMA_CLR_95 = array(NA, dim = c(n_state, 20, 3), dimnames = list(state, 1:20, c("ECP", "CPD", "score")))
MFTS_int_fore_subnational_err_F_K6_ARIMA_tune_para_CLR_95 = MFTS_int_fore_subnational_err_F_K6_ARIMA_tune_para_obj_CLR_95 = 
MFTS_int_fore_subnational_err_M_K6_ARIMA_tune_para_CLR_95 = MFTS_int_fore_subnational_err_M_K6_ARIMA_tune_para_obj_CLR_95 = matrix(NA, n_state, 20)
for(ij in 1:n_state)
{
    female_data = replace(female_prefecture_dx[[ij]], which(female_prefecture_dx[[ij]] == 0), 10^-5)
    male_data   = replace(male_prefecture_dx[[ij]],   which(male_prefecture_dx[[ij]] == 0),   10^-5)
    for(iw in 1:20)
    {
        dum = interval_fore_subnational_cdf_MFTS(fdata_F = female_data, 
                                                 fdata_M = male_data, 
                                                 fore_method = "CLR", 
                                                 horizon = iw, way_ncomp = "provide", uni_fore_method = "arima",
                                                 level_sig = 0.95)
        MFTS_int_fore_subnational_err_F_K6_ARIMA_CLR_95[ij,iw,] = dum$int_F_err
        MFTS_int_fore_subnational_err_F_K6_ARIMA_tune_para_CLR_95[ij,iw] = dum$tune_para_find_F
        MFTS_int_fore_subnational_err_F_K6_ARIMA_tune_para_obj_CLR_95[ij,iw] = dum$tune_para_find_F_obj
        
        MFTS_int_fore_subnational_err_M_K6_ARIMA_CLR_95[ij,iw,] = dum$int_M_err
        MFTS_int_fore_subnational_err_M_K6_ARIMA_tune_para_CLR_95[ij,iw] = dum$tune_para_find_M
        MFTS_int_fore_subnational_err_M_K6_ARIMA_tune_para_obj_CLR_95[ij,iw] = dum$tune_para_find_M_obj
        rm(dum); rm(iw)
    }
    print(ij); rm(ij); rm(female_data); rm(male_data)
}

horizon_specific_MFTS_int_fore_subnational_err_F_K6_ARIMA_CLR_95 = apply(MFTS_int_fore_subnational_err_F_K6_ARIMA_CLR_95, c(2, 3), mean)
horizon_specific_MFTS_int_fore_subnational_err_M_K6_ARIMA_CLR_95 = apply(MFTS_int_fore_subnational_err_M_K6_ARIMA_CLR_95, c(2, 3), mean)

horizon_specific_MFTS_int_fore_subnational_err_F_K6_ARIMA_CLR_95_prefecture = apply(MFTS_int_fore_subnational_err_F_K6_ARIMA_CLR_95, c(1, 3), mean)
horizon_specific_MFTS_int_fore_subnational_err_M_K6_ARIMA_CLR_95_prefecture = apply(MFTS_int_fore_subnational_err_M_K6_ARIMA_CLR_95, c(1, 3), mean)

