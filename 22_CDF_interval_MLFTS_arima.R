###############################
## level of significance = 0.8
###############################

# EVR

MLFTS_int_fore_subnational_err_F_EVR_ARIMA = MLFTS_int_fore_subnational_err_M_EVR_ARIMA = array(NA, dim = c(20, 3, n_state), dimnames = list(1:20, c("ECP", "CPD", "MIS"), state))
MLFTS_int_fore_subnational_err_F_EVR_ARIMA_tune_para = MLFTS_int_fore_subnational_err_F_EVR_ARIMA_tune_para_obj =
MLFTS_int_fore_subnational_err_M_EVR_ARIMA_tune_para = MLFTS_int_fore_subnational_err_M_EVR_ARIMA_tune_para_obj = matrix(NA, 20, n_state)
for(ij in 1:n_state)
{
    for(iw in 1:20)
    {
        dum = interval_fore_subnational_cdf_MLFTS(fdata_F = female_prefecture_dx[[ij]],
                                                  fdata_M = male_prefecture_dx[[ij]],
                                                  fdata_common = NULL,
                                                  fore_method = "CDF",
                                                  horizon = iw, way_ncomp = "EVR",
                                                  level_sig = 0.8, uni_fore_method = "arima")
        MLFTS_int_fore_subnational_err_F_EVR_ARIMA[iw,,ij] = dum$int_F_err
        MLFTS_int_fore_subnational_err_F_EVR_ARIMA_tune_para[iw,ij] = dum$tune_para_find_F
        MLFTS_int_fore_subnational_err_F_EVR_ARIMA_tune_para_obj[iw,ij] = dum$tune_para_find_F_obj
        
        MLFTS_int_fore_subnational_err_M_EVR_ARIMA[iw,,ij] = dum$int_M_err
        MLFTS_int_fore_subnational_err_M_EVR_ARIMA_tune_para[iw,ij] = dum$tune_para_find_M
        MLFTS_int_fore_subnational_err_M_EVR_ARIMA_tune_para_obj[iw,ij] = dum$tune_para_find_M_obj
        rm(dum); print(iw); rm(iw)
    }
    print(ij); rm(ij)
}

MLFTS_int_fore_subnational_err_F_EVR_ARIMA_mean = apply(MLFTS_int_fore_subnational_err_F_EVR_ARIMA, c(1, 2), mean)
MLFTS_int_fore_subnational_err_M_EVR_ARIMA_mean = apply(MLFTS_int_fore_subnational_err_M_EVR_ARIMA, c(1, 2), mean)

colnames(MLFTS_int_fore_subnational_err_F_EVR_ARIMA_mean) = colnames(MLFTS_int_fore_subnational_err_M_EVR_ARIMA_mean) = c("ECP", "CPD", "score")
rownames(MLFTS_int_fore_subnational_err_F_EVR_ARIMA_mean) = rownames(MLFTS_int_fore_subnational_err_M_EVR_ARIMA_mean) = 1:20

MLFTS_int_fore_subnational_err_F_EVR_ARIMA_mean_prefecture = t(apply(MLFTS_int_fore_subnational_err_F_EVR_ARIMA, c(2, 3), mean))
MLFTS_int_fore_subnational_err_M_EVR_ARIMA_mean_prefecture = t(apply(MLFTS_int_fore_subnational_err_M_EVR_ARIMA, c(2, 3), mean))

# K = 6

MLFTS_int_fore_subnational_err_F_K6_ARIMA = MLFTS_int_fore_subnational_err_M_K6_ARIMA = array(NA, dim = c(20, 3, n_state), dimnames = list(1:20, c("ECP", "CPD", "MIS"), state))
MLFTS_int_fore_subnational_err_F_K6_ARIMA_tune_para = MLFTS_int_fore_subnational_err_F_K6_ARIMA_tune_para_obj =
MLFTS_int_fore_subnational_err_M_K6_ARIMA_tune_para = MLFTS_int_fore_subnational_err_M_K6_ARIMA_tune_para_obj = matrix(NA, 20, n_state)
for(ij in 1:n_state)
{
    for(iw in 1:20)
    {
        dum = interval_fore_subnational_cdf_MLFTS(fdata_F = female_prefecture_dx[[ij]],
                                                  fdata_M = male_prefecture_dx[[ij]],
                                                  fdata_common = NULL, fore_method = "CDF",
                                                  horizon = iw, way_ncomp = "provide",
                                                  level_sig = 0.8, uni_fore_method = "arima")
        MLFTS_int_fore_subnational_err_F_K6_ARIMA[iw,,ij] = dum$int_F_err
        MLFTS_int_fore_subnational_err_F_K6_ARIMA_tune_para[iw,ij] = dum$tune_para_find_F
        MLFTS_int_fore_subnational_err_F_K6_ARIMA_tune_para_obj[iw,ij] = dum$tune_para_find_F_obj
        
        MLFTS_int_fore_subnational_err_M_K6_ARIMA[iw,,ij] = dum$int_M_err
        MLFTS_int_fore_subnational_err_M_K6_ARIMA_tune_para[iw,ij] = dum$tune_para_find_M
        MLFTS_int_fore_subnational_err_M_K6_ARIMA_tune_para_obj[iw,ij] = dum$tune_para_find_M_obj
        rm(dum); print(iw); rm(iw)
    }
    print(ij); rm(ij)
}

MLFTS_int_fore_subnational_err_F_K6_ARIMA_mean = apply(MLFTS_int_fore_subnational_err_F_K6_ARIMA, c(1, 2), mean)
MLFTS_int_fore_subnational_err_M_K6_ARIMA_mean = apply(MLFTS_int_fore_subnational_err_M_K6_ARIMA, c(1, 2), mean)

colnames(MLFTS_int_fore_subnational_err_F_K6_ARIMA_mean) = colnames(MLFTS_int_fore_subnational_err_M_K6_ARIMA_mean) = c("ECP", "CPD", "score")
rownames(MLFTS_int_fore_subnational_err_F_K6_ARIMA_mean) = rownames(MLFTS_int_fore_subnational_err_M_K6_ARIMA_mean) = 1:20

MLFTS_int_fore_subnational_err_F_K6_ARIMA_mean_prefecture = t(apply(MLFTS_int_fore_subnational_err_F_K6_ARIMA, c(2, 3), mean))
MLFTS_int_fore_subnational_err_M_K6_ARIMA_mean_prefecture = t(apply(MLFTS_int_fore_subnational_err_M_K6_ARIMA, c(2, 3), mean))


################################
## level of significance = 0.95
################################

# EVR

MLFTS_int_fore_subnational_err_F_EVR_ARIMA_alpha_0.95 = MLFTS_int_fore_subnational_err_M_EVR_ARIMA_alpha_0.95 = array(NA, dim = c(20, 3, n_state), dimnames = list(1:20, c("ECP", "CPD", "MIS"), state))
MLFTS_int_fore_subnational_err_F_EVR_ARIMA_tune_para_alpha_0.95 = MLFTS_int_fore_subnational_err_F_EVR_ARIMA_tune_para_obj_alpha_0.95 =
MLFTS_int_fore_subnational_err_M_EVR_ARIMA_tune_para_alpha_0.95 = MLFTS_int_fore_subnational_err_M_EVR_ARIMA_tune_para_obj_alpha_0.95 = matrix(NA, 20, n_state)
for(ij in 1:n_state)
{
    for(iw in 1:20)
    {
        dum = interval_fore_subnational_cdf_MLFTS(fdata_F = female_prefecture_dx[[ij]],
                                                  fdata_M = male_prefecture_dx[[ij]],
                                                  fdata_common = NULL, fore_method = "CDF",
                                                  horizon = iw, way_ncomp = "EVR",
                                                  level_sig = 0.95, uni_fore_method = "arima")
        MLFTS_int_fore_subnational_err_F_EVR_ARIMA_alpha_0.95[iw,,ij] = dum$int_F_err
        MLFTS_int_fore_subnational_err_F_EVR_ARIMA_tune_para_alpha_0.95[iw,ij] = dum$tune_para_find_F
        MLFTS_int_fore_subnational_err_F_EVR_ARIMA_tune_para_obj_alpha_0.95[iw,ij] = dum$tune_para_find_F_obj
        
        MLFTS_int_fore_subnational_err_M_EVR_ARIMA_alpha_0.95[iw,,ij] = dum$int_M_err
        MLFTS_int_fore_subnational_err_M_EVR_ARIMA_tune_para_alpha_0.95[iw,ij] = dum$tune_para_find_M
        MLFTS_int_fore_subnational_err_M_EVR_ARIMA_tune_para_obj_alpha_0.95[iw,ij] = dum$tune_para_find_M_obj
        rm(dum); rm(iw)
    }
    print(ij); rm(ij)
}

MLFTS_int_fore_subnational_err_F_EVR_ARIMA_alpha_0.95_mean = apply(MLFTS_int_fore_subnational_err_F_EVR_ARIMA_alpha_0.95, c(1, 2), mean)
MLFTS_int_fore_subnational_err_M_EVR_ARIMA_alpha_0.95_mean = apply(MLFTS_int_fore_subnational_err_M_EVR_ARIMA_alpha_0.95, c(1, 2), mean)

colnames(MLFTS_int_fore_subnational_err_F_EVR_ARIMA_alpha_0.95_mean) = colnames(MLFTS_int_fore_subnational_err_M_EVR_ARIMA_alpha_0.95_mean) = c("ECP", "CPD", "score")
rownames(MLFTS_int_fore_subnational_err_F_EVR_ARIMA_alpha_0.95_mean) = rownames(MLFTS_int_fore_subnational_err_M_EVR_ARIMA_alpha_0.95_mean) = 1:20

MLFTS_int_fore_subnational_err_F_EVR_ARIMA_alpha_0.95_mean_prefecture = t(apply(MLFTS_int_fore_subnational_err_F_EVR_ARIMA_alpha_0.95, c(2, 3), mean))
MLFTS_int_fore_subnational_err_M_EVR_ARIMA_alpha_0.95_mean_prefecture = t(apply(MLFTS_int_fore_subnational_err_M_EVR_ARIMA_alpha_0.95, c(2, 3), mean))

# K = 6

MLFTS_int_fore_subnational_err_F_K6_ARIMA_alpha_0.95 = MLFTS_int_fore_subnational_err_M_K6_ARIMA_alpha_0.95 = array(NA, dim = c(20, 3, n_state), dimnames = list(1:20, c("ECP", "CPD", "MIS"), state))
MLFTS_int_fore_subnational_err_F_K6_ARIMA_tune_para_alpha_0.95 = MLFTS_int_fore_subnational_err_F_K6_ARIMA_tune_para_obj_alpha_0.95 =
MLFTS_int_fore_subnational_err_M_K6_ARIMA_tune_para_alpha_0.95 = MLFTS_int_fore_subnational_err_M_K6_ARIMA_tune_para_obj_alpha_0.95 = matrix(NA, 20, n_state)
for(ij in 1:n_state)
{
    for(iw in 1:20)
    {
        dum = interval_fore_subnational_cdf_MLFTS(fdata_F = female_prefecture_dx[[ij]],
                                                  fdata_M = male_prefecture_dx[[ij]],
                                                  fdata_common = NULL,
                                                  fore_method = "CDF",
                                                  horizon = iw, way_ncomp = "provide",
                                                  level_sig = 0.95, uni_fore_method = "arima")
        MLFTS_int_fore_subnational_err_F_K6_ARIMA_alpha_0.95[iw,,ij] = dum$int_F_err
        MLFTS_int_fore_subnational_err_F_K6_ARIMA_tune_para_alpha_0.95[iw,ij] = dum$tune_para_find_F
        MLFTS_int_fore_subnational_err_F_K6_ARIMA_tune_para_obj_alpha_0.95[iw,ij] = dum$tune_para_find_F_obj
        
        MLFTS_int_fore_subnational_err_M_K6_ARIMA_alpha_0.95[iw,,ij] = dum$int_M_err
        MLFTS_int_fore_subnational_err_M_K6_ARIMA_tune_para_alpha_0.95[iw,ij] = dum$tune_para_find_M
        MLFTS_int_fore_subnational_err_M_K6_ARIMA_tune_para_obj_alpha_0.95[iw,ij] = dum$tune_para_find_M_obj
        rm(dum); rm(iw)
    }
    print(ij); rm(ij)
}

MLFTS_int_fore_subnational_err_F_K6_ARIMA_alpha_0.95_mean = apply(MLFTS_int_fore_subnational_err_F_K6_ARIMA_alpha_0.95, c(1, 2), mean)
MLFTS_int_fore_subnational_err_M_K6_ARIMA_alpha_0.95_mean = apply(MLFTS_int_fore_subnational_err_M_K6_ARIMA_alpha_0.95, c(1, 2), mean)

colnames(MLFTS_int_fore_subnational_err_F_K6_ARIMA_alpha_0.95_mean) = colnames(MLFTS_int_fore_subnational_err_M_K6_ARIMA_alpha_0.95_mean) = c("ECP", "CPD", "score")
rownames(MLFTS_int_fore_subnational_err_F_K6_ARIMA_alpha_0.95_mean) = rownames(MLFTS_int_fore_subnational_err_M_K6_ARIMA_alpha_0.95_mean) = 1:20

MLFTS_int_fore_subnational_err_F_K6_ARIMA_alpha_0.95_mean_prefecture = t(apply(MLFTS_int_fore_subnational_err_F_K6_ARIMA_alpha_0.95, c(2, 3), mean))
MLFTS_int_fore_subnational_err_M_K6_ARIMA_alpha_0.95_mean_prefecture = t(apply(MLFTS_int_fore_subnational_err_M_K6_ARIMA_alpha_0.95, c(2, 3), mean))

