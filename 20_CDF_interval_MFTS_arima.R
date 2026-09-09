# load R packages

source("load_packages.R")
source("auxiliary_interval.R")

###############################
## level of significance = 0.8
###############################

# EVR

MFTS_int_fore_subnational_err_F_EVR_ARIMA = MFTS_int_fore_subnational_err_M_EVR_ARIMA = array(NA, dim = c(20, 3, n_state), dimnames = list(1:20, c("ECP", "CPD", "MIS"), state))
MFTS_int_fore_subnational_err_F_EVR_ARIMA_tune_para = MFTS_int_fore_subnational_err_F_EVR_ARIMA_tune_para_obj = 
MFTS_int_fore_subnational_err_M_EVR_ARIMA_tune_para = MFTS_int_fore_subnational_err_M_EVR_ARIMA_tune_para_obj = matrix(NA, 20, n_state)
for(ij in 1:n_state)
{
    for(iw in 1:20)
    {
        dum = interval_fore_subnational_cdf_MFTS(fdata_F = female_prefecture_dx[[ij]], 
                                                 fdata_M = male_prefecture_dx[[ij]], 
                                                 fore_method = "CDF", horizon = iw, 
                                                 way_ncomp = "EVR", uni_fore_method = "arima",
                                                 level_sig = 0.8)
        MFTS_int_fore_subnational_err_F_EVR_ARIMA[iw,,ij] = dum$int_F_err
        MFTS_int_fore_subnational_err_F_EVR_ARIMA_tune_para[iw,ij] = dum$tune_para_find_F
        MFTS_int_fore_subnational_err_F_EVR_ARIMA_tune_para_obj[iw,ij] = dum$tune_para_find_F_obj
        
        MFTS_int_fore_subnational_err_M_EVR_ARIMA[iw,,ij] = dum$int_M_err
        MFTS_int_fore_subnational_err_M_EVR_ARIMA_tune_para[iw,ij] = dum$tune_para_find_M
        MFTS_int_fore_subnational_err_M_EVR_ARIMA_tune_para_obj[iw,ij] = dum$tune_para_find_M_obj
        rm(dum); rm(iw)
    }
    print(ij); rm(ij)
}

MFTS_int_fore_subnational_err_F_EVR_ARIMA_mean = apply(MFTS_int_fore_subnational_err_F_EVR_ARIMA, c(1, 2), mean)
MFTS_int_fore_subnational_err_M_EVR_ARIMA_mean = apply(MFTS_int_fore_subnational_err_M_EVR_ARIMA, c(1, 2), mean)

colnames(MFTS_int_fore_subnational_err_F_EVR_ARIMA_mean) = colnames(MFTS_int_fore_subnational_err_M_EVR_ARIMA_mean) = c("ECP", "CPD", "score")
rownames(MFTS_int_fore_subnational_err_F_EVR_ARIMA_mean) = rownames(MFTS_int_fore_subnational_err_M_EVR_ARIMA_mean) = 1:20

MFTS_int_fore_subnational_err_F_EVR_ARIMA_mean_prefecture = t(apply(MFTS_int_fore_subnational_err_F_EVR_ARIMA, c(2, 3), mean))
MFTS_int_fore_subnational_err_M_EVR_ARIMA_mean_prefecture = t(apply(MFTS_int_fore_subnational_err_M_EVR_ARIMA, c(2, 3), mean))


# K = 6

MFTS_int_fore_subnational_err_F_K6_ARIMA = MFTS_int_fore_subnational_err_M_K6_ARIMA = array(NA, dim = c(20, 3, n_state), dimnames = list(1:20, c("ECP", "CPD", "MIS"), state))
MFTS_int_fore_subnational_err_F_K6_ARIMA_tune_para = MFTS_int_fore_subnational_err_F_K6_ARIMA_tune_para_obj = 
MFTS_int_fore_subnational_err_M_K6_ARIMA_tune_para = MFTS_int_fore_subnational_err_M_K6_ARIMA_tune_para_obj = matrix(NA, 20, n_state)
for(ij in 1:n_state)
{
    for(iw in 1:20)
    {
        dum = interval_fore_subnational_cdf_MFTS(fdata_F = female_prefecture_dx[[ij]], 
                                                 fdata_M = male_prefecture_dx[[ij]], 
                                                 fore_method = "CDF", horizon = iw, 
                                                 way_ncomp = "provide", uni_fore_method = "arima",
                                                 level_sig = 0.8)
        MFTS_int_fore_subnational_err_F_K6_ARIMA[iw,,ij] = dum$int_F_err
        MFTS_int_fore_subnational_err_F_K6_ARIMA_tune_para[iw,ij] = dum$tune_para_find_F
        MFTS_int_fore_subnational_err_F_K6_ARIMA_tune_para_obj[iw,ij] = dum$tune_para_find_F_obj
        
        MFTS_int_fore_subnational_err_M_K6_ARIMA[iw,,ij] = dum$int_M_err
        MFTS_int_fore_subnational_err_M_K6_ARIMA_tune_para[iw,ij] = dum$tune_para_find_M
        MFTS_int_fore_subnational_err_M_K6_ARIMA_tune_para_obj[iw,ij] = dum$tune_para_find_M_obj
        rm(dum); rm(iw)
    }
    print(ij); rm(ij)
}

MFTS_int_fore_subnational_err_F_K6_ARIMA_mean = apply(MFTS_int_fore_subnational_err_F_K6_ARIMA, c(1, 2), mean)
MFTS_int_fore_subnational_err_M_K6_ARIMA_mean = apply(MFTS_int_fore_subnational_err_M_K6_ARIMA, c(1, 2), mean)

colnames(MFTS_int_fore_subnational_err_F_K6_ARIMA_mean) = colnames(MFTS_int_fore_subnational_err_M_K6_ARIMA_mean) = c("ECP", "CPD", "score")
rownames(MFTS_int_fore_subnational_err_F_K6_ARIMA_mean) = rownames(MFTS_int_fore_subnational_err_M_K6_ARIMA_mean) = 1:20

MFTS_int_fore_subnational_err_F_K6_ARIMA_mean_prefecture = t(apply(MFTS_int_fore_subnational_err_F_K6_ARIMA, c(2, 3), mean))
MFTS_int_fore_subnational_err_M_K6_ARIMA_mean_prefecture = t(apply(MFTS_int_fore_subnational_err_M_K6_ARIMA, c(2, 3), mean))


################################
## level of significance = 0.95
################################

# EVR

MFTS_int_fore_subnational_err_F_EVR_ARIMA_alpha_0.95 = MFTS_int_fore_subnational_err_M_EVR_ARIMA_alpha_0.95 = array(NA, dim = c(20, 3, n_state), dimnames = list(1:20, c("ECP", "CPD", "MIS"), state))
MFTS_int_fore_subnational_err_F_EVR_ARIMA_tune_para_alpha_0.95 = MFTS_int_fore_subnational_err_F_EVR_ARIMA_tune_para_obj_alpha_0.95 = 
MFTS_int_fore_subnational_err_M_EVR_ARIMA_tune_para_alpha_0.95 = MFTS_int_fore_subnational_err_M_EVR_ARIMA_tune_para_obj_alpha_0.95 = matrix(NA, 20, n_state)
for(ij in 1:n_state)
{
    for(iw in 1:20)
    {
        dum = interval_fore_subnational_cdf_MFTS(fdata_F = female_prefecture_dx[[ij]], 
                                                 fdata_M = male_prefecture_dx[[ij]], 
                                                 fore_method = "CDF", 
                                                 horizon = iw, way_ncomp = "EVR", uni_fore_method = "arima",
                                                 level_sig = 0.95)
        MFTS_int_fore_subnational_err_F_EVR_ARIMA_alpha_0.95[iw,,ij] = dum$int_F_err
        MFTS_int_fore_subnational_err_F_EVR_ARIMA_tune_para_alpha_0.95[iw,ij] = dum$tune_para_find_F
        MFTS_int_fore_subnational_err_F_EVR_ARIMA_tune_para_obj_alpha_0.95[iw,ij] = dum$tune_para_find_F_obj
        
        MFTS_int_fore_subnational_err_M_EVR_ARIMA_alpha_0.95[iw,,ij] = dum$int_M_err
        MFTS_int_fore_subnational_err_M_EVR_ARIMA_tune_para_alpha_0.95[iw,ij] = dum$tune_para_find_M
        MFTS_int_fore_subnational_err_M_EVR_ARIMA_tune_para_obj_alpha_0.95[iw,ij] = dum$tune_para_find_M_obj
        rm(dum); rm(iw)
    }
    print(ij); rm(ij)
}

MFTS_int_fore_subnational_err_F_EVR_ARIMA_alpha_0.95_mean = apply(MFTS_int_fore_subnational_err_F_EVR_ARIMA_alpha_0.95, c(1, 2), mean)
MFTS_int_fore_subnational_err_M_EVR_ARIMA_alpha_0.95_mean = apply(MFTS_int_fore_subnational_err_M_EVR_ARIMA_alpha_0.95, c(1, 2), mean)

colnames(MFTS_int_fore_subnational_err_F_EVR_ARIMA_alpha_0.95_mean) = colnames(MFTS_int_fore_subnational_err_M_EVR_ARIMA_alpha_0.95_mean) = c("ECP", "CPD", "score")
rownames(MFTS_int_fore_subnational_err_F_EVR_ARIMA_alpha_0.95_mean) = rownames(MFTS_int_fore_subnational_err_M_EVR_ARIMA_alpha_0.95_mean) = 1:20

MFTS_int_fore_subnational_err_F_EVR_ARIMA_alpha_0.95_mean_prefecture = t(apply(MFTS_int_fore_subnational_err_F_EVR_ARIMA_alpha_0.95, c(2, 3), mean))
MFTS_int_fore_subnational_err_M_EVR_ARIMA_alpha_0.95_mean_prefecture = t(apply(MFTS_int_fore_subnational_err_M_EVR_ARIMA_alpha_0.95, c(2, 3), mean))


# K = 6

MFTS_int_fore_subnational_err_F_K6_ARIMA_alpha_0.95 = MFTS_int_fore_subnational_err_M_K6_ARIMA_alpha_0.95 = array(NA, dim = c(20, 3, n_state), dimnames = list(1:20, c("ECP", "CPD", "MIS"), state))
MFTS_int_fore_subnational_err_F_K6_ARIMA_tune_para_alpha_0.95 = MFTS_int_fore_subnational_err_F_K6_ARIMA_tune_para_obj_alpha_0.95 = 
MFTS_int_fore_subnational_err_M_K6_ARIMA_tune_para_alpha_0.95 = MFTS_int_fore_subnational_err_M_K6_ARIMA_tune_para_obj_alpha_0.95 = matrix(NA, 20, n_state)
for(ij in 1:n_state)
{
    for(iw in 1:20)
    {
        dum = interval_fore_subnational_cdf_MFTS(fdata_F = female_prefecture_dx[[ij]], 
                                                 fdata_M = male_prefecture_dx[[ij]], 
                                                 fore_method = "CDF", horizon = iw, 
                                                 way_ncomp = "provide", uni_fore_method = "arima",
                                                 level_sig = 0.95)
        MFTS_int_fore_subnational_err_F_K6_ARIMA_alpha_0.95[iw,,ij] = dum$int_F_err
        MFTS_int_fore_subnational_err_F_K6_ARIMA_tune_para_alpha_0.95[iw,ij] = dum$tune_para_find_F
        MFTS_int_fore_subnational_err_F_K6_ARIMA_tune_para_obj_alpha_0.95[iw,ij] = dum$tune_para_find_F_obj
        
        MFTS_int_fore_subnational_err_M_K6_ARIMA_alpha_0.95[iw,,ij] = dum$int_M_err
        MFTS_int_fore_subnational_err_M_K6_ARIMA_tune_para_alpha_0.95[iw,ij] = dum$tune_para_find_M
        MFTS_int_fore_subnational_err_M_K6_ARIMA_tune_para_obj_alpha_0.95[iw,ij] = dum$tune_para_find_M_obj
        rm(dum); rm(iw)
    }
    print(ij); rm(ij)
}

MFTS_int_fore_subnational_err_F_K6_ARIMA_alpha_0.95_mean = apply(MFTS_int_fore_subnational_err_F_K6_ARIMA_alpha_0.95, c(1, 2), mean)
MFTS_int_fore_subnational_err_M_K6_ARIMA_alpha_0.95_mean = apply(MFTS_int_fore_subnational_err_M_K6_ARIMA_alpha_0.95, c(1, 2), mean)

colnames(MFTS_int_fore_subnational_err_F_K6_ARIMA_alpha_0.95_mean) = colnames(MFTS_int_fore_subnational_err_M_K6_ARIMA_alpha_0.95_mean) = c("ECP", "CPD", "score")
rownames(MFTS_int_fore_subnational_err_F_K6_ARIMA_alpha_0.95_mean) = rownames(MFTS_int_fore_subnational_err_M_K6_ARIMA_alpha_0.95_mean) = 1:20

MFTS_int_fore_subnational_err_F_K6_ARIMA_alpha_0.95_mean_prefecture = t(apply(MFTS_int_fore_subnational_err_F_K6_ARIMA_alpha_0.95, c(2, 3), mean))
MFTS_int_fore_subnational_err_M_K6_ARIMA_alpha_0.95_mean_prefecture = t(apply(MFTS_int_fore_subnational_err_M_K6_ARIMA_alpha_0.95, c(2, 3), mean))

