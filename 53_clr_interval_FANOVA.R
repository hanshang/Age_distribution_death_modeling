###############
## alpha = 0.2
###############

# fmethod = "ets"

FANOVA_FFM_F_ets_int_err_CLR = FANOVA_FFM_M_ets_int_err_CLR = array(NA, dim = c(n_state, 3, 20),
                                                            dimnames = list(state, c("ECP", "CPD", "MIS"), 1:20))
for(iwk in 1:20)
{
    dum = interval_fore_subnational_cdf_FANOVA(fdata_F = female_prefecture_dx_array, 
                                               fdata_M = male_prefecture_dx_array, 
                                               fore_method = "CLR", horizon = iwk,  
                                               level_sig = 0.8, uni_fore_method = "ets")
    FANOVA_FFM_F_ets_int_err_CLR[,,iwk] = dum$int_F_err
    FANOVA_FFM_M_ets_int_err_CLR[,,iwk] = dum$int_M_err
    print(iwk); rm(iwk)
}

FANOVA_FFM_F_ets_int_err_mean_CLR = t(apply(FANOVA_FFM_F_ets_int_err_CLR, c(2, 3), mean))
FANOVA_FFM_M_ets_int_err_mean_CLR = t(apply(FANOVA_FFM_M_ets_int_err_CLR, c(2, 3), mean))

FANOVA_FFM_F_ets_int_err_mean_CLR_prefecture = apply(FANOVA_FFM_F_ets_int_err_CLR, c(1, 2), mean)
FANOVA_FFM_M_ets_int_err_mean_CLR_prefecture = apply(FANOVA_FFM_M_ets_int_err_CLR, c(1, 2), mean)

# fmethod = "arima"

FANOVA_FFM_F_arima_int_err_CLR = FANOVA_FFM_M_arima_int_err_CLR = array(NA, dim = c(n_state, 3, 20),
                                                                dimnames = list(state, c("ECP", "CPD", "MIS"), 1:20))
for(iwk in 1:20)
{
    dum = interval_fore_subnational_cdf_FANOVA(fdata_F = female_prefecture_dx_array, 
                                               fdata_M = male_prefecture_dx_array, 
                                               fore_method = "CLR", horizon = iwk,  
                                               level_sig = 0.8, uni_fore_method = "arima")
    FANOVA_FFM_F_arima_int_err_CLR[,,iwk] = dum$int_F_err
    FANOVA_FFM_M_arima_int_err_CLR[,,iwk] = dum$int_M_err
    print(iwk); rm(iwk)
}

FANOVA_FFM_F_arima_int_err_mean_CLR = t(apply(FANOVA_FFM_F_arima_int_err_CLR, c(2, 3), mean))
FANOVA_FFM_M_arima_int_err_mean_CLR = t(apply(FANOVA_FFM_M_arima_int_err_CLR, c(2, 3), mean))

FANOVA_FFM_F_arima_int_err_mean_CLR_prefecture = apply(FANOVA_FFM_F_arima_int_err_CLR, c(1, 2), mean)
FANOVA_FFM_M_arima_int_err_mean_CLR_prefecture = apply(FANOVA_FFM_M_arima_int_err_CLR, c(1, 2), mean)

################
## alpha = 0.05
################

# fmethod = "ets"

FANOVA_FFM_F_ets_int_err_95_CLR = FANOVA_FFM_M_ets_int_err_95_CLR = array(NA, dim = c(n_state, 3, 20),
                                                                  dimnames = list(state, c("ECP", "CPD", "MIS"), 1:20))
for(iwk in 1:20)
{
    dum = interval_fore_subnational_cdf_FANOVA(fdata_F = female_prefecture_dx_array, 
                                               fdata_M = male_prefecture_dx_array, 
                                               fore_method = "CLR", horizon = iwk,  
                                               level_sig = 0.95, uni_fore_method = "ets")
    FANOVA_FFM_F_ets_int_err_95_CLR[,,iwk] = dum$int_F_err
    FANOVA_FFM_M_ets_int_err_95_CLR[,,iwk] = dum$int_M_err
    print(iwk); rm(iwk)
}

FANOVA_FFM_F_ets_int_err_95_mean_CLR = t(apply(FANOVA_FFM_F_ets_int_err_95_CLR, c(2, 3), mean))
FANOVA_FFM_M_ets_int_err_95_mean_CLR = t(apply(FANOVA_FFM_M_ets_int_err_95_CLR, c(2, 3), mean))

FANOVA_FFM_F_ets_int_err_95_mean_CLR_prefecture = apply(FANOVA_FFM_F_ets_int_err_95_CLR, c(1, 2), mean)
FANOVA_FFM_M_ets_int_err_95_mean_CLR_prefecture = apply(FANOVA_FFM_M_ets_int_err_95_CLR, c(1, 2), mean)

# fmethod = "arima"

FANOVA_FFM_F_arima_int_err_95_CLR = FANOVA_FFM_M_arima_int_err_95_CLR = array(NA, dim = c(n_state, 3, 20),
                                                        dimnames = list(state, c("ECP", "CPD", "MIS"), 1:20))
for(iwk in 1:20)
{
    dum = interval_fore_subnational_cdf_FANOVA(fdata_F = female_prefecture_dx_array, 
                                               fdata_M = male_prefecture_dx_array, 
                                               fore_method = "CLR", horizon = iwk,  
                                               level_sig = 0.95, uni_fore_method = "arima")
    FANOVA_FFM_F_arima_int_err_95_CLR[,,iwk] = dum$int_F_err
    FANOVA_FFM_M_arima_int_err_95_CLR[,,iwk] = dum$int_M_err
    print(iwk); rm(iwk)
}

FANOVA_FFM_F_arima_int_err_mean_95_CLR = t(apply(FANOVA_FFM_F_arima_int_err_95_CLR, c(2, 3), mean))
FANOVA_FFM_M_arima_int_err_mean_95_CLR = t(apply(FANOVA_FFM_M_arima_int_err_95_CLR, c(2, 3), mean))

FANOVA_FFM_F_arima_int_err_mean_95_CLR_prefecture = apply(FANOVA_FFM_F_arima_int_err_95_CLR, c(1, 2), mean)
FANOVA_FFM_M_arima_int_err_mean_95_CLR_prefecture = apply(FANOVA_FFM_M_arima_int_err_95_CLR, c(1, 2), mean)

