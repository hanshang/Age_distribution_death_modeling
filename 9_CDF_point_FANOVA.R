#######
## ETS
#######

point_fore_subnational_err_FANOVA_FFM_F_ETS = 
point_fore_subnational_err_FANOVA_FFM_M_ETS = array(NA, dim = c(n_state, 20, 4), dimnames = list(state, 1:20, c("KLD", "JSD (geo)","Wasserstein L1", "Wasserstein L2")))
for(iwk in 1:20)
{
    dum = point_fore_national_cdf_FANOVA_FFM(fdata_F = female_prefecture_dx_array, fdata_M = male_prefecture_dx_array, 
                                             fore_method = "CDF", horizon = iwk, uni_fore_method = "ets")
    point_fore_subnational_err_FANOVA_FFM_F_ETS[,iwk,] = t(dum$err_F)
    point_fore_subnational_err_FANOVA_FFM_M_ETS[,iwk,] = t(dum$err_M)
    print(iwk); rm(iwk)
}

## female

# average across horizons

point_fore_subnational_err_FANOVA_FFM_F_ETS_mean = round(rbind(apply(point_fore_subnational_err_FANOVA_FFM_F_ETS, c(1, 3), mean),
                                                            colMeans(apply(point_fore_subnational_err_FANOVA_FFM_F_ETS, c(1, 3), mean)),
                                                            apply(apply(point_fore_subnational_err_FANOVA_FFM_F_ETS, c(1, 3), mean), 2, median)), 4)
rownames(point_fore_subnational_err_FANOVA_FFM_F_ETS_mean) = c(state, "Mean", "Median")  

# average across prefectures

horizon_point_fore_subnational_err_FANOVA_FFM_F_ETS_mean = rbind(apply(point_fore_subnational_err_FANOVA_FFM_F_ETS, c(2, 3), mean),
                                                              colMeans(apply(point_fore_subnational_err_FANOVA_FFM_F_ETS, c(2, 3), mean)),
                                                              apply(apply(point_fore_subnational_err_FANOVA_FFM_F_ETS, c(2, 3), mean), 2, median))
rownames(horizon_point_fore_subnational_err_FANOVA_FFM_F_ETS_mean) = c(1:20, "Mean", "Median")

## male

# average across horizons

point_fore_subnational_err_FANOVA_FFM_M_ETS_mean = round(rbind(apply(point_fore_subnational_err_FANOVA_FFM_M_ETS, c(1, 3), mean),
                                                            colMeans(apply(point_fore_subnational_err_FANOVA_FFM_M_ETS, c(1, 3), mean)),
                                                            apply(apply(point_fore_subnational_err_FANOVA_FFM_M_ETS, c(1, 3), mean), 2, median)), 4)
rownames(point_fore_subnational_err_FANOVA_FFM_M_ETS_mean) = c(state, "Mean", "Median")

# average across prefectures

horizon_point_fore_subnational_err_FANOVA_FFM_M_ETS_mean = rbind(apply(point_fore_subnational_err_FANOVA_FFM_M_ETS, c(2, 3), mean),
                                                              colMeans(apply(point_fore_subnational_err_FANOVA_FFM_M_ETS, c(2, 3), mean)),
                                                              apply(apply(point_fore_subnational_err_FANOVA_FFM_M_ETS, c(2, 3), mean), 2, median))
rownames(horizon_point_fore_subnational_err_FANOVA_FFM_M_ETS_mean) = c(1:20, "Mean", "Median")

########
# ARIMA
########

point_fore_subnational_err_FANOVA_FFM_F_ARIMA =
point_fore_subnational_err_FANOVA_FFM_M_ARIMA = array(NA, dim = c(n_state, 20, 4), dimnames = list(state, 1:20, c("KLD", "JSD (geo)","Wasserstein L1", "Wasserstein L2")))
for(iwk in 1:20)
{
    dum = point_fore_national_cdf_FANOVA_FFM(fdata_F = female_prefecture_dx_array, fdata_M = male_prefecture_dx_array,
                                             fore_method = "CDF", horizon = iwk, uni_fore_method = "arima")
    point_fore_subnational_err_FANOVA_FFM_F_ARIMA[,iwk,] = t(dum$err_F)
    point_fore_subnational_err_FANOVA_FFM_M_ARIMA[,iwk,] = t(dum$err_M)
    print(iwk); rm(iwk)
}

## female

# average across horizons

point_fore_subnational_err_FANOVA_FFM_F_ARIMA_mean = round(rbind(apply(point_fore_subnational_err_FANOVA_FFM_F_ARIMA, c(1, 3), mean),
                                                               colMeans(apply(point_fore_subnational_err_FANOVA_FFM_F_ARIMA, c(1, 3), mean)),
                                                               apply(apply(point_fore_subnational_err_FANOVA_FFM_F_ARIMA, c(1, 3), mean), 2, median)), 4)
rownames(point_fore_subnational_err_FANOVA_FFM_F_ARIMA_mean) = c(state, "Mean", "Median")  

# average across prefectures

horizon_point_fore_subnational_err_FANOVA_FFM_F_ARIMA_mean = rbind(apply(point_fore_subnational_err_FANOVA_FFM_F_ARIMA, c(2, 3), mean),
                                                                 colMeans(apply(point_fore_subnational_err_FANOVA_FFM_F_ARIMA, c(2, 3), mean)),
                                                                 apply(apply(point_fore_subnational_err_FANOVA_FFM_F_ARIMA, c(2, 3), mean), 2, median))
rownames(horizon_point_fore_subnational_err_FANOVA_FFM_F_ARIMA_mean) = c(1:20, "Mean", "Median")

## male

# average across horizons

point_fore_subnational_err_FANOVA_FFM_M_ARIMA_mean = round(rbind(apply(point_fore_subnational_err_FANOVA_FFM_M_ARIMA, c(1, 3), mean),
                                                               colMeans(apply(point_fore_subnational_err_FANOVA_FFM_M_ARIMA, c(1, 3), mean)),
                                                               apply(apply(point_fore_subnational_err_FANOVA_FFM_M_ARIMA, c(1, 3), mean), 2, median)), 4)
rownames(point_fore_subnational_err_FANOVA_FFM_M_ARIMA_mean) = c(state, "Mean", "Median")

# average across prefectures

horizon_point_fore_subnational_err_FANOVA_FFM_M_ARIMA_mean = rbind(apply(point_fore_subnational_err_FANOVA_FFM_M_ARIMA, c(2, 3), mean),
                                                                 colMeans(apply(point_fore_subnational_err_FANOVA_FFM_M_ARIMA, c(2, 3), mean)),
                                                                 apply(apply(point_fore_subnational_err_FANOVA_FFM_M_ARIMA, c(2, 3), mean), 2, median))
rownames(horizon_point_fore_subnational_err_FANOVA_FFM_M_ARIMA_mean) = c(1:20, "Mean", "Median")

