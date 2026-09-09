####################################
# Multilevel functional time series
####################################

# load R packages

setwd("~/Dropbox/Todos/FANOVA_FFM_CDF/code")
source("load_packages.R")
source("auxiliary_point.R")

######
# ETS
######

point_fore_subnational_err_MLFTS_F_EVR_ETS_clr = point_fore_subnational_err_MLFTS_M_EVR_ETS_clr =
point_fore_subnational_err_MLFTS_F_K6_ETS_clr  = point_fore_subnational_err_MLFTS_M_K6_ETS_clr  = array(NA, dim = c(n_state, 20, 4),
                                            dimnames = list(state, 1:20, c("KLD", "JSD (geo)", "Wasserstein L1", "Wasserstein L2")))
for(ij in 1:n_state)
{
    female_data = replace(female_prefecture_dx[[ij]], which(female_prefecture_dx[[ij]] == 0), 10^-5)
    male_data   = replace(male_prefecture_dx[[ij]],   which(male_prefecture_dx[[ij]] == 0),   10^-5)
    for(iw in 1:20)
    {
        ## EVR
        
        dum = point_fore_national_cdf_MLFTS(fdata_F = female_data, fdata_M = male_data, fdata_common = NULL,
                                            fore_method = "CLR", horizon = iw, way_ncomp = "EVR",
                                            forecasting_method = "ets")
        point_fore_subnational_err_MLFTS_F_EVR_ETS_clr[ij,iw,] = dum$err_F
        point_fore_subnational_err_MLFTS_M_EVR_ETS_clr[ij,iw,] = dum$err_M
        rm(dum)
        
        ## K = 6
        
        dum = point_fore_national_cdf_MLFTS(fdata_F = female_data, fdata_M = male_data, fdata_common = NULL,
                                            fore_method = "CLR", horizon = iw, way_ncomp = "provide",
                                            forecasting_method = "ets")
        point_fore_subnational_err_MLFTS_F_K6_ETS_clr[ij,iw,] = dum$err_F
        point_fore_subnational_err_MLFTS_M_K6_ETS_clr[ij,iw,] = dum$err_M
        rm(dum); rm(iw)
    }
    print(ij); rm(ij); rm(female_data); rm(male_data)
}

## Female

# by prefecture

point_fore_subnational_err_F_EVR_ETS_MLFTS_clr_mean = rbind(apply(point_fore_subnational_err_MLFTS_F_EVR_ETS_clr, c(1, 3), mean), 
                                                        colMeans(apply(point_fore_subnational_err_MLFTS_F_EVR_ETS_clr, c(1, 3), mean)),
                                                        apply(apply(point_fore_subnational_err_MLFTS_F_EVR_ETS_clr, c(1, 3), mean), 2, median))

point_fore_subnational_err_F_K6_ETS_MLFTS_clr_mean  = rbind(apply(point_fore_subnational_err_MLFTS_F_K6_ETS_clr, c(1, 3), mean), 
                                                        colMeans(apply(point_fore_subnational_err_MLFTS_F_K6_ETS_clr, c(1, 3), mean)),
                                                        apply(apply(point_fore_subnational_err_MLFTS_F_K6_ETS_clr, c(1, 3), mean), 2, median))

# by horizon

horizon_point_fore_subnational_err_F_EVR_ETS_MLFTS_clr_mean = apply(point_fore_subnational_err_MLFTS_F_EVR_ETS_clr, c(2, 3), mean)
horizon_point_fore_subnational_err_F_K6_ETS_MLFTS_clr_mean = apply(point_fore_subnational_err_MLFTS_F_K6_ETS_clr, c(2, 3), mean)

## Male

# by prefecture

point_fore_subnational_err_M_EVR_ETS_MLFTS_clr_mean = rbind(apply(point_fore_subnational_err_MLFTS_M_EVR_ETS_clr, c(1, 3), mean), 
                                                        colMeans(apply(point_fore_subnational_err_MLFTS_M_EVR_ETS_clr, c(1, 3), mean)),
                                                        apply(apply(point_fore_subnational_err_MLFTS_M_EVR_ETS_clr, c(1, 3), mean), 2, median))

point_fore_subnational_err_M_K6_ETS_MLFTS_clr_mean  = rbind(apply(point_fore_subnational_err_MLFTS_M_K6_ETS_clr, c(1, 3), mean), 
                                                        colMeans(apply(point_fore_subnational_err_MLFTS_M_K6_ETS_clr, c(1, 3), mean)),
                                                        apply(apply(point_fore_subnational_err_MLFTS_M_K6_ETS_clr, c(1, 3), mean), 2, median))

rownames(point_fore_subnational_err_F_EVR_ETS_MLFTS_clr_mean) = rownames(point_fore_subnational_err_F_K6_ETS_MLFTS_clr_mean) = 
rownames(point_fore_subnational_err_M_EVR_ETS_MLFTS_clr_mean) = rownames(point_fore_subnational_err_M_K6_ETS_MLFTS_clr_mean) = c(state, "Mean", "Median")

# by horizon

horizon_point_fore_subnational_err_M_EVR_ETS_MLFTS_clr_mean = apply(point_fore_subnational_err_MLFTS_M_EVR_ETS_clr, c(2, 3), mean)
horizon_point_fore_subnational_err_M_K6_ETS_MLFTS_clr_mean = apply(point_fore_subnational_err_MLFTS_M_K6_ETS_clr, c(2, 3), mean)

########
# ARIMA
########

point_fore_subnational_err_MLFTS_F_EVR_ARIMA_clr = point_fore_subnational_err_MLFTS_M_EVR_ARIMA_clr =
point_fore_subnational_err_MLFTS_F_K6_ARIMA_clr  = point_fore_subnational_err_MLFTS_M_K6_ARIMA_clr  = array(NA, dim = c(n_state, 20, 4),
                                                                                                          dimnames = list(state, 1:20, c("KLD", "JSD (geo)", "Wasserstein L1", "Wasserstein L2")))
for(ij in 1:n_state)
{
    female_data = replace(female_prefecture_dx[[ij]], which(female_prefecture_dx[[ij]] == 0), 10^-5)
    male_data   = replace(male_prefecture_dx[[ij]],   which(male_prefecture_dx[[ij]] == 0),   10^-5)
    for(iw in 1:20)
    {
        ## EVR
        
        dum = point_fore_national_cdf_MLFTS(fdata_F = female_data, fdata_M = male_data, fdata_common = NULL,
                                            fore_method = "CLR", horizon = iw, way_ncomp = "EVR",
                                            forecasting_method = "arima")
        point_fore_subnational_err_MLFTS_F_EVR_ARIMA_clr[ij,iw,] = dum$err_F
        point_fore_subnational_err_MLFTS_M_EVR_ARIMA_clr[ij,iw,] = dum$err_M
        rm(dum)
        
        ## K = 6
        
        dum = point_fore_national_cdf_MLFTS(fdata_F = female_data, fdata_M = male_data, fdata_common = NULL,
                                            fore_method = "CLR", horizon = iw, way_ncomp = "provide",
                                            forecasting_method = "arima")
        point_fore_subnational_err_MLFTS_F_K6_ARIMA_clr[ij,iw,] = dum$err_F
        point_fore_subnational_err_MLFTS_M_K6_ARIMA_clr[ij,iw,] = dum$err_M
        rm(dum); rm(iw)
    }
    print(ij); rm(ij); rm(female_data); rm(male_data)
}

## Female

# by prefecture

point_fore_subnational_err_F_EVR_ARIMA_MLFTS_clr_mean = rbind(apply(point_fore_subnational_err_MLFTS_F_EVR_ARIMA_clr, c(1, 3), mean), 
                                                        colMeans(apply(point_fore_subnational_err_MLFTS_F_EVR_ARIMA_clr, c(1, 3), mean)),
                                                        apply(apply(point_fore_subnational_err_MLFTS_F_EVR_ARIMA_clr, c(1, 3), mean), 2, median))

point_fore_subnational_err_F_K6_ARIMA_MLFTS_clr_mean  = rbind(apply(point_fore_subnational_err_MLFTS_F_K6_ARIMA_clr, c(1, 3), mean), 
                                                        colMeans(apply(point_fore_subnational_err_MLFTS_F_K6_ARIMA_clr, c(1, 3), mean)),
                                                        apply(apply(point_fore_subnational_err_MLFTS_F_K6_ARIMA_clr, c(1, 3), mean), 2, median))

# by horizon

horizon_point_fore_subnational_err_F_EVR_ARIMA_MLFTS_clr_mean = apply(point_fore_subnational_err_MLFTS_F_EVR_ARIMA_clr, c(2, 3), mean)
horizon_point_fore_subnational_err_F_K6_ARIMA_MLFTS_clr_mean = apply(point_fore_subnational_err_MLFTS_F_K6_ARIMA_clr, c(2, 3), mean)

## Male

# by prefecture

point_fore_subnational_err_M_EVR_ARIMA_MLFTS_clr_mean = rbind(apply(point_fore_subnational_err_MLFTS_M_EVR_ARIMA_clr, c(1, 3), mean), 
                                                        colMeans(apply(point_fore_subnational_err_MLFTS_M_EVR_ARIMA_clr, c(1, 3), mean)),
                                                        apply(apply(point_fore_subnational_err_MLFTS_M_EVR_ARIMA_clr, c(1, 3), mean), 2, median))

point_fore_subnational_err_M_K6_ARIMA_MLFTS_clr_mean  = rbind(apply(point_fore_subnational_err_MLFTS_M_K6_ARIMA_clr, c(1, 3), mean), 
                                                        colMeans(apply(point_fore_subnational_err_MLFTS_M_K6_ARIMA_clr, c(1, 3), mean)),
                                                        apply(apply(point_fore_subnational_err_MLFTS_M_K6_ARIMA_clr, c(1, 3), mean), 2, median))

rownames(point_fore_subnational_err_F_EVR_ARIMA_MLFTS_clr_mean) = rownames(point_fore_subnational_err_F_K6_ARIMA_MLFTS_clr_mean) = 
rownames(point_fore_subnational_err_M_EVR_ARIMA_MLFTS_clr_mean) = rownames(point_fore_subnational_err_M_K6_ARIMA_MLFTS_clr_mean) = c(state, "Mean", "Median")

# by horizon

horizon_point_fore_subnational_err_M_EVR_ARIMA_MLFTS_clr_mean = apply(point_fore_subnational_err_MLFTS_M_EVR_ARIMA_clr, c(2, 3), mean)
horizon_point_fore_subnational_err_M_K6_ARIMA_MLFTS_clr_mean = apply(point_fore_subnational_err_MLFTS_M_K6_ARIMA_clr, c(2, 3), mean)

