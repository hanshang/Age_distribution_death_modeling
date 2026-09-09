#########################################################
# Multivariate functional time-series forecasting method
#########################################################

# load R packages

source("load_packages.R")
source("auxiliary_point.R")

#######
## ETS
#######

point_fore_subnational_err_F_EVR_ETS_MFTS_clr   = point_fore_subnational_err_M_EVR_ETS_MFTS_clr =
point_fore_subnational_err_F_K6_ETS_MFTS_clr    = point_fore_subnational_err_M_K6_ETS_MFTS_clr = array(NA, dim = c(n_state, 20, 4),
                                dimnames = list(state, 1:20, c("KLD", "JSD (geo)", "Wasserstein L1", "Wasserstein L2")))
for(ij in 1:n_state)
{
    female_data = replace(female_prefecture_dx[[ij]], which(female_prefecture_dx[[ij]] == 0), 10^-5)
    male_data   = replace(male_prefecture_dx[[ij]],   which(male_prefecture_dx[[ij]] == 0),   10^-5)
    for(iw in 1:20)
    {
        ## EVR
        
        clr_MFTS_EVR = point_fore_national_cdf_MFTS(fdata_F = female_data, fdata_M = male_data, 
                                                    fore_method = "CLR", horizon = iw, 
                                                    way_ncomp = "EVR", uni_fore_method = "ets")
        point_fore_subnational_err_F_EVR_ETS_MFTS_clr[ij,iw,] = clr_MFTS_EVR$err_F
        point_fore_subnational_err_M_EVR_ETS_MFTS_clr[ij,iw,] = clr_MFTS_EVR$err_M
        
        ## K = 6
        
        clr_MFTS_K6 = point_fore_national_cdf_MFTS(fdata_F = female_data, fdata_M = male_data, 
                                                   fore_method = "CLR", horizon = iw, 
                                                   way_ncomp = "provide", uni_fore_method = "ets")
        point_fore_subnational_err_F_K6_ETS_MFTS_clr[ij,iw,] = clr_MFTS_K6$err_F
        point_fore_subnational_err_M_K6_ETS_MFTS_clr[ij,iw,] = clr_MFTS_K6$err_M
        rm(iw); rm(clr_MFTS_EVR); rm(clr_MFTS_K6)
    }
    print(ij); rm(ij); rm(female_data); rm(male_data)
}

## female

# by prefecture

point_fore_subnational_err_F_EVR_ETS_MFTS_clr_mean = round(rbind(apply(point_fore_subnational_err_F_EVR_ETS_MFTS_clr, c(1, 3), mean),
                                                             colMeans(apply(point_fore_subnational_err_F_EVR_ETS_MFTS_clr, c(1, 3), mean)),
                                                             apply(apply(point_fore_subnational_err_F_EVR_ETS_MFTS_clr, c(1, 3), mean), 2, median)), 4)

point_fore_subnational_err_F_K6_ETS_MFTS_clr_mean = round(rbind(apply(point_fore_subnational_err_F_K6_ETS_MFTS_clr, c(1, 3), mean),
                                                            colMeans(apply(point_fore_subnational_err_F_K6_ETS_MFTS_clr, c(1, 3), mean)),
                                                            apply(apply(point_fore_subnational_err_F_K6_ETS_MFTS_clr, c(1, 3), mean), 2, median)), 4)
rownames(point_fore_subnational_err_F_EVR_ETS_MFTS_clr_mean) = 
rownames(point_fore_subnational_err_F_K6_ETS_MFTS_clr_mean) = c(state, "Mean", "Median")

# by horizon

horizon_point_fore_subnational_err_F_EVR_ETS_MFTS_clr_mean = apply(point_fore_subnational_err_F_EVR_ETS_MFTS_clr, c(2, 3), mean)
horizon_point_fore_subnational_err_F_K6_ETS_MFTS_clr_mean = apply(point_fore_subnational_err_F_K6_ETS_MFTS_clr, c(2, 3), mean)

## male

# by prefecture

point_fore_subnational_err_M_EVR_ETS_MFTS_clr_mean = round(rbind(apply(point_fore_subnational_err_M_EVR_ETS_MFTS_clr, c(1, 3), mean),
                                                             colMeans(apply(point_fore_subnational_err_M_EVR_ETS_MFTS_clr, c(1, 3), mean)),
                                                             apply(apply(point_fore_subnational_err_M_EVR_ETS_MFTS_clr, c(1, 3), mean), 2, median)), 4)

point_fore_subnational_err_M_K6_ETS_MFTS_clr_mean = round(rbind(apply(point_fore_subnational_err_M_K6_ETS_MFTS_clr, c(1, 3), mean),
                                                            colMeans(apply(point_fore_subnational_err_M_K6_ETS_MFTS_clr, c(1, 3), mean)),
                                                            apply(apply(point_fore_subnational_err_M_K6_ETS_MFTS_clr, c(1, 3), mean), 2, median)), 4)
rownames(point_fore_subnational_err_M_EVR_ETS_MFTS_clr_mean) = 
rownames(point_fore_subnational_err_M_K6_ETS_MFTS_clr_mean) = c(state, "Mean", "Median")

# by horizon

horizon_point_fore_subnational_err_M_EVR_ETS_MFTS_clr_mean = apply(point_fore_subnational_err_M_EVR_ETS_MFTS_clr, c(2, 3), mean)
horizon_point_fore_subnational_err_M_K6_ETS_MFTS_clr_mean = apply(point_fore_subnational_err_M_K6_ETS_MFTS_clr, c(2, 3), mean)

########
# ARIMA
########

point_fore_subnational_err_F_EVR_ARIMA_MFTS_clr   = point_fore_subnational_err_M_EVR_ARIMA_MFTS_clr =
point_fore_subnational_err_F_K6_ARIMA_MFTS_clr    = point_fore_subnational_err_M_K6_ARIMA_MFTS_clr = array(NA, dim = c(n_state, 20, 4),
                                              dimnames = list(state, 1:20, c("KLD", "JSD (geo)", "Wasserstein L1", "Wasserstein L2")))
for(ij in 1:n_state)
{
    female_data = replace(female_prefecture_dx[[ij]], which(female_prefecture_dx[[ij]] == 0), 10^-5)
    male_data   = replace(male_prefecture_dx[[ij]],   which(male_prefecture_dx[[ij]] == 0),   10^-5)
    for(iw in 1:20)
    {
        ## EVR
        
        clr_MFTS_EVR = point_fore_national_cdf_MFTS(fdata_F = female_data, fdata_M = male_data, 
                                                    fore_method = "CLR", horizon = iw, 
                                                    way_ncomp = "EVR", uni_fore_method = "arima")
        point_fore_subnational_err_F_EVR_ARIMA_MFTS_clr[ij,iw,] = clr_MFTS_EVR$err_F
        point_fore_subnational_err_M_EVR_ARIMA_MFTS_clr[ij,iw,] = clr_MFTS_EVR$err_M
        
        ## K = 6
        
        clr_MFTS_K6 = point_fore_national_cdf_MFTS(fdata_F = female_data, fdata_M = male_data, 
                                                   fore_method = "CLR", horizon = iw, 
                                                   way_ncomp = "provide", uni_fore_method = "arima")
        point_fore_subnational_err_F_K6_ARIMA_MFTS_clr[ij,iw,] = clr_MFTS_K6$err_F
        point_fore_subnational_err_M_K6_ARIMA_MFTS_clr[ij,iw,] = clr_MFTS_K6$err_M
        rm(iw)
    }
    print(ij); rm(ij); rm(female_data); rm(male_data)
}

## female

# by prefecture

point_fore_subnational_err_F_EVR_ARIMA_MFTS_clr_mean = round(rbind(apply(point_fore_subnational_err_F_EVR_ARIMA_MFTS_clr, c(1, 3), mean),
                                                                 colMeans(apply(point_fore_subnational_err_F_EVR_ARIMA_MFTS_clr, c(1, 3), mean)),
                                                                 apply(apply(point_fore_subnational_err_F_EVR_ARIMA_MFTS_clr, c(1, 3), mean), 2, median)), 4)

point_fore_subnational_err_F_K6_ARIMA_MFTS_clr_mean = round(rbind(apply(point_fore_subnational_err_F_K6_ARIMA_MFTS_clr, c(1, 3), mean),
                                                                colMeans(apply(point_fore_subnational_err_F_K6_ARIMA_MFTS_clr, c(1, 3), mean)),
                                                                apply(apply(point_fore_subnational_err_F_K6_ARIMA_MFTS_clr, c(1, 3), mean), 2, median)), 4)
rownames(point_fore_subnational_err_F_EVR_ARIMA_MFTS_clr_mean) = 
rownames(point_fore_subnational_err_F_K6_ARIMA_MFTS_clr_mean) = c(state, "Mean", "Median")

# by horizon

horizon_point_fore_subnational_err_F_EVR_ARIMA_MFTS_clr_mean = apply(point_fore_subnational_err_F_EVR_ARIMA_MFTS_clr, c(2, 3), mean)
horizon_point_fore_subnational_err_F_K6_ARIMA_MFTS_clr_mean = apply(point_fore_subnational_err_F_K6_ARIMA_MFTS_clr, c(2, 3), mean)

## male

# by prefecture

point_fore_subnational_err_M_EVR_ARIMA_MFTS_clr_mean = round(rbind(apply(point_fore_subnational_err_M_EVR_ARIMA_MFTS_clr, c(1, 3), mean),
                                                                 colMeans(apply(point_fore_subnational_err_M_EVR_ARIMA_MFTS_clr, c(1, 3), mean)),
                                                                 apply(apply(point_fore_subnational_err_M_EVR_ARIMA_MFTS_clr, c(1, 3), mean), 2, median)), 4)

point_fore_subnational_err_M_K6_ARIMA_MFTS_clr_mean = round(rbind(apply(point_fore_subnational_err_M_K6_ARIMA_MFTS_clr, c(1, 3), mean),
                                                                colMeans(apply(point_fore_subnational_err_M_K6_ARIMA_MFTS_clr, c(1, 3), mean)),
                                                                apply(apply(point_fore_subnational_err_M_K6_ARIMA_MFTS_clr, c(1, 3), mean), 2, median)), 4)
rownames(point_fore_subnational_err_M_EVR_ARIMA_MFTS_clr_mean) = 
rownames(point_fore_subnational_err_M_K6_ARIMA_MFTS_clr_mean) = c(state, "Mean", "Median")

# by horizon

horizon_point_fore_subnational_err_M_EVR_ARIMA_MFTS_clr_mean = apply(point_fore_subnational_err_M_EVR_ARIMA_MFTS_clr, c(2, 3), mean)
horizon_point_fore_subnational_err_M_K6_ARIMA_MFTS_clr_mean = apply(point_fore_subnational_err_M_K6_ARIMA_MFTS_clr, c(2, 3), mean)

