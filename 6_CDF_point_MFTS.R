######################################
# Multivariate functional time series
######################################

source("load_packages.R")
source("auxiliary_point.R")

## ARIMA

point_fore_subnational_err_MFTS_F_EVR_ARIMA   = point_fore_subnational_err_MFTS_M_EVR_ARIMA   =
point_fore_subnational_err_MFTS_F_K6_ARIMA    = point_fore_subnational_err_MFTS_M_K6_ARIMA    = array(NA, dim = c(n_state, 20, 4), dimnames = list(state, 1:20, c("KLD", "JSD (geo)", "Wasserstein L1", "Wasserstein L2")))
for(ij in 1:n_state)
{
    for(iw in 1:20)
    {
        ## EVR
        
        dum = point_fore_national_cdf_MFTS(fdata_F = female_prefecture_dx[[ij]],
                                           fdata_M = male_prefecture_dx[[ij]],
                                           fore_method = "CDF", horizon = iw, way_ncomp = "EVR", 
                                           uni_fore_method = "arima")
        point_fore_subnational_err_MFTS_F_EVR_ARIMA[ij,iw,] = dum$err_F
        point_fore_subnational_err_MFTS_M_EVR_ARIMA[ij,iw,] = dum$err_M
        rm(dum)
        
        ## K = 6
        
        dum = point_fore_national_cdf_MFTS(fdata_F = female_prefecture_dx[[ij]],
                                           fdata_M = male_prefecture_dx[[ij]],
                                           fore_method = "CDF", horizon = iw, way_ncomp = "provide", 
                                           uni_fore_method = "arima")
        point_fore_subnational_err_MFTS_F_K6_ARIMA[ij,iw,] = dum$err_F
        point_fore_subnational_err_MFTS_M_K6_ARIMA[ij,iw,] = dum$err_M
        rm(dum); rm(iw)
    }
    print(ij); rm(ij)
}

#########
## ARIMA
#########

## female

# average across horizons

point_fore_subnational_err_MFTS_F_EVR_ARIMA_mean = round(rbind(apply(point_fore_subnational_err_MFTS_F_EVR_ARIMA, c(1, 3), mean),
                                                             colMeans(apply(point_fore_subnational_err_MFTS_F_EVR_ARIMA, c(1, 3), mean)),
                                                             apply(apply(point_fore_subnational_err_MFTS_F_EVR_ARIMA, c(1, 3), mean), 2, median)), 4)

point_fore_subnational_err_MFTS_F_K6_ARIMA_mean = round(rbind(apply(point_fore_subnational_err_MFTS_F_K6_ARIMA, c(1, 3), mean),
                                                            colMeans(apply(point_fore_subnational_err_MFTS_F_K6_ARIMA, c(1, 3), mean)),
                                                            apply(apply(point_fore_subnational_err_MFTS_F_K6_ARIMA, c(1, 3), mean), 2, median)), 4)
rownames(point_fore_subnational_err_MFTS_F_EVR_ARIMA_mean) = rownames(point_fore_subnational_err_MFTS_F_K6_ARIMA_mean) = c(state, "Mean", "Median")  

# average across prefectures

horizon_point_fore_subnational_err_MFTS_F_EVR_ARIMA_mean = rbind(apply(point_fore_subnational_err_MFTS_F_EVR_ARIMA, c(2, 3), mean),
                                                               colMeans(apply(point_fore_subnational_err_MFTS_F_EVR_ARIMA, c(2, 3), mean)),
                                                               apply(apply(point_fore_subnational_err_MFTS_F_EVR_ARIMA, c(2, 3), mean), 2, median))

horizon_point_fore_subnational_err_MFTS_F_K6_ARIMA_mean = rbind(apply(point_fore_subnational_err_MFTS_F_K6_ARIMA, c(2, 3), mean),
                                                              colMeans(apply(point_fore_subnational_err_MFTS_F_K6_ARIMA, c(2, 3), mean)),
                                                              apply(apply(point_fore_subnational_err_MFTS_F_K6_ARIMA, c(2, 3), mean), 2, median))
rownames(horizon_point_fore_subnational_err_MFTS_F_EVR_ARIMA_mean) = rownames(horizon_point_fore_subnational_err_MFTS_F_K6_ARIMA_mean) = c(1:20, "Mean", "Median")

## male

# average across horizons

point_fore_subnational_err_MFTS_M_EVR_ARIMA_mean = round(rbind(apply(point_fore_subnational_err_MFTS_M_EVR_ARIMA, c(1, 3), mean),
                                                             colMeans(apply(point_fore_subnational_err_MFTS_M_EVR_ARIMA, c(1, 3), mean)),
                                                             apply(apply(point_fore_subnational_err_MFTS_M_EVR_ARIMA, c(1, 3), mean), 2, median)), 4)

point_fore_subnational_err_MFTS_M_K6_ARIMA_mean = round(rbind(apply(point_fore_subnational_err_MFTS_M_K6_ARIMA, c(1, 3), mean),
                                                            colMeans(apply(point_fore_subnational_err_MFTS_M_K6_ARIMA, c(1, 3), mean)),
                                                            apply(apply(point_fore_subnational_err_MFTS_M_K6_ARIMA, c(1, 3), mean), 2, median)), 4)
rownames(point_fore_subnational_err_MFTS_M_EVR_ARIMA_mean) = rownames(point_fore_subnational_err_MFTS_M_K6_ARIMA_mean) = c(state, "Mean", "Median")

# average across prefectures

horizon_point_fore_subnational_err_MFTS_M_EVR_ARIMA_mean = rbind(apply(point_fore_subnational_err_MFTS_M_EVR_ARIMA, c(2, 3), mean),
                                                               colMeans(apply(point_fore_subnational_err_MFTS_M_EVR_ARIMA, c(2, 3), mean)),
                                                               apply(apply(point_fore_subnational_err_MFTS_M_EVR_ARIMA, c(2, 3), mean), 2, median))

horizon_point_fore_subnational_err_MFTS_M_K6_ARIMA_mean = rbind(apply(point_fore_subnational_err_MFTS_M_K6_ARIMA, c(2, 3), mean),
                                                              colMeans(apply(point_fore_subnational_err_MFTS_M_K6_ARIMA, c(2, 3), mean)),
                                                              apply(apply(point_fore_subnational_err_MFTS_M_K6_ARIMA, c(2, 3), mean), 2, median))
rownames(horizon_point_fore_subnational_err_MFTS_M_EVR_ARIMA_mean) = rownames(horizon_point_fore_subnational_err_MFTS_M_K6_ARIMA_mean) = c(1:20, "Mean", "Median")


#######
## ETS
#######

point_fore_subnational_err_MFTS_F_EVR_ETS   = point_fore_subnational_err_MFTS_M_EVR_ETS   =
point_fore_subnational_err_MFTS_F_K6_ETS    = point_fore_subnational_err_MFTS_M_K6_ETS    = array(NA, dim = c(n_state, 20, 4), dimnames = list(state, 1:20, c("KLD", "JSD (geo)", "Wasserstein L1", "Wasserstein L2")))
for(ij in 1:47)
{
    for(iw in 1:20)
    {
        ## EVR
        
        dum = point_fore_national_cdf_MFTS(fdata_F = female_prefecture_dx[[ij]],
                                           fdata_M = male_prefecture_dx[[ij]],
                                           fore_method = "CDF", horizon = iw, way_ncomp = "EVR", 
                                           uni_fore_method = "ets")
        point_fore_subnational_err_MFTS_F_EVR_ETS[ij,iw,] = dum$err_F
        point_fore_subnational_err_MFTS_M_EVR_ETS[ij,iw,] = dum$err_M
        rm(dum)
        
        ## K = 6
        
        dum = point_fore_national_cdf_MFTS(fdata_F = female_prefecture_dx[[ij]],
                                           fdata_M = male_prefecture_dx[[ij]],
                                           fore_method = "CDF", horizon = iw, way_ncomp = "provide", 
                                           uni_fore_method = "ets")
        point_fore_subnational_err_MFTS_F_K6_ETS[ij,iw,] = dum$err_F
        point_fore_subnational_err_MFTS_M_K6_ETS[ij,iw,] = dum$err_M
        rm(dum); rm(iw)
    }
    print(ij); rm(ij)
}

#######
## ETS
#######

## female

# average across horizons

point_fore_subnational_err_MFTS_F_EVR_ETS_mean = round(rbind(apply(point_fore_subnational_err_MFTS_F_EVR_ETS, c(1, 3), mean),
                                                             colMeans(apply(point_fore_subnational_err_MFTS_F_EVR_ETS, c(1, 3), mean)),
                                                             apply(apply(point_fore_subnational_err_MFTS_F_EVR_ETS, c(1, 3), mean), 2, median)), 4)

point_fore_subnational_err_MFTS_F_K6_ETS_mean = round(rbind(apply(point_fore_subnational_err_MFTS_F_K6_ETS, c(1, 3), mean),
                                                            colMeans(apply(point_fore_subnational_err_MFTS_F_K6_ETS, c(1, 3), mean)),
                                                            apply(apply(point_fore_subnational_err_MFTS_F_K6_ETS, c(1, 3), mean), 2, median)), 4)
rownames(point_fore_subnational_err_MFTS_F_EVR_ETS_mean) = rownames(point_fore_subnational_err_MFTS_F_K6_ETS_mean) = c(state, "Mean", "Median")  

# average across prefectures

horizon_point_fore_subnational_err_MFTS_F_EVR_ETS_mean = rbind(apply(point_fore_subnational_err_MFTS_F_EVR_ETS, c(2, 3), mean),
                                                               colMeans(apply(point_fore_subnational_err_MFTS_F_EVR_ETS, c(2, 3), mean)),
                                                               apply(apply(point_fore_subnational_err_MFTS_F_EVR_ETS, c(2, 3), mean), 2, median))

horizon_point_fore_subnational_err_MFTS_F_K6_ETS_mean = rbind(apply(point_fore_subnational_err_MFTS_F_K6_ETS, c(2, 3), mean),
                                                              colMeans(apply(point_fore_subnational_err_MFTS_F_K6_ETS, c(2, 3), mean)),
                                                              apply(apply(point_fore_subnational_err_MFTS_F_K6_ETS, c(2, 3), mean), 2, median))
rownames(horizon_point_fore_subnational_err_MFTS_F_EVR_ETS_mean) = rownames(horizon_point_fore_subnational_err_MFTS_F_K6_ETS_mean) = c(1:20, "Mean", "Median")

## male

# average across horizons

point_fore_subnational_err_MFTS_M_EVR_ETS_mean = round(rbind(apply(point_fore_subnational_err_MFTS_M_EVR_ETS, c(1, 3), mean),
                                                             colMeans(apply(point_fore_subnational_err_MFTS_M_EVR_ETS, c(1, 3), mean)),
                                                             apply(apply(point_fore_subnational_err_MFTS_M_EVR_ETS, c(1, 3), mean), 2, median)), 4)

point_fore_subnational_err_MFTS_M_K6_ETS_mean = round(rbind(apply(point_fore_subnational_err_MFTS_M_K6_ETS, c(1, 3), mean),
                                                            colMeans(apply(point_fore_subnational_err_MFTS_M_K6_ETS, c(1, 3), mean)),
                                                            apply(apply(point_fore_subnational_err_MFTS_M_K6_ETS, c(1, 3), mean), 2, median)), 4)
rownames(point_fore_subnational_err_MFTS_M_EVR_ETS_mean) = rownames(point_fore_subnational_err_MFTS_M_K6_ETS_mean) = c(state, "Mean", "Median")

# average across prefectures

horizon_point_fore_subnational_err_MFTS_M_EVR_ETS_mean = rbind(apply(point_fore_subnational_err_MFTS_M_EVR_ETS, c(2, 3), mean),
                                                               colMeans(apply(point_fore_subnational_err_MFTS_M_EVR_ETS, c(2, 3), mean)),
                                                               apply(apply(point_fore_subnational_err_MFTS_M_EVR_ETS, c(2, 3), mean), 2, median))

horizon_point_fore_subnational_err_MFTS_M_K6_ETS_mean = rbind(apply(point_fore_subnational_err_MFTS_M_K6_ETS, c(2, 3), mean),
                                                              colMeans(apply(point_fore_subnational_err_MFTS_M_K6_ETS, c(2, 3), mean)),
                                                              apply(apply(point_fore_subnational_err_MFTS_M_K6_ETS, c(2, 3), mean), 2, median))
rownames(horizon_point_fore_subnational_err_MFTS_M_EVR_ETS_mean) = rownames(horizon_point_fore_subnational_err_MFTS_M_K6_ETS_mean) = c(1:20, "Mean", "Median")

