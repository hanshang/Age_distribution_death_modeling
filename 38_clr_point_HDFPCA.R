###########################################################
# High-dimensional functional principal component analysis
# transformation = "clr"
###########################################################

# load R packages

source("load_packages.R")
source("auxiliary_point.R")
source("hdfpca_fun.R")

###################################################################
# point forecast errors (47 prefecture x 16 forecast horizons x 2)
###################################################################

# ARIMA

point_fore_subnational_err_ARIMA_HDFPCA_F_clr = point_fore_subnational_err_HDFPCA_F_clr
point_fore_subnational_err_ARIMA_HDFPCA_M_clr = point_fore_subnational_err_HDFPCA_M_clr

point_fore_subnational_err_ARIMA_HDFPCA_F_clr = point_fore_subnational_err_ARIMA_HDFPCA_M_clr = array(NA, dim = c(n_state, 20, 4),
                              dimnames = list(state, 1:20, c("KLD", "JSD (geo)", "Wasserstein L1", "Wasserstein L2")))
for(ij in 1:n_state)
{
    for(iw in 1:20)
    {
        dum = hdfpca_fun(fdata_F = female_prefecture_dx[[ij]], 
                         fdata_M = male_prefecture_dx[[ij]], 
                         horizon = iw, first_order = 6, second_order = 2, 
                         transformation = "clr", forecasting_method = "arima")
        point_fore_subnational_err_ARIMA_HDFPCA_F_clr[ij,iw,] = dum$err_F
        point_fore_subnational_err_ARIMA_HDFPCA_M_clr[ij,iw,] = dum$err_M
        rm(dum); rm(iw)
    }
    print(ij); rm(ij)
}

# by prefecture

point_fore_subnational_err_ARIMA_HDFPCA_F_clr_mean = rbind(apply(point_fore_subnational_err_ARIMA_HDFPCA_F_clr, c(1, 3), mean),
                                                     colMeans(apply(point_fore_subnational_err_ARIMA_HDFPCA_F_clr, c(1, 3), mean)),
                                                     apply(apply(point_fore_subnational_err_ARIMA_HDFPCA_F_clr, c(1, 3), mean), 2, median))

point_fore_subnational_err_ARIMA_HDFPCA_M_clr_mean = rbind(apply(point_fore_subnational_err_ARIMA_HDFPCA_M_clr, c(1, 3), mean),
                                                     colMeans(apply(point_fore_subnational_err_ARIMA_HDFPCA_M_clr, c(1, 3), mean)),
                                                     apply(apply(point_fore_subnational_err_ARIMA_HDFPCA_M_clr, c(1, 3), mean), 2, median))

rownames(point_fore_subnational_err_ARIMA_HDFPCA_F_clr_mean) = 
rownames(point_fore_subnational_err_ARIMA_HDFPCA_M_clr_mean) = c(state, "Mean", "Median")

colnames(point_fore_subnational_err_ARIMA_HDFPCA_F_clr_mean) = 
colnames(point_fore_subnational_err_ARIMA_HDFPCA_M_clr_mean) = c("KLD", "JSD", "Wasserstein L1", "Wasserstein L2")

round(point_fore_subnational_err_ARIMA_HDFPCA_F_clr_mean[48,], 4) # 0.0201 0.0657 900.8919 1699.7571
round(point_fore_subnational_err_ARIMA_HDFPCA_M_clr_mean[48,], 4) # 0.0284 0.0766 900.8919 1545.3338

# by horizon

horizon_point_fore_subnational_err_ARIMA_HDFPCA_F_clr_mean = apply(point_fore_subnational_err_ARIMA_HDFPCA_F_clr, c(2, 3), mean)
horizon_point_fore_subnational_err_ARIMA_HDFPCA_M_clr_mean = apply(point_fore_subnational_err_ARIMA_HDFPCA_M_clr, c(2, 3), mean)

## ETS

point_fore_subnational_err_ETS_HDFPCA_F_clr = point_fore_subnational_err_ETS_HDFPCA_M_clr = array(NA, dim = c(n_state, 20, 4),
                            dimnames = list(state, 1:20, c("KLD", "JSD (geo)", "Wasserstein L1", "Wasserstein L2")))
for(ij in 1:n_state)
{
    for(iw in 1:20)
    {
        dum = hdfpca_fun(fdata_F = female_prefecture_dx[[ij]], 
                         fdata_M = male_prefecture_dx[[ij]], 
                         horizon = iw, first_order = 6, second_order = 2, 
                         transformation = "clr", forecasting_method = "ets")
        point_fore_subnational_err_ETS_HDFPCA_F_clr[ij,iw,] = dum$err_F
        point_fore_subnational_err_ETS_HDFPCA_M_clr[ij,iw,] = dum$err_M
        rm(dum); rm(iw)
    }
    print(ij); rm(ij)
}

# by prefecture

point_fore_subnational_err_ETS_HDFPCA_F_clr_mean = rbind(apply(point_fore_subnational_err_ETS_HDFPCA_F_clr, c(1, 3), mean),
                                                           colMeans(apply(point_fore_subnational_err_ETS_HDFPCA_F_clr, c(1, 3), mean)),
                                                           apply(apply(point_fore_subnational_err_ETS_HDFPCA_F_clr, c(1, 3), mean), 2, median))

point_fore_subnational_err_ETS_HDFPCA_M_clr_mean = rbind(apply(point_fore_subnational_err_ETS_HDFPCA_M_clr, c(1, 3), mean),
                                                           colMeans(apply(point_fore_subnational_err_ETS_HDFPCA_M_clr, c(1, 3), mean)),
                                                           apply(apply(point_fore_subnational_err_ETS_HDFPCA_M_clr, c(1, 3), mean), 2, median))

rownames(point_fore_subnational_err_ETS_HDFPCA_F_clr_mean) = 
rownames(point_fore_subnational_err_ETS_HDFPCA_M_clr_mean) = c(state, "Mean", "Median")

colnames(point_fore_subnational_err_ETS_HDFPCA_F_clr_mean) = 
colnames(point_fore_subnational_err_ETS_HDFPCA_M_clr_mean) = c("KLD", "JSD", "Wasserstein L1", "Wasserstein L2")

round(point_fore_subnational_err_ETS_HDFPCA_F_clr_mean[48,], 4) # 0.0161 0.0598 900.8919 1699.7574
round(point_fore_subnational_err_ETS_HDFPCA_M_clr_mean[48,], 4) # 0.0229 0.0566 900.8919 1545.3341

# by horizon

horizon_point_fore_subnational_err_ETS_HDFPCA_F_clr_mean = apply(point_fore_subnational_err_ETS_HDFPCA_F_clr, c(2, 3), mean)
horizon_point_fore_subnational_err_ETS_HDFPCA_M_clr_mean = apply(point_fore_subnational_err_ETS_HDFPCA_M_clr, c(2, 3), mean)

