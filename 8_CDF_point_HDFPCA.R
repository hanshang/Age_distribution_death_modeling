###########################################################
# High-Dimensional Functional Principal Component Analysis
# transformation = "CDF"
###########################################################

setwd("~/Dropbox/Todos/FANOVA_FFM_CDF/code")
source("load_packages.R")
source("hdfpca_fun.R")

# first_order, K = 6 
# second order, L = 2

# ETS

point_fore_subnational_err_HDFPCA_F_CDF_ETS = point_fore_subnational_err_HDFPCA_M_CDF_ETS = array(NA, dim = c(n_state, 20, 4), 
                                                                                          dimnames = list(state, 1:20, c("KLD", "JSD (geo)", "Wasserstein L1", "Wasserstein L2")))
for(ij in 1:n_state)
{
    for(iw in 1:20)
    {
        dum = hdfpca_fun(fdata_F = female_prefecture_dx[[ij]], 
                         fdata_M = male_prefecture_dx[[ij]], horizon = iw, 
                         first_order = 6, second_order = 2, transformation = "CDF",
                         forecasting_method = "ets")
        point_fore_subnational_err_HDFPCA_F_CDF_ETS[ij,iw,] = dum$err_F
        point_fore_subnational_err_HDFPCA_M_CDF_ETS[ij,iw,] = dum$err_M
        rm(dum); rm(iw)
    }
    print(ij); rm(ij)
}

###########
## summary
###########

## Female

# state-specific 

point_fore_subnational_err_HDFPCA_F_CDF_ETS_mean = rbind(apply(point_fore_subnational_err_HDFPCA_F_CDF_ETS, c(1,3), mean),
                                                     colMeans(apply(point_fore_subnational_err_HDFPCA_F_CDF_ETS, c(1,3), mean)),
                                                     apply(apply(point_fore_subnational_err_HDFPCA_F_CDF_ETS, c(1,3), mean), 2, median)) # 
rownames(point_fore_subnational_err_HDFPCA_F_CDF_ETS_mean) = c(state, "Mean", "Median")
colnames(point_fore_subnational_err_HDFPCA_F_CDF_ETS_mean) = c("KLD", "JSD (geo)", "Wasserstein L1", "Wasserstein L2")

# horizon-specific 

horizon_point_fore_subnational_err_HDFPCA_F_CDF_ETS_mean = rbind(apply(point_fore_subnational_err_HDFPCA_F_CDF_ETS, c(2, 3), mean), 
                                                             colMeans(apply(point_fore_subnational_err_HDFPCA_F_CDF_ETS, c(2, 3), mean)),
                                                             apply(apply(point_fore_subnational_err_HDFPCA_F_CDF_ETS, c(2, 3), mean), 2, median))
rownames(horizon_point_fore_subnational_err_HDFPCA_F_CDF_ETS_mean) = c(1:20, "Mean", "Median")

## Male

# state-specific

point_fore_subnational_err_HDFPCA_M_CDF_ETS_mean = rbind(apply(point_fore_subnational_err_HDFPCA_M_CDF_ETS, c(1,3), mean),
                                                     colMeans(apply(point_fore_subnational_err_HDFPCA_M_CDF_ETS, c(1,3), mean)),
                                                     apply(apply(point_fore_subnational_err_HDFPCA_M_CDF_ETS, c(1,3), mean), 2, median)) # 
rownames(point_fore_subnational_err_HDFPCA_M_CDF_ETS_mean) = c(state, "Mean", "Median")
colnames(point_fore_subnational_err_HDFPCA_M_CDF_ETS_mean) = c("KLD", "JSD (geo)", "Wasserstein L1", "Wasserstein L2")

# horizon-specific

horizon_point_fore_subnational_err_HDFPCA_M_CDF_ETS_mean = rbind(apply(point_fore_subnational_err_HDFPCA_M_CDF_ETS, c(2, 3), mean),
                                                             colMeans(apply(point_fore_subnational_err_HDFPCA_M_CDF_ETS, c(2, 3), mean)),
                                                             apply(apply(point_fore_subnational_err_HDFPCA_M_CDF_ETS, c(2, 3), mean), 2, median))

rownames(horizon_point_fore_subnational_err_HDFPCA_M_CDF_ETS_mean) = c(1:20, "Mean", "Median")


########
# ARIMA
########

point_fore_subnational_err_HDFPCA_F_CDF = point_fore_subnational_err_HDFPCA_M_CDF = array(NA, dim = c(n_state, 20, 4), 
                            dimnames = list(state, 1:20, c("KLD", "JSD (geo)", "Wasserstein L1", "Wasserstein L2")))
for(ij in 1:n_state)
{
    for(iw in 1:20)
    {
        dum = hdfpca_fun(fdata_F = female_prefecture_dx[[ij]], 
                         fdata_M = male_prefecture_dx[[ij]], horizon = iw, 
                         first_order = 6, second_order = 2, transformation = "CDF",
                         forecasting_method = "arima")
        point_fore_subnational_err_HDFPCA_F_CDF[ij,iw,] = dum$err_F
        point_fore_subnational_err_HDFPCA_M_CDF[ij,iw,] = dum$err_M
        rm(dum); rm(iw)
    }
    print(ij); rm(ij)
}

###########
## summary
###########

## Female

# state-specific 

point_fore_subnational_err_HDFPCA_F_CDF_mean = rbind(apply(point_fore_subnational_err_HDFPCA_F_CDF, c(1,3), mean),
                                                     colMeans(apply(point_fore_subnational_err_HDFPCA_F_CDF, c(1,3), mean)),
                                                     apply(apply(point_fore_subnational_err_HDFPCA_F_CDF, c(1,3), mean), 2, median)) # 
rownames(point_fore_subnational_err_HDFPCA_F_CDF_mean) = c(state, "Mean", "Median")
colnames(point_fore_subnational_err_HDFPCA_F_CDF_mean) = c("KLD", "JSD (geo)", "Wasserstein L1", "Wasserstein L2")

# horizon-specific 

horizon_point_fore_subnational_err_HDFPCA_F_CDF_mean = rbind(apply(point_fore_subnational_err_HDFPCA_F_CDF, c(2, 3), mean), 
                                                             colMeans(apply(point_fore_subnational_err_HDFPCA_F_CDF, c(2, 3), mean)),
                                                             apply(apply(point_fore_subnational_err_HDFPCA_F_CDF, c(2, 3), mean), 2, median))
rownames(horizon_point_fore_subnational_err_HDFPCA_F_CDF_mean) = c(1:20, "Mean", "Median")

## Male

# state-specific

point_fore_subnational_err_HDFPCA_M_CDF_mean = rbind(apply(point_fore_subnational_err_HDFPCA_M_CDF, c(1,3), mean),
                                                     colMeans(apply(point_fore_subnational_err_HDFPCA_M_CDF, c(1,3), mean)),
                                                     apply(apply(point_fore_subnational_err_HDFPCA_M_CDF, c(1,3), mean), 2, median)) # 
rownames(point_fore_subnational_err_HDFPCA_M_CDF_mean) = c(state, "Mean", "Median")
colnames(point_fore_subnational_err_HDFPCA_M_CDF_mean) = c("KLD", "JSD (geo)", "Wasserstein L1", "Wasserstein L2")

# horizon-specific

horizon_point_fore_subnational_err_HDFPCA_M_CDF_mean = rbind(apply(point_fore_subnational_err_HDFPCA_M_CDF, c(2, 3), mean),
                                                             colMeans(apply(point_fore_subnational_err_HDFPCA_M_CDF, c(2, 3), mean)),
                                                             apply(apply(point_fore_subnational_err_HDFPCA_M_CDF, c(2, 3), mean), 2, median))

rownames(horizon_point_fore_subnational_err_HDFPCA_M_CDF_mean) = c(1:20, "Mean", "Median")

