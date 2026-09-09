##################
# load R packages
##################

source("load_packages.R")
source("auxiliary_point.R")

# from list to array

female_prefecture_dx_array = male_prefecture_dx_array = array(NA, dim = c(n_year, n_age, n_state), 
                                                dimnames = list(years, ages, state))
for(ij in 1:n_state)
{
    female_prefecture_dx_array[,,ij] = female_prefecture_dx[[ij]]
    male_prefecture_dx_array[,,ij] = male_prefecture_dx[[ij]]
    rm(ij)
}

# from array to long matrix

female_prefecture_dx_array_mat = male_prefecture_dx_array_mat = matrix(NA, n_year * n_state, n_age)
for(ij in 1:n_state)
{
    female_prefecture_dx_array_mat[((ij - 1) * n_year + 1):(ij * n_year),] = female_prefecture_dx_array[,,ij]
    male_prefecture_dx_array_mat[((ij - 1) * n_year + 1):(ij * n_year),] = male_prefecture_dx_array[,,ij]
    rm(ij)
}

#######
## ETS
#######

point_fore_subnational_err_FANOVA_FFM_F_ETS_clr = 
point_fore_subnational_err_FANOVA_FFM_M_ETS_clr = array(NA, dim = c(n_state, 20, 4), dimnames = list(state, 1:20, c("KLD", "JSD (geo)","Wasserstein L1", "Wasserstein L2")))
for(iwk in 1:20)
{
    dum = point_fore_national_cdf_FANOVA_FFM(fdata_F = female_prefecture_dx_array, fdata_M = male_prefecture_dx_array, 
                                             fore_method = "CLR", horizon = iwk, uni_fore_method = "ets")
    point_fore_subnational_err_FANOVA_FFM_F_ETS_clr[,iwk,] = t(dum$err_F)
    point_fore_subnational_err_FANOVA_FFM_M_ETS_clr[,iwk,] = t(dum$err_M)
    print(iwk); rm(iwk)
}

## female

# average across horizons (by prefecture)

point_fore_subnational_err_FANOVA_FFM_F_ETS_clr_mean = round(rbind(apply(point_fore_subnational_err_FANOVA_FFM_F_ETS_clr, c(1, 3), mean),
                                                               colMeans(apply(point_fore_subnational_err_FANOVA_FFM_F_ETS_clr, c(1, 3), mean)),
                                                               apply(apply(point_fore_subnational_err_FANOVA_FFM_F_ETS_clr, c(1, 3), mean), 2, median)), 4)
rownames(point_fore_subnational_err_FANOVA_FFM_F_ETS_clr_mean) = c(state, "Mean", "Median")  

# average across prefectures (by horizon)

horizon_point_fore_subnational_err_FANOVA_FFM_F_ETS_clr_mean = rbind(apply(point_fore_subnational_err_FANOVA_FFM_F_ETS_clr, c(2, 3), mean),
                                                                 colMeans(apply(point_fore_subnational_err_FANOVA_FFM_F_ETS_clr, c(2, 3), mean)),
                                                                 apply(apply(point_fore_subnational_err_FANOVA_FFM_F_ETS_clr, c(2, 3), mean), 2, median))
rownames(horizon_point_fore_subnational_err_FANOVA_FFM_F_ETS_clr_mean) = c(1:20, "Mean", "Median")

## male

# average across horizons (by prefecture)

point_fore_subnational_err_FANOVA_FFM_M_ETS_clr_mean = round(rbind(apply(point_fore_subnational_err_FANOVA_FFM_M_ETS_clr, c(1, 3), mean),
                                                               colMeans(apply(point_fore_subnational_err_FANOVA_FFM_M_ETS_clr, c(1, 3), mean)),
                                                               apply(apply(point_fore_subnational_err_FANOVA_FFM_M_ETS_clr, c(1, 3), mean), 2, median)), 4)
rownames(point_fore_subnational_err_FANOVA_FFM_M_ETS_clr_mean) = c(state, "Mean", "Median")

# average across prefectures (by horizon)

horizon_point_fore_subnational_err_FANOVA_FFM_M_ETS_clr_mean = rbind(apply(point_fore_subnational_err_FANOVA_FFM_M_ETS_clr, c(2, 3), mean),
                                                                 colMeans(apply(point_fore_subnational_err_FANOVA_FFM_M_ETS_clr, c(2, 3), mean)),
                                                                 apply(apply(point_fore_subnational_err_FANOVA_FFM_M_ETS_clr, c(2, 3), mean), 2, median))
rownames(horizon_point_fore_subnational_err_FANOVA_FFM_M_ETS_clr_mean) = c(1:20, "Mean", "Median")

########
# ARIMA
########

point_fore_subnational_err_FANOVA_FFM_F_ARIMA_clr = 
point_fore_subnational_err_FANOVA_FFM_M_ARIMA_clr = array(NA, dim = c(n_state, 20, 4), dimnames = list(state, 1:20, c("KLD", "JSD (geo)","Wasserstein L1", "Wasserstein L2")))
for(iwk in 1:20)
{
    dum = point_fore_national_cdf_FANOVA_FFM(fdata_F = female_prefecture_dx_array, fdata_M = male_prefecture_dx_array, 
                                             fore_method = "CLR", horizon = iwk, uni_fore_method = "arima")
    point_fore_subnational_err_FANOVA_FFM_F_ARIMA_clr[,iwk,] = t(dum$err_F)
    point_fore_subnational_err_FANOVA_FFM_M_ARIMA_clr[,iwk,] = t(dum$err_M)
    print(iwk); rm(iwk)
}

## female

# average across horizons (by prefecture)

point_fore_subnational_err_FANOVA_FFM_F_ARIMA_clr_mean = round(rbind(apply(point_fore_subnational_err_FANOVA_FFM_F_ARIMA_clr, c(1, 3), mean),
                                                                   colMeans(apply(point_fore_subnational_err_FANOVA_FFM_F_ARIMA_clr, c(1, 3), mean)),
                                                                   apply(apply(point_fore_subnational_err_FANOVA_FFM_F_ARIMA_clr, c(1, 3), mean), 2, median)), 4)
rownames(point_fore_subnational_err_FANOVA_FFM_F_ARIMA_clr_mean) = c(state, "Mean", "Median")  

# average across prefectures (by horizon)

horizon_point_fore_subnational_err_FANOVA_FFM_F_ARIMA_clr_mean = rbind(apply(point_fore_subnational_err_FANOVA_FFM_F_ARIMA_clr, c(2, 3), mean),
                                                                     colMeans(apply(point_fore_subnational_err_FANOVA_FFM_F_ARIMA_clr, c(2, 3), mean)),
                                                                     apply(apply(point_fore_subnational_err_FANOVA_FFM_F_ARIMA_clr, c(2, 3), mean), 2, median))
rownames(horizon_point_fore_subnational_err_FANOVA_FFM_F_ARIMA_clr_mean) = c(1:20, "Mean", "Median")

## male

# average across horizons (by prefecture)

point_fore_subnational_err_FANOVA_FFM_M_ARIMA_clr_mean = round(rbind(apply(point_fore_subnational_err_FANOVA_FFM_M_ARIMA_clr, c(1, 3), mean),
                                                                   colMeans(apply(point_fore_subnational_err_FANOVA_FFM_M_ARIMA_clr, c(1, 3), mean)),
                                                                   apply(apply(point_fore_subnational_err_FANOVA_FFM_M_ARIMA_clr, c(1, 3), mean), 2, median)), 4)
rownames(point_fore_subnational_err_FANOVA_FFM_M_ARIMA_clr_mean) = c(state, "Mean", "Median")

# average across prefectures (by horizon)

horizon_point_fore_subnational_err_FANOVA_FFM_M_ARIMA_clr_mean = rbind(apply(point_fore_subnational_err_FANOVA_FFM_M_ARIMA_clr, c(2, 3), mean),
                                                                     colMeans(apply(point_fore_subnational_err_FANOVA_FFM_M_ARIMA_clr, c(2, 3), mean)),
                                                                     apply(apply(point_fore_subnational_err_FANOVA_FFM_M_ARIMA_clr, c(2, 3), mean), 2, median))
rownames(horizon_point_fore_subnational_err_FANOVA_FFM_M_ARIMA_clr_mean) = c(1:20, "Mean", "Median")

