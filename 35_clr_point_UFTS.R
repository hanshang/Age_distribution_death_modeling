#######################################################
# Univariate functional time-series forecasting method
#######################################################

# load R packages

setwd("~/Library/CloudStorage/Dropbox/Todos/FANOVA_FFM_CDF/code")
source("load_packages.R")
source("auxiliary_point.R")

###########################
# log-ratio approach (clr)
###########################

# fdata: n by p data matrix
# ncomp_selection: method for selecting the number of retained components
# fh: forecast horizon
# fore_method: forecasting method

clr_fun <- function(fdata, ncomp_selection, fh, fore_method)
{
    n_age = ncol(fdata)
    n_year = nrow(fdata)
    h_x_t = CLR(fdata)$LR
    
    SVD_decomp = svd(h_x_t)
    if(ncomp_selection == "EVR")
    {
        ncomp = select_K(tau = 0.001, eigenvalue = SVD_decomp$d^2)
    }
    else if(ncomp_selection == "fixed")
    {
        ncomp = 6
    }
    else
    {
        warning("The number of retained component must be chosen by EVR or fixed at 6.")
    }
    basis = SVD_decomp$v[,1:ncomp]
    score = t(basis) %*% t(h_x_t)
    recon = basis %*% score
    resi = t(h_x_t) - recon
    
    # reconstruction (model in-sample fitting)
    
    recon = invCLR(t(recon))
    
    # forecasts of principal component scores
    
    score_fore = matrix(NA, ncomp, 1)
    for(ik in 1:ncomp)
    {
        if(fore_method == "RWF_no_drift")
        {
            score_fore[ik,] = rwf(as.numeric(score[ik,]), h = fh, drift = FALSE)$mean[fh]
        }
        else if(fore_method == "RWF_drift")
        {
            score_fore[ik,] = rwf(as.numeric(score[ik,]), h = fh, drift = TRUE)$mean[fh]
        }
        else if(fore_method == "ETS")
        {
            score_fore[ik,] = forecast(ets(as.numeric(score[ik,])), h = fh)$mean[fh]
        }
        else if(fore_method == "ARIMA")
        {
            score_fore[ik,] = forecast(auto.arima(as.numeric(score[ik,])), h = fh)$mean[fh]
        }
        else
        {
            warning("Univariate time series forecasting method is not on the list.")
        }
    }
    
    # obtain forecasts in real-valued space
    
    fore_val = basis %*% score_fore
    fore_count = invCLR(t(fore_val)) * 10^5
    return(list(ncomp = ncomp, fore_count = fore_count))
}

#######
## ETS
#######

point_fore_subnational_err_F_EVR_ETS_clr   = point_fore_subnational_err_M_EVR_ETS_clr =
point_fore_subnational_err_F_K6_ETS_clr    = point_fore_subnational_err_M_K6_ETS_clr = array(NA, dim = c(n_state, 20, 4),
                                        dimnames = list(state, 1:20, c("KLD", "JSD (geo)", "Wasserstein L1", "Wasserstein L2")))
for(ij in 1:n_state)
{
    female_data = replace(female_prefecture_dx[[ij]], which(female_prefecture_dx[[ij]] == 0), 10^-5)
    male_data   = replace(male_prefecture_dx[[ij]],   which(male_prefecture_dx[[ij]] == 0),   10^-5)
    for(iw in 1:20)
    {
        ## EVR
        
        point_fore_subnational_err_F_EVR_ETS_clr[ij,iw,] = point_fore_national_cdf(fdata = female_data, CLR_ncomp_selection = "EVR",
                                                                                   horizon = iw, fore_method = "CLR", uni_fore_method = "ETS")$err
        point_fore_subnational_err_M_EVR_ETS_clr[ij,iw,] = point_fore_national_cdf(fdata = male_data, CLR_ncomp_selection = "EVR",
                                                                                   horizon = iw, fore_method = "CLR", uni_fore_method = "ETS")$err
        ## K = 6
        
        point_fore_subnational_err_F_K6_ETS_clr[ij,iw,] = point_fore_national_cdf(fdata = female_data, CLR_ncomp_selection = "fixed",
                                                                                  horizon = iw, fore_method = "CLR", uni_fore_method = "ETS")$err
        point_fore_subnational_err_M_K6_ETS_clr[ij,iw,] = point_fore_national_cdf(fdata = male_data, CLR_ncomp_selection = "fixed",
                                                                                  horizon = iw, fore_method = "CLR", uni_fore_method = "ETS")$err
        rm(iw)
    }
    print(ij); rm(ij); rm(female_data); rm(male_data)
}

## female

# by prefecture

point_fore_subnational_err_F_EVR_ETS_clr_mean = round(rbind(apply(point_fore_subnational_err_F_EVR_ETS_clr, c(1, 3), mean),
                                                            colMeans(apply(point_fore_subnational_err_F_EVR_ETS_clr, c(1, 3), mean)),
                                                            apply(apply(point_fore_subnational_err_F_EVR_ETS_clr, c(1, 3), mean), 2, median)), 4)

point_fore_subnational_err_F_K6_ETS_clr_mean = round(rbind(apply(point_fore_subnational_err_F_K6_ETS_clr, c(1, 3), mean),
                                                           colMeans(apply(point_fore_subnational_err_F_K6_ETS_clr, c(1, 3), mean)),
                                                           apply(apply(point_fore_subnational_err_F_K6_ETS_clr, c(1, 3), mean), 2, median)), 4)

rownames(point_fore_subnational_err_F_EVR_ETS_clr_mean) = rownames(point_fore_subnational_err_F_K6_ETS_clr_mean) = c(state, "Mean", "Median")

# by horizon

horizon_point_fore_subnational_err_F_EVR_ETS_clr_mean = apply(point_fore_subnational_err_F_EVR_ETS_clr, c(2, 3), mean)
horizon_point_fore_subnational_err_F_K6_ETS_clr_mean = apply(point_fore_subnational_err_F_K6_ETS_clr, c(2, 3), mean)

## male

# by prefecture

point_fore_subnational_err_M_EVR_ETS_clr_mean = round(rbind(apply(point_fore_subnational_err_M_EVR_ETS_clr, c(1, 3), mean),
                                                            colMeans(apply(point_fore_subnational_err_M_EVR_ETS_clr, c(1, 3), mean)),
                                                            apply(apply(point_fore_subnational_err_M_EVR_ETS_clr, c(1, 3), mean), 2, median)), 4)

point_fore_subnational_err_M_K6_ETS_clr_mean = round(rbind(apply(point_fore_subnational_err_M_K6_ETS_clr, c(1, 3), mean),
                                                           colMeans(apply(point_fore_subnational_err_M_K6_ETS_clr, c(1, 3), mean)),
                                                           apply(apply(point_fore_subnational_err_M_K6_ETS_clr, c(1, 3), mean), 2, median)), 4)

rownames(point_fore_subnational_err_M_EVR_ETS_clr_mean) = rownames(point_fore_subnational_err_M_K6_ETS_clr_mean) = c(state, "Mean", "Median")

# by horizon

horizon_point_fore_subnational_err_M_EVR_ETS_clr_mean = apply(point_fore_subnational_err_M_EVR_ETS_clr, c(2, 3), mean)
horizon_point_fore_subnational_err_M_K6_ETS_clr_mean = apply(point_fore_subnational_err_M_K6_ETS_clr, c(2, 3), mean)

########
# ARIMA
########

point_fore_subnational_err_F_EVR_ARIMA_clr   = point_fore_subnational_err_M_EVR_ARIMA_clr =
point_fore_subnational_err_F_K6_ARIMA_clr    = point_fore_subnational_err_M_K6_ARIMA_clr = array(NA, dim = c(n_state, 20, 4),
                                                  dimnames = list(state, 1:20, c("KLD", "JSD (geo)", "Wasserstein L1", "Wasserstein L2")))
for(ij in 1:n_state)
{
    female_data = replace(female_prefecture_dx[[ij]], which(female_prefecture_dx[[ij]] == 0), 10^-5)
    male_data   = replace(male_prefecture_dx[[ij]],   which(male_prefecture_dx[[ij]] == 0),   10^-5)
    for(iw in 1:20)
    {
        ## EVR
        
        point_fore_subnational_err_F_EVR_ARIMA_clr[ij,iw,] = point_fore_national_cdf(fdata = female_data, CLR_ncomp_selection = "EVR",
                                                                                   horizon = iw, fore_method = "CLR", uni_fore_method = "ARIMA")$err
        point_fore_subnational_err_M_EVR_ARIMA_clr[ij,iw,] = point_fore_national_cdf(fdata = male_data, CLR_ncomp_selection = "EVR",
                                                                                   horizon = iw, fore_method = "CLR", uni_fore_method = "ARIMA")$err
        ## K = 6
        
        point_fore_subnational_err_F_K6_ARIMA_clr[ij,iw,] = point_fore_national_cdf(fdata = female_data, CLR_ncomp_selection = "fixed",
                                                                                  horizon = iw, fore_method = "CLR", uni_fore_method = "ARIMA")$err
        point_fore_subnational_err_M_K6_ARIMA_clr[ij,iw,] = point_fore_national_cdf(fdata = male_data, CLR_ncomp_selection = "fixed",
                                                                                  horizon = iw, fore_method = "CLR", uni_fore_method = "ARIMA")$err
        rm(iw)
    }
    print(ij); rm(ij); rm(female_data); rm(male_data)
}

## female

# by prefecture

point_fore_subnational_err_F_EVR_ARIMA_clr_mean = round(rbind(apply(point_fore_subnational_err_F_EVR_ARIMA_clr, c(1, 3), mean),
                                                            colMeans(apply(point_fore_subnational_err_F_EVR_ARIMA_clr, c(1, 3), mean)),
                                                            apply(apply(point_fore_subnational_err_F_EVR_ARIMA_clr, c(1, 3), mean), 2, median)), 4)

point_fore_subnational_err_F_K6_ARIMA_clr_mean = round(rbind(apply(point_fore_subnational_err_F_K6_ARIMA_clr, c(1, 3), mean),
                                                           colMeans(apply(point_fore_subnational_err_F_K6_ARIMA_clr, c(1, 3), mean)),
                                                           apply(apply(point_fore_subnational_err_F_K6_ARIMA_clr, c(1, 3), mean), 2, median)), 4)

rownames(point_fore_subnational_err_F_EVR_ARIMA_clr_mean) = rownames(point_fore_subnational_err_F_K6_ARIMA_clr_mean) = c(state, "Mean", "Median")

# by horizon

horizon_point_fore_subnational_err_F_EVR_ARIMA_clr_mean = apply(point_fore_subnational_err_F_EVR_ARIMA_clr, c(2, 3), mean)
horizon_point_fore_subnational_err_F_K6_ARIMA_clr_mean = apply(point_fore_subnational_err_F_K6_ARIMA_clr, c(2, 3), mean)

## male

# by prefecture

point_fore_subnational_err_M_EVR_ARIMA_clr_mean = round(rbind(apply(point_fore_subnational_err_M_EVR_ARIMA_clr, c(1, 3), mean),
                                                            colMeans(apply(point_fore_subnational_err_M_EVR_ARIMA_clr, c(1, 3), mean)),
                                                            apply(apply(point_fore_subnational_err_M_EVR_ARIMA_clr, c(1, 3), mean), 2, median)), 4)

point_fore_subnational_err_M_K6_ARIMA_clr_mean = round(rbind(apply(point_fore_subnational_err_M_K6_ARIMA_clr, c(1, 3), mean),
                                                           colMeans(apply(point_fore_subnational_err_M_K6_ARIMA_clr, c(1, 3), mean)),
                                                           apply(apply(point_fore_subnational_err_M_K6_ARIMA_clr, c(1, 3), mean), 2, median)), 4)

rownames(point_fore_subnational_err_M_EVR_ARIMA_clr_mean) = rownames(point_fore_subnational_err_M_K6_ARIMA_clr_mean) = c(state, "Mean", "Median")

# by horizon

horizon_point_fore_subnational_err_M_EVR_ARIMA_clr_mean = apply(point_fore_subnational_err_M_EVR_ARIMA_clr, c(2, 3), mean)
horizon_point_fore_subnational_err_M_K6_ARIMA_clr_mean = apply(point_fore_subnational_err_M_K6_ARIMA_clr, c(2, 3), mean)

