setwd("/Users/hanlinshang/Library/CloudStorage/Dropbox/Todos/FANOVA_FFM_CDF/code")
source("load_packages.R")
source("auxiliary_point.R")

###########################################
# Univariate functional time series method
###########################################

# fdata: a data matrix of dimension (n by p)
# method_ncomp: way of selecting the number of components
# horizon: forecast horizon 1 to 20
# fore_method: forecasting method, ARIMA or ETS
# CLR_ncomp_selection: when the fore_method = "CLR", it requires a way for selecting number of components

point_fore_national_cdf <- function(fdata, method_ncomp, horizon, fore_method, uni_fore_method,
                                    CLR_ncomp_selection)
{
    n_year = nrow(fdata)
    fore_val = matrix(NA, ncol(fdata), (21 - horizon))
    if(fore_method == "CDF")
    {
        for(ij in 1:(21 - horizon))
        {
            fore_val[,ij] <- fore_national_cdf(data_set = fdata[1:(n_year - 21 + ij),], ncomp_method = method_ncomp,
                                               fh = horizon, fmethod = uni_fore_method)
            rm(ij)
        }
    }
    else if(fore_method == "CLR")
    {
        for(ij in 1:(21 - horizon))
        {
            fore_val[,ij] <- as.numeric(clr_fun(fdata = fdata[1:(n_year - 21 + ij),], ncomp_selection = CLR_ncomp_selection,
                                                fh = horizon, fore_method = uni_fore_method)$fore_count)
            rm(ij)
        }
    }
    else
    {
        warning("forecasting method must either be CDF or CLR.")
    }
    
    holdout_val_dum = t(matrix(fdata[(n_year - 20 + horizon):n_year,], length((n_year - 20 + horizon):n_year), ncol(fdata)))
    if(any(holdout_val_dum == 0))
    {
        holdout_val = replace(x = holdout_val_dum, list = which(holdout_val_dum == 0), values = 10^-5)
    }
    else
    {
        holdout_val = holdout_val_dum
    }
    rm(holdout_val_dum)
    
    # compute the KL divergence and JS divergence
    
    KL_div_val = JS_div_val = L1_dist = L2_dist = vector("numeric", (21 - horizon))
    for(ij in 1:(21 - horizon))
    {
        # symmetric KL dist
        
        KL_div_val[ij] = mean(KLdiv(cbind(fore_val[,ij], holdout_val[,ij]))[2:3])
        
        # Jensen-Shannon dist
        
        JS_div_val[ij] = sqrt(mean(KLdiv(cbind(fore_val[,ij], 
                                               apply(cbind(fore_val[,ij], holdout_val[,ij]), 1, geometric.mean)))[2:3]))
        
        # L1 Wasserstein (not report)
        
        L1_dist[ij] = wasserstein1d(a = fore_val[,ij], b = holdout_val[,ij], p = 1)
        
        # L2 Wasserstein (not report)
        
        L2_dist[ij] = wasserstein1d(a = fore_val[,ij], b = holdout_val[,ij], p = 2)      
    }
    err = c(mean(KL_div_val), mean(JS_div_val), sqrt(mean(L1_dist^2)), sqrt(mean(L2_dist^2)))
    return(list(err = err, forecast_pdf = fore_val, holdout_pdf = holdout_val))
}

############################################
# Univariate functional time series (ARIMA)
############################################

# point forecast error (KLD, JSD, W1, W2) for h = 1, 2,...,20

point_fore_subnational_err_F_EVR_ARIMA   = point_fore_subnational_err_M_EVR_ARIMA =
point_fore_subnational_err_F_K6_ARIMA    = point_fore_subnational_err_M_K6_ARIMA = array(NA, dim = c(n_state, 20, 4), dimnames = list(state, 1:20, c("KLD", "JSD", "W1", "W2")))
for(ij in 1:n_state)
{
    for(iw in 1:20)
    {
        ## EVR
        
        point_fore_subnational_err_F_EVR_ARIMA[ij,iw,] = point_fore_national_cdf(fdata = female_prefecture_dx[[ij]], method_ncomp = "EVR",
                                                                                 horizon = iw, fore_method = "CDF", uni_fore_method = "arima")$err
        point_fore_subnational_err_M_EVR_ARIMA[ij,iw,] = point_fore_national_cdf(fdata = male_prefecture_dx[[ij]], method_ncomp = "EVR",
                                                                                 horizon = iw, fore_method = "CDF", uni_fore_method = "arima")$err
        ## K = 6
        
        point_fore_subnational_err_F_K6_ARIMA[ij,iw,] = point_fore_national_cdf(fdata = female_prefecture_dx[[ij]], method_ncomp = "provide",
                                                                                horizon = iw, fore_method = "CDF", uni_fore_method = "arima")$err
        point_fore_subnational_err_M_K6_ARIMA[ij,iw,] = point_fore_national_cdf(fdata = male_prefecture_dx[[ij]], method_ncomp = "provide",
                                                                                horizon = iw, fore_method = "CDF", uni_fore_method = "arima")$err
        rm(iw)
    }
    print(ij); rm(ij)
}

## female

# average across horizons

point_fore_subnational_err_F_EVR_ARIMA_mean = round(rbind(apply(point_fore_subnational_err_F_EVR_ARIMA, c(1, 3), mean),
                                                        colMeans(apply(point_fore_subnational_err_F_EVR_ARIMA, c(1, 3), mean)),
                                                        apply(apply(point_fore_subnational_err_F_EVR_ARIMA, c(1, 3), mean),2,median)), 4)

point_fore_subnational_err_F_K6_ARIMA_mean = round(rbind(apply(point_fore_subnational_err_F_K6_ARIMA, c(1, 3), mean),
                                                       colMeans(apply(point_fore_subnational_err_F_K6_ARIMA, c(1, 3), mean)),
                                                       apply(apply(point_fore_subnational_err_F_K6_ARIMA, c(1, 3), mean), 2, median)), 4)

rownames(point_fore_subnational_err_F_EVR_ARIMA_mean) = rownames(point_fore_subnational_err_F_K6_ARIMA_mean) = c(state, "Mean", "Median")

# average across prefectures

horizon_point_fore_subnational_err_F_EVR_ARIMA_mean = rbind(apply(point_fore_subnational_err_F_EVR_ARIMA, c(2, 3), mean),
                                                          colMeans(apply(point_fore_subnational_err_F_EVR_ARIMA, c(2, 3), mean)),
                                                          apply(apply(point_fore_subnational_err_F_EVR_ARIMA, c(2, 3), mean), 2, median))

horizon_point_fore_subnational_err_F_K6_ARIMA_mean = rbind(apply(point_fore_subnational_err_F_K6_ARIMA, c(2, 3), mean),
                                                         colMeans(apply(point_fore_subnational_err_F_K6_ARIMA, c(2, 3), mean)),
                                                         apply(apply(point_fore_subnational_err_F_K6_ARIMA, c(2, 3), mean), 2, median))

rownames(horizon_point_fore_subnational_err_F_EVR_ARIMA_mean) = rownames(horizon_point_fore_subnational_err_F_K6_ARIMA_mean) = c(1:20, "Mean", "Median")

## male

# average across horizons

point_fore_subnational_err_M_EVR_ARIMA_mean = round(rbind(apply(point_fore_subnational_err_M_EVR_ARIMA, c(1, 3), mean),
                                                        colMeans(apply(point_fore_subnational_err_M_EVR_ARIMA, c(1, 3), mean)),
                                                        apply(apply(point_fore_subnational_err_M_EVR_ARIMA, c(1, 3), mean), 2, median)), 4)

point_fore_subnational_err_M_K6_ARIMA_mean = round(rbind(apply(point_fore_subnational_err_M_K6_ARIMA, c(1, 3), mean),
                                                       colMeans(apply(point_fore_subnational_err_M_K6_ARIMA, c(1, 3), mean)),
                                                       apply(apply(point_fore_subnational_err_M_K6_ARIMA, c(1, 3), mean), 2, median)), 4)

rownames(point_fore_subnational_err_M_EVR_ARIMA_mean) = rownames(point_fore_subnational_err_M_K6_ARIMA_mean) = c(state, "Mean", "Median")

# average across prefectures

horizon_point_fore_subnational_err_M_EVR_ARIMA_mean = rbind(apply(point_fore_subnational_err_M_EVR_ARIMA, c(2, 3), mean), 
                                                          colMeans(apply(point_fore_subnational_err_M_EVR_ARIMA, c(2, 3), mean)),
                                                          apply(apply(point_fore_subnational_err_M_EVR_ARIMA, c(2, 3), mean), 2, median))

horizon_point_fore_subnational_err_M_K6_ARIMA_mean = rbind(apply(point_fore_subnational_err_M_K6_ARIMA, c(2, 3), mean),
                                                         colMeans(apply(point_fore_subnational_err_M_K6_ARIMA, c(2, 3), mean)),
                                                         apply(apply(point_fore_subnational_err_M_K6_ARIMA, c(2, 3), mean), 2, median))

rownames(horizon_point_fore_subnational_err_M_EVR_ARIMA_mean) = rownames(horizon_point_fore_subnational_err_M_K6_ARIMA_mean) = c(1:20, "Mean", "Median")


##########################################
# Univariate functional time series (ETS)
##########################################

# point forecast error (KLD, JSD, W1, W2) for h = 1, 2,...,20

point_fore_subnational_err_F_EVR_ETS   = point_fore_subnational_err_M_EVR_ETS =
point_fore_subnational_err_F_K6_ETS    = point_fore_subnational_err_M_K6_ETS = array(NA, dim = c(47, 20, 4), dimnames = list(state, 1:20, c("KLD", "JSD", "W1", "W2")))
for(ij in 1:47)
{
    for(iw in 1:20)
    {
        ## EVR
        
        point_fore_subnational_err_F_EVR_ETS[ij,iw,] = point_fore_national_cdf(fdata = female_prefecture_dx[[ij]], method_ncomp = "EVR",
                                                                               horizon = iw, fore_method = "CDF", uni_fore_method = "ets")$err
        point_fore_subnational_err_M_EVR_ETS[ij,iw,] = point_fore_national_cdf(fdata = male_prefecture_dx[[ij]], method_ncomp = "EVR",
                                                                               horizon = iw, fore_method = "CDF", uni_fore_method = "ets")$err
        ## K = 6
        
        point_fore_subnational_err_F_K6_ETS[ij,iw,] = point_fore_national_cdf(fdata = female_prefecture_dx[[ij]], method_ncomp = "provide",
                                                                              horizon = iw, fore_method = "CDF", uni_fore_method = "ets")$err
        point_fore_subnational_err_M_K6_ETS[ij,iw,] = point_fore_national_cdf(fdata = male_prefecture_dx[[ij]], method_ncomp = "provide",
                                                                              horizon = iw, fore_method = "CDF", uni_fore_method = "ets")$err
        rm(iw)
    }
    print(ij); rm(ij)
}

#######
## ETS
#######

## female

# average across horizons

point_fore_subnational_err_F_EVR_ETS_mean = round(rbind(apply(point_fore_subnational_err_F_EVR_ETS, c(1, 3), mean),
                                                        colMeans(apply(point_fore_subnational_err_F_EVR_ETS, c(1, 3), mean)),
                                                        apply(apply(point_fore_subnational_err_F_EVR_ETS, c(1, 3), mean),2,median)), 4)

point_fore_subnational_err_F_K6_ETS_mean = round(rbind(apply(point_fore_subnational_err_F_K6_ETS, c(1, 3), mean),
                                                       colMeans(apply(point_fore_subnational_err_F_K6_ETS, c(1, 3), mean)),
                                                       apply(apply(point_fore_subnational_err_F_K6_ETS, c(1, 3), mean), 2, median)), 4)

rownames(point_fore_subnational_err_F_EVR_ETS_mean) = rownames(point_fore_subnational_err_F_K6_ETS_mean) = c(state, "Mean", "Median")

# average across prefectures

horizon_point_fore_subnational_err_F_EVR_ETS_mean = rbind(apply(point_fore_subnational_err_F_EVR_ETS, c(2, 3), mean),
                                                          colMeans(apply(point_fore_subnational_err_F_EVR_ETS, c(2, 3), mean)),
                                                          apply(apply(point_fore_subnational_err_F_EVR_ETS, c(2, 3), mean), 2, median))

horizon_point_fore_subnational_err_F_K6_ETS_mean = rbind(apply(point_fore_subnational_err_F_K6_ETS, c(2, 3), mean),
                                                         colMeans(apply(point_fore_subnational_err_F_K6_ETS, c(2, 3), mean)),
                                                         apply(apply(point_fore_subnational_err_F_K6_ETS, c(2, 3), mean), 2, median))

rownames(horizon_point_fore_subnational_err_F_EVR_ETS_mean) = rownames(horizon_point_fore_subnational_err_F_K6_ETS_mean) = c(1:20, "Mean", "Median")

## male

# average across horizons

point_fore_subnational_err_M_EVR_ETS_mean = round(rbind(apply(point_fore_subnational_err_M_EVR_ETS, c(1, 3), mean),
                                                        colMeans(apply(point_fore_subnational_err_M_EVR_ETS, c(1, 3), mean)),
                                                        apply(apply(point_fore_subnational_err_M_EVR_ETS, c(1, 3), mean), 2, median)), 4)

point_fore_subnational_err_M_K6_ETS_mean = round(rbind(apply(point_fore_subnational_err_M_K6_ETS, c(1, 3), mean),
                                                       colMeans(apply(point_fore_subnational_err_M_K6_ETS, c(1, 3), mean)),
                                                       apply(apply(point_fore_subnational_err_M_K6_ETS, c(1, 3), mean), 2, median)), 4)

rownames(point_fore_subnational_err_M_EVR_ETS_mean) = rownames(point_fore_subnational_err_M_K6_ETS_mean) = c(state, "Mean", "Median")

# average across prefectures

horizon_point_fore_subnational_err_M_EVR_ETS_mean = rbind(apply(point_fore_subnational_err_M_EVR_ETS, c(2, 3), mean), 
                                                          colMeans(apply(point_fore_subnational_err_M_EVR_ETS, c(2, 3), mean)),
                                                          apply(apply(point_fore_subnational_err_M_EVR_ETS, c(2, 3), mean), 2, median))

horizon_point_fore_subnational_err_M_K6_ETS_mean = rbind(apply(point_fore_subnational_err_M_K6_ETS, c(2, 3), mean),
                                                         colMeans(apply(point_fore_subnational_err_M_K6_ETS, c(2, 3), mean)),
                                                         apply(apply(point_fore_subnational_err_M_K6_ETS, c(2, 3), mean), 2, median))

rownames(horizon_point_fore_subnational_err_M_EVR_ETS_mean) = rownames(horizon_point_fore_subnational_err_M_K6_ETS_mean) = c(1:20, "Mean", "Median")

