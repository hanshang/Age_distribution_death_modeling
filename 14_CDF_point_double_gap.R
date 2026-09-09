setwd("~/Dropbox/Todos/FANOVA_FFM_CDF/code")
load("read_data.RData")
source("load_packages.R")

# N_F_CDF: national female CDF
# subnational_region_F_diff_CDF: regional gap = subnational - national female CDF
# subnational_gender_diff_CDF: gender gap = subnational female - subnational male CDF
# fh: forecast horizon
# fmethod: forecasting method
# length_test_data: length of testing data

double_gap_fore_fun <- function(index, region_gap, gender_gap, N_F_CDF, fh, fmethod, length_test_data)
{
    # N_nrow is number of ages
    # N_ncol is number of years
    
    N_nrow = nrow(N_F_CDF)
    N_ncol = ncol(N_F_CDF)
    
    # ensure female national CDF >= 0
    if(any(N_F_CDF <= 0))
    {
        N_F_CDF = replace(N_F_CDF, which(N_F_CDF == 0), 10^-8)
    }
    
    Japan_female_pop_CDF_forecast = matrix(NA, N_nrow, (length_test_data + 1 - fh))
    for(ik in 1:(length_test_data + 1 - fh))
    {
        Japan_female_pop_CDF_forecast[,ik] = c(invlogit(forecast(ftsm(fts(ages[1:(N_nrow - 1)], logit(N_F_CDF[1:(N_nrow - 1), 1:(N_ncol - (length_test_data + 1) + ik)]))), h = fh, method = fmethod)$mean$y[,fh]), 1)
        rm(ik)
    }
    
    # female region gap between subnational and national data
    
    subnational_region_F_diff_CDF_forecast = matrix(NA, N_nrow, (length_test_data + 1 - fh))
    for(ij in 1:(length_test_data + 1 - fh))
    {
        subnational_region_F_diff_CDF_forecast[,ij] = FisherZInv(forecast(ftsm(fts(ages, FisherZ(region_gap[[index]][,1:(N_ncol - (length_test_data + 1) + ij)]))), h = fh, method = fmethod)$mean$y[,fh])
        rm(ij)
    }
    
    # subnational female forecasts = female region gap + national female forecasts
    
    subnational_female_pop_CDF_forecast = Japan_female_pop_CDF_forecast + subnational_region_F_diff_CDF_forecast
    
    # gender gap between subnational males and females
    
    gender_gap_fisherz_fts_forecast_transform = matrix(NA, N_nrow, (length_test_data + 1 - fh))
    for(ij in 1:(length_test_data + 1 - fh))
    {
        gender_gap_fisherz_fts_forecast_transform[,ij] = FisherZInv(forecast(ftsm(fts(ages, FisherZ(gender_gap[[index]][,1:(N_ncol - (length_test_data + 1) + ij)]))), h = fh, method = fmethod)$mean$y[,fh])
        rm(ij)
    }
    
    # subnational male forecasts
    
    subnational_male_pop_CDF_forecast = subnational_female_pop_CDF_forecast + gender_gap_fisherz_fts_forecast_transform
    
    # turn CDF to PDF
    
    subnational_female_pop_CDF_forecast <- pmin(pmax(subnational_female_pop_CDF_forecast, 10^-8), 1)
    subnational_male_pop_CDF_forecast   <- pmin(pmax(subnational_male_pop_CDF_forecast,   10^-8), 1)
    
    # from CDF to PDF
    
    subnational_female_pop_PDF_forecast = subnational_male_pop_PDF_forecast = matrix(NA, N_nrow, (length_test_data + 1 - fh))
    for(ij in 1:(length_test_data + 1 - fh))
    {
        subnational_female_pop_PDF_forecast[,ij] = c(subnational_female_pop_CDF_forecast[1,ij], diff(subnational_female_pop_CDF_forecast[,ij]))
        subnational_male_pop_PDF_forecast[,ij]   = c(subnational_male_pop_CDF_forecast[1,ij],   diff(subnational_male_pop_CDF_forecast[,ij]))
        rm(ij)
    }
    
    return(list(male_fore = pmax(subnational_male_pop_PDF_forecast, 0) * 10^5, 
                female_fore = pmax(subnational_female_pop_PDF_forecast, 0) * 10^5))
}

##########################################
# compute one- to 20-step-ahead forecasts
##########################################

# ARIMA

subnational_double_gap_fore_M_ARIMA = subnational_double_gap_fore_F_ARIMA = list()
for(iwk in 1:n_state)
{
    male_fore = female_fore = list()
    for(iwj in 1:20)
    {
        dum = double_gap_fore_fun(index = iwk, region_gap = subnational_region_F_diff_CDF, 
                                  gender_gap = subnational_gender_diff_CDF,
                                  N_F_CDF = Japan_female_pop_CDF, fh = iwj, fmethod = "arima",
                                  length_test_data = 20)
        male_fore[[iwj]] = dum$male_fore
        female_fore[[iwj]] = dum$female_fore
        rm(iwj); rm(dum)
    }
    subnational_double_gap_fore_M_ARIMA[[iwk]] = male_fore
    subnational_double_gap_fore_F_ARIMA[[iwk]] = female_fore
    print(iwk); rm(iwk); rm(male_fore); rm(female_fore)
}

# ETS

subnational_double_gap_fore_M_ETS = subnational_double_gap_fore_F_ETS = list()
for(iwk in 1:n_state)
{
    male_fore = female_fore = list()
    for(iwj in 1:20)
    {
        dum = double_gap_fore_fun(index = iwk, region_gap = subnational_region_F_diff_CDF, 
                                  gender_gap = subnational_gender_diff_CDF,
                                  N_F_CDF = Japan_female_pop_CDF, fh = iwj, fmethod = "ets",
                                  length_test_data = 20)
        male_fore[[iwj]] = dum$male_fore
        female_fore[[iwj]] = dum$female_fore
        rm(iwj); rm(dum)
    }
    subnational_double_gap_fore_M_ETS[[iwk]] = male_fore
    subnational_double_gap_fore_F_ETS[[iwk]] = female_fore
    print(iwk); rm(iwk); rm(male_fore); rm(female_fore)
}
  
##############################################################
# assess forecast accuracy between forecasts and holdout data
##############################################################

# index: prefecture index
# forecast_M: forecast normalized male life-table death counts
# forecast_F: forecast normalized female life-table death counts
# PDF_M_holdout: male holodut data
# PDF_F_holdout: female holdout data
# horizon: forecast horizon
# length_test_data: number of years in the testing data

double_gap_fore_fun_fore <- function(index, forecast_M, forecast_F, PDF_M_holdout, PDF_F_holdout, 
                                     horizon, length_test_data)
{
    n_col = nrow(PDF_M_holdout[[index]])
    n_row = ncol(PDF_M_holdout[[index]])
    
    # holdout data
    
    PDF_M = matrix(t(PDF_M_holdout[[index]][(n_col - length_test_data + horizon):n_col,]), n_row, (length_test_data + 1 - horizon))
    PDF_F = matrix(t(PDF_F_holdout[[index]][(n_col - length_test_data + horizon):n_col,]), n_row, (length_test_data + 1 - horizon))
    
    # KLD and JSD
    
    KL_div_val_M = JS_div_val_M = KL_div_val_F = JS_div_val_F = vector("numeric", (length_test_data + 1 - horizon))
    for(ij in 1:(length_test_data + 1 - horizon))
    {
        # symmetric KLD
        
        KL_div_val_M[ij] = mean(KLdiv(cbind(forecast_M[[index]][[horizon]][,ij], PDF_M[,ij]))[2:3])
        KL_div_val_F[ij] = mean(KLdiv(cbind(forecast_F[[index]][[horizon]][,ij], PDF_F[,ij]))[2:3])
        
        # Jensen-Shannon divergence (JSD)
        
        JS_div_val_M[ij] = mean(KLdiv(cbind(forecast_M[[index]][[horizon]][,ij], 
                                            apply(cbind(forecast_M[[index]][[horizon]][,ij], PDF_M[,ij]), 1, geometric.mean)))[2:3])
        JS_div_val_F[ij] = mean(KLdiv(cbind(forecast_F[[index]][[horizon]][,ij], 
                                            apply(cbind(forecast_F[[index]][[horizon]][,ij], PDF_F[,ij]), 1, geometric.mean)))[2:3])
    }
    # compute KLD and JSD
    
    err_M = c(mean(KL_div_val_M), mean(JS_div_val_M))
    err_F = c(mean(KL_div_val_F), mean(JS_div_val_F))
    return(list(err_M = err_M, err_F = err_F))
}

## ARIMA

subnational_double_gap_fore_male_ARIMA = subnational_double_gap_fore_female_ARIMA = array(NA, dim = c(n_state, 20, 2),
                                                                        dimnames = list(state, 1:20, c("KLD", "JSD")))
for(iwk in 1:n_state)
{
    for(iwj in 1:20)
    {
        dum = double_gap_fore_fun_fore(index = iwk, forecast_M = subnational_double_gap_fore_M_ARIMA, 
                                       forecast_F = subnational_double_gap_fore_F_ARIMA, 
                                       PDF_M_holdout = male_prefecture_dx, PDF_F_holdout = female_prefecture_dx, 
                                       horizon = iwj, length_test_data = 20)
        subnational_double_gap_fore_male_ARIMA[iwk,iwj,] = dum$err_M
        subnational_double_gap_fore_female_ARIMA[iwk,iwj,] = dum$err_F
        rm(iwj)
    }
    print(iwk); rm(iwk)
}

# by prefecture

subnational_double_gap_fore_male_ARIMA_prefecture_mean = apply(subnational_double_gap_fore_male_ARIMA, c(1, 3), mean)
subnational_double_gap_fore_female_ARIMA_prefecture_mean = apply(subnational_double_gap_fore_female_ARIMA, c(1, 3), mean)

# by forecast horizon

subnational_double_gap_fore_male_ARIMA_mean   = apply(subnational_double_gap_fore_male_ARIMA,   c(2, 3), mean)
subnational_double_gap_fore_female_ARIMA_mean = apply(subnational_double_gap_fore_female_ARIMA, c(2, 3), mean)

## ETS

subnational_double_gap_fore_male_ETS = subnational_double_gap_fore_female_ETS = array(NA, dim = c(n_state, 20, 2),
                                                                            dimnames = list(state, 1:20, c("KLD", "JSD")))
for(iwk in 1:n_state)
{
    for(iwj in 1:20)
    {
        dum = double_gap_fore_fun_fore(index = iwk, forecast_M = subnational_double_gap_fore_M_ETS, 
                                       forecast_F = subnational_double_gap_fore_F_ETS, 
                                       PDF_M_holdout = male_prefecture_dx, PDF_F_holdout = female_prefecture_dx, 
                                       horizon = iwj, length_test_data = 20)
        subnational_double_gap_fore_male_ETS[iwk,iwj,] = dum$err_M
        subnational_double_gap_fore_female_ETS[iwk,iwj,] = dum$err_F
        rm(iwj)
    }
    print(iwk); rm(iwk)
}

# by prefecture

subnational_double_gap_fore_male_ETS_prefecture_mean = apply(subnational_double_gap_fore_male_ETS, c(1, 3), mean)
subnational_double_gap_fore_female_ETS_prefecture_mean = apply(subnational_double_gap_fore_female_ETS, c(1, 3), mean)

# by forecast horizon

subnational_double_gap_fore_male_ETS_mean   = apply(subnational_double_gap_fore_male_ETS,   c(2, 3), mean)
subnational_double_gap_fore_female_ETS_mean = apply(subnational_double_gap_fore_female_ETS, c(2, 3), mean)

