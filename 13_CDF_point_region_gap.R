######################
# region gap modeling
######################

# s_M_CDF: subnational prefecture male CDF
# s_F_CDF: subnational prefecture female CDF
# N_M_CDF: national male CDF
# N_F_CDF: national female CDF
# fh: forecast horizon
# fmethod: univariate time-series forecasting method, such as ARIMA or ETS
# length_test_data: length of testing data

region_gap_fore_fun <- function(s_M_CDF, s_F_CDF, N_M_CDF, N_F_CDF, fh, fmethod, length_test_data)
{
    N_nrow = nrow(N_M_CDF)
    N_ncol = ncol(N_M_CDF)
    
    # ensure national CDF >= 0
    
    N_F_CDF[N_F_CDF <= 0] <- 1e-8
    N_M_CDF[N_M_CDF <= 0] <- 1e-8
    
    # forecast national female and male CDFs via logit transformation
    
    Japan_female_pop_CDF_forecast = Japan_male_pop_CDF_forecast = matrix(NA, N_nrow, (length_test_data + 1 - fh))
    for(ik in 1:(length_test_data + 1 - fh))
    {
        Japan_female_pop_CDF_forecast[,ik] = c(invlogit(forecast(ftsm(fts(ages[1:(N_nrow - 1)], logit(N_F_CDF[1:(N_nrow - 1),1:(N_ncol - (length_test_data + 1) + ik)]))), h = fh, method = fmethod)$mean$y[,fh]), 1)
        Japan_male_pop_CDF_forecast[,ik]   = c(invlogit(forecast(ftsm(fts(ages[1:(N_nrow - 1)], logit(N_M_CDF[1:(N_nrow - 1),1:(N_ncol - (length_test_data + 1) + ik)]))), h = fh, method = fmethod)$mean$y[,fh]), 1)
        rm(ik)
    }
    
    # region gap via Fisher Z transformation
    
    subnational_region_F_diff_CDF_forecast = subnational_region_M_diff_CDF_forecast = array(NA, dim = c(N_nrow, (length_test_data + 1 - fh), 47))
    for(ik in 1:n_state)
    {
        n_col = ncol(subnational_region_F_diff_CDF[[ik]])
        for(ij in 1:(length_test_data + 1 - fh))
        {
            subnational_region_F_diff_CDF_forecast[,ij,ik] = FisherZInv(forecast(ftsm(fts(ages, FisherZ(subnational_region_F_diff_CDF[[ik]][,1:(n_col - (length_test_data + 1) + ij)]))), h = fh, method = fmethod)$mean$y[,fh])
            subnational_region_M_diff_CDF_forecast[,ij,ik] = FisherZInv(forecast(ftsm(fts(ages, FisherZ(subnational_region_M_diff_CDF[[ik]][,1:(n_col - (length_test_data + 1) + ij)]))), h = fh, method = fmethod)$mean$y[,fh])
        }
        rm(ik); rm(n_col)
    }
    
    # region gap + national (M or F) forecasts
    
    subnational_female_pop_CDF_forecast = subnational_male_pop_CDF_forecast = array(NA, dim = c(N_nrow, (length_test_data + 1 - fh), 47))
    for(ik in 1:n_state)
    {
        subnational_female_pop_CDF_forecast[,,ik] = subnational_region_F_diff_CDF_forecast[,,ik] + Japan_female_pop_CDF_forecast
        subnational_male_pop_CDF_forecast[,,ik]   = subnational_region_M_diff_CDF_forecast[,,ik] + Japan_male_pop_CDF_forecast
        rm(ik)
    }
    colnames(subnational_female_pop_CDF_forecast) = colnames(subnational_male_pop_CDF_forecast) = 1:(length_test_data + 1 - fh)
    rownames(subnational_female_pop_CDF_forecast) = rownames(subnational_male_pop_CDF_forecast) = ages
    
    # ensure CDF between 0 and 1
    
    subnational_female_pop_CDF_forecast <- pmin(pmax(subnational_female_pop_CDF_forecast, 10^-8), 1)
    subnational_male_pop_CDF_forecast   <- pmin(pmax(subnational_male_pop_CDF_forecast, 10^-8), 1)

    # from CDF to PDF via first-order differencing
    
    subnational_female_pop_PDF_forecast = subnational_male_pop_PDF_forecast = array(NA, dim = c(N_nrow, (length_test_data + 1 - fh), 47))
    for(ik in 1:n_state)
    {
        for(ij in 1:(length_test_data + 1 - fh))
        {
            subnational_female_pop_PDF_forecast[,ij,ik] = c(subnational_female_pop_CDF_forecast[1,ij,ik], diff(subnational_female_pop_CDF_forecast[,ij,ik]))
            subnational_male_pop_PDF_forecast[,ij,ik]   = c(subnational_male_pop_CDF_forecast[1,ij,ik],   diff(subnational_male_pop_CDF_forecast[,ij,ik]))
            rm(ij)
        }
        rm(ik)    
    }
    rownames(subnational_female_pop_PDF_forecast) = rownames(subnational_male_pop_PDF_forecast) = ages
    colnames(subnational_female_pop_PDF_forecast) = colnames(subnational_male_pop_PDF_forecast) = 1:(length_test_data + 1 - fh)
    
    subnational_female_pop_PDF_forecast = pmax(subnational_female_pop_PDF_forecast, 0)
    subnational_male_pop_PDF_forecast = pmax(subnational_male_pop_PDF_forecast, 0)
    return(list(female_fore = subnational_female_pop_PDF_forecast * 10^5, 
                male_fore = subnational_male_pop_PDF_forecast * 10^5))
}

# ARIMA univariate time-series forecasting method

subnational_region_gap_fore_F_ARIMA = subnational_region_gap_fore_M_ARIMA = list()
for(iwj in 1:20)
{
    dum = region_gap_fore_fun(s_M_CDF = subnational_male_prefecture_dx_CDF, 
                              s_F_CDF = subnational_female_prefecture_dx_CDF,
                              N_M_CDF = Japan_male_pop_CDF,
                              N_F_CDF = Japan_female_pop_CDF, 
                              fh = iwj, fmethod = "arima", length_test_data = 20)
    subnational_region_gap_fore_F_ARIMA[[iwj]] = dum$female_fore
    subnational_region_gap_fore_M_ARIMA[[iwj]] = dum$male_fore
    print(iwj); rm(iwj)
}

# ETS univariate time-series forecasting method

subnational_region_gap_fore_F_ETS = subnational_region_gap_fore_M_ETS = list()
for(iwj in 1:20)
{
    dum = region_gap_fore_fun(s_M_CDF = subnational_male_prefecture_dx_CDF, 
                              s_F_CDF = subnational_female_prefecture_dx_CDF,
                              N_M_CDF = Japan_male_pop_CDF, 
                              N_F_CDF = Japan_female_pop_CDF, 
                              fh = iwj, fmethod = "ets", length_test_data = 20)
    subnational_region_gap_fore_F_ETS[[iwj]] = dum$female_fore
    subnational_region_gap_fore_M_ETS[[iwj]] = dum$male_fore
    print(iwj); rm(iwj)
}

##############################################################
# assess forecast accuracy between forecasts and holdout data
##############################################################

# forecast_M: male forecasts
# forecast_F: female forecasts
# PDF_M_holdout: male holdout data
# PDF_F_holdout: female holdout data
# horizon: forecast horizon
# length_test_data: length of testing data

region_gap_fore_fun_fore <- function(forecast_M, forecast_F, PDF_M_holdout, PDF_F_holdout, 
                                     horizon, length_test_data)
{
    n_col = nrow(PDF_M_holdout)
    n_row = ncol(PDF_M_holdout)
    
    # holdout data
    
    PDF_M = matrix(t(PDF_M_holdout[(n_col - length_test_data + horizon):n_col,]), n_row, (length_test_data + 1 - horizon))
    PDF_F = matrix(t(PDF_F_holdout[(n_col - length_test_data + horizon):n_col,]), n_row, (length_test_data + 1 - horizon))
    
    # KLD and JSD
    
    KL_div_val_M = JS_div_val_M = KL_div_val_F = JS_div_val_F = vector("numeric", (length_test_data + 1 - horizon))
    for(ij in 1:(length_test_data + 1 - horizon))
    {
        # symmetric KLD
        
        KL_div_val_M[ij] = mean(KLdiv(cbind(forecast_M[,ij], PDF_M[,ij]))[2:3])
        KL_div_val_F[ij] = mean(KLdiv(cbind(forecast_F[,ij], PDF_F[,ij]))[2:3])
        
        # Jensen-Shannon divergence (JSD)
        
        JS_div_val_M[ij] = mean(KLdiv(cbind(forecast_M[,ij], 
                                            apply(cbind(forecast_M[,ij], PDF_M[,ij]), 1, geometric.mean)))[2:3])
        JS_div_val_F[ij] = mean(KLdiv(cbind(forecast_F[,ij], 
                                            apply(cbind(forecast_F[,ij], PDF_F[,ij]), 1, geometric.mean)))[2:3])
    }
    # compute KLD and JSD
    
    err_M = c(mean(KL_div_val_M), mean(JS_div_val_M))
    err_F = c(mean(KL_div_val_F), mean(JS_div_val_F))
    return(list(err_M = err_M, err_F = err_F))
}

#########
## ARIMA
#########

subnational_region_gap_fore_male_ARIMA = subnational_region_gap_fore_female_ARIMA = array(NA, dim = c(n_state, 20, 2),
                                                                dimnames = list(state, 1:20, c("KLD", "JSD")))
for(iwj in 1:n_state)
{
    for(iwk in 1:20)
    {
        dum = region_gap_fore_fun_fore(forecast_M = matrix(subnational_region_gap_fore_M_ARIMA[[iwk]][,,iwj], n_age, ),
                                       forecast_F = matrix(subnational_region_gap_fore_F_ARIMA[[iwk]][,,iwj], n_age, ),
                                       PDF_M_holdout = male_prefecture_dx[[iwj]], 
                                       PDF_F_holdout = female_prefecture_dx[[iwj]],
                                       horizon = iwk, length_test_data = 20)
        subnational_region_gap_fore_male_ARIMA[iwj,iwk,] = dum$err_M
        subnational_region_gap_fore_female_ARIMA[iwj,iwk,] = dum$err_F
        rm(iwk); rm(dum)
    }
    print(iwj); rm(iwj)
}

# by prefecture

subnational_region_gap_fore_male_ARIMA_prefecture_mean   = apply(subnational_region_gap_fore_male_ARIMA,   c(1, 3), mean)
subnational_region_gap_fore_female_ARIMA_prefecture_mean = apply(subnational_region_gap_fore_female_ARIMA, c(1, 3), mean)

# by forecast horizon

subnational_region_gap_fore_male_ARIMA_mean   = apply(subnational_region_gap_fore_male_ARIMA,   c(2, 3), mean)
subnational_region_gap_fore_female_ARIMA_mean = apply(subnational_region_gap_fore_female_ARIMA, c(2, 3), mean)

rownames(subnational_region_gap_fore_male_ARIMA_mean) = rownames(subnational_region_gap_fore_female_ARIMA_mean) = 1:20
colnames(subnational_region_gap_fore_male_ARIMA_mean) = colnames(subnational_region_gap_fore_female_ARIMA_mean) = c("KLD", "JSD")

#######
## ETS
#######

subnational_region_gap_fore_male_ETS = subnational_region_gap_fore_female_ETS = array(NA, dim = c(n_state, 20, 2),
                                                                dimnames = list(state, 1:20, c("KLD", "JSD")))
for(iwj in 1:n_state)
{
    for(iwk in 1:20)
    {
        dum = region_gap_fore_fun_fore(forecast_M = matrix(subnational_region_gap_fore_M_ETS[[iwk]][,,iwj], n_age, ),
                                       forecast_F = matrix(subnational_region_gap_fore_F_ETS[[iwk]][,,iwj], n_age, ),
                                       PDF_M_holdout = male_prefecture_dx[[iwj]], 
                                       PDF_F_holdout = female_prefecture_dx[[iwj]],
                                       horizon = iwk, length_test_data = 20)
        subnational_region_gap_fore_male_ETS[iwj,iwk,] = dum$err_M
        subnational_region_gap_fore_female_ETS[iwj,iwk,] = dum$err_F
        rm(iwk); rm(dum)
    }
    print(iwj); rm(iwj)
}

# by prefecture

subnational_region_gap_fore_male_ETS_prefecture_mean   = apply(subnational_region_gap_fore_male_ETS,   c(1, 3), mean)
subnational_region_gap_fore_female_ETS_prefecture_mean = apply(subnational_region_gap_fore_female_ETS, c(1, 3), mean)

# by horizon

subnational_region_gap_fore_male_ETS_mean   = apply(subnational_region_gap_fore_male_ETS,   c(2, 3), mean)
subnational_region_gap_fore_female_ETS_mean = apply(subnational_region_gap_fore_female_ETS, c(2, 3), mean)

rownames(subnational_region_gap_fore_male_ETS_mean) = rownames(subnational_region_gap_fore_female_ETS_mean) = 1:20
colnames(subnational_region_gap_fore_male_ETS_mean) = colnames(subnational_region_gap_fore_female_ETS_mean) = c("KLD", "JSD")

