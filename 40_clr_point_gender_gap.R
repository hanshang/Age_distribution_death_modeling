############################
# years 1973:2004
# last 20 years for testing
#############################

# load R packages

setwd("~/Dropbox/Todos/FANOVA_FFM_CDF/code")
source("load_packages.R")
source("auxiliary_point.R")
load("read_data.RData")

# subnational data for handling zero counts

replace_zero <- function(x) 
{
    if(any(x == 0)) cmultRepl(x, method = "CZM") else x
}

# point forecasts and KLD

gender_gap_clr <- function(horizon, fmethod, length_test_data)
{
    female_den_fore = male_den_fore = array(NA, dim = c(n_age, (length_test_data + 1 - horizon), n_state),
                                            dimnames = list(ages, 1:(length_test_data + 1 - horizon), state))
    for(iwk in 1:n_state)
    {
        for(ik in 1:(length_test_data + 1 - horizon))
        {
            female_val = replace_zero(female_prefecture_dx_array[1:(n_year - (length_test_data + 1) + ik),,iwk])
            male_val = replace_zero(male_prefecture_dx_array[1:(n_year - (length_test_data + 1) + ik),,iwk])
    
            # compute gender gap (male - female)
        
            subnational_gender_gap = CLR(male_val)$LR - CLR(female_val)$LR
            subnational_gender_gap_fore = forecast(ftsm(fts(ages, t(subnational_gender_gap))), 
                                                   h = horizon, method = fmethod)$mean$y[,horizon]
    
            # h-step-ahead forecast
            
            female_clr_fore = forecast(ftsm(fts(ages, t(CLR(female_val)$LR))), h = horizon, method = fmethod)$mean$y[,horizon]
            male_clr_fore = female_clr_fore + subnational_gender_gap_fore
            
            female_den_fore[,ik,iwk] = as.numeric(invCLR(matrix(female_clr_fore, nrow = 1))) * 10^5
            male_den_fore[,ik,iwk] = as.numeric(invCLR(matrix(male_clr_fore, nrow = 1))) * 10^5
            rm(ik); rm(female_clr_fore); rm(male_clr_fore); rm(female_val); rm(male_val); rm(subnational_gender_gap); rm(subnational_gender_gap_fore)
        }
        rm(iwk)                
    }

    # compute symmetric KLD
    
    KL_div_val_M = KL_div_val_F = matrix(NA, (length_test_data + 1 - horizon), n_state)
    for(iwk in 1:n_state)    
    {
        # holdout data
    
        PDF_M = matrix(t(male_prefecture_dx_array[(n_year - length_test_data + horizon):n_year,,iwk]), n_age, (length_test_data + 1 - horizon))
        PDF_F = matrix(t(female_prefecture_dx_array[(n_year - length_test_data + horizon):n_year,,iwk]), n_age, (length_test_data + 1 - horizon))
        
        for(ij in 1:(length_test_data + 1 - horizon))
        {
            KL_div_val_M[ij,iwk] = mean(KLdiv(cbind(male_den_fore[,ij,iwk], PDF_M[,ij]))[2:3])
            KL_div_val_F[ij,iwk] = mean(KLdiv(cbind(female_den_fore[,ij,iwk], PDF_F[,ij]))[2:3])
            rm(ij)
        }
        rm(iwk); rm(PDF_M); rm(PDF_F)
    }
    colnames(KL_div_val_M) = colnames(KL_div_val_F) = state
    rownames(KL_div_val_M) = rownames(KL_div_val_F) = 1:(length_test_data + 1 - horizon)
    return(list(KLD_M = colMeans(KL_div_val_M), KLD_F = colMeans(KL_div_val_F)))
}

##################################
# one- to 20-step-ahead forecasts
##################################

## fmethod = "ets"

gender_gap_clr_KLD_M_ets = gender_gap_clr_KLD_F_ets = matrix(NA, 20, n_state)
for(iwj in 1:20)
{
    dum = gender_gap_clr(horizon = iwj, fmethod = "ets", length_test_data = 20)
    gender_gap_clr_KLD_M_ets[iwj,] = dum$KLD_M
    gender_gap_clr_KLD_F_ets[iwj,] = dum$KLD_F
    print(iwj); rm(iwj); rm(dum)
}
colnames(gender_gap_clr_KLD_M_ets) = colnames(gender_gap_clr_KLD_F_ets) = state
rownames(gender_gap_clr_KLD_M_ets) = rownames(gender_gap_clr_KLD_F_ets) = 1:20

# by prefecture

gender_gap_clr_KLD_M_ets_mean = colMeans(gender_gap_clr_KLD_M_ets)
gender_gap_clr_KLD_F_ets_mean = colMeans(gender_gap_clr_KLD_F_ets)

# by horizon

horizon_gender_gap_clr_KLD_M_ets_mean = rowMeans(gender_gap_clr_KLD_M_ets)
horizon_gender_gap_clr_KLD_F_ets_mean = rowMeans(gender_gap_clr_KLD_F_ets)


## fmethod = "arima"

gender_gap_clr_KLD_M_arima = gender_gap_clr_KLD_F_arima = matrix(NA, 20, n_state)
for(iwj in 1:20)
{
    dum = gender_gap_clr(horizon = iwj, fmethod = "arima", length_test_data = 20)
    gender_gap_clr_KLD_M_arima[iwj,] = dum$KLD_M
    gender_gap_clr_KLD_F_arima[iwj,] = dum$KLD_F
    print(iwj); rm(iwj); rm(dum)
}
colnames(gender_gap_clr_KLD_M_arima) = colnames(gender_gap_clr_KLD_F_arima) = state
rownames(gender_gap_clr_KLD_M_arima) = rownames(gender_gap_clr_KLD_F_arima) = 1:20

# by prefecture

gender_gap_clr_KLD_F_arima_mean = colMeans(gender_gap_clr_KLD_F_arima)
gender_gap_clr_KLD_M_arima_mean = colMeans(gender_gap_clr_KLD_M_arima)

# by horizon

horizon_gender_gap_clr_KLD_F_arima_mean = rowMeans(gender_gap_clr_KLD_F_arima)
horizon_gender_gap_clr_KLD_M_arima_mean = rowMeans(gender_gap_clr_KLD_M_arima)

