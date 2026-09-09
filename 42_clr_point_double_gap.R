# load R packages

setwd("~/Library/CloudStorage/Dropbox/Todos/FANOVA_FFM_CDF/code")
source("load_packages.R")
source("auxiliary_point.R")

# subnational data for handling zero counts

replace_zero <- function(x) 
{
  if(any(x == 0)) cmultRepl(x, method = "CZM") else x
}

# double gap

double_gap_clr <- function(horizon, fmethod, length_test_data)
{
    female_den_fore = male_den_fore = array(NA, dim = c(n_age, (length_test_data + 1 - horizon), n_state), 
                                            dimnames = list(ages, 1:(length_test_data + 1 - horizon), state))
    
    for(iwk in 1:n_state)
    {
        for(ik in 1:(length_test_data + 1 - horizon))
        {
            female_val = replace_zero(female_prefecture_dx_array[1:(n_year - (length_test_data + 1) + ik),,iwk])
            male_val = replace_zero(male_prefecture_dx_array[1:(n_year - (length_test_data + 1) + ik),,iwk])
            
            # compute region gap
            
            subnational_female_region_gap = CLR(female_val)$LR - CLR(Japan_female_dum[1:(n_year - (length_test_data + 1) + ik),])$LR
            subnational_female_region_gap_fore = forecast(ftsm(fts(ages, t(subnational_female_region_gap))), h = horizon, method = fmethod)$mean$y[,horizon]
            
            # compute Japanese female forecast
            
            female_national_clr_fore = forecast(ftsm(fts(ages, t(CLR(Japan_female_dum[1:(n_year - (length_test_data + 1) + ik),])$LR))), h = horizon, method = fmethod)$mean$y[,horizon]
            
            # compute Japanese subnational female forecast
            
            female_subnational_clr_fore = female_national_clr_fore + subnational_female_region_gap_fore
            
            # compute gender gap between Japanese subnational female and male data
            
            subnational_gender_gap = CLR(male_val)$LR - CLR(female_val)$LR
            subnational_gender_gap_fore = forecast(ftsm(fts(ages, t(subnational_gender_gap))), h = horizon, method = fmethod)$mean$y[,horizon]
            
            # compute Japanese subnational male forecast
            
            male_subnational_clr_fore = female_subnational_clr_fore + subnational_gender_gap_fore
            
            # inverse CLR
            
            female_den_fore[,ik,iwk] = as.numeric(invCLR(matrix(female_subnational_clr_fore, nrow = 1))) * 10^5
            male_den_fore[,ik,iwk] = as.numeric(invCLR(matrix(male_subnational_clr_fore, nrow = 1))) * 10^5
            
            rm(ik); rm(female_val); rm(male_val); rm(subnational_female_region_gap); rm(subnational_female_region_gap_fore)
            rm(female_national_clr_fore); rm(female_subnational_clr_fore); rm(subnational_gender_gap); rm(subnational_gender_gap_fore); rm(male_subnational_clr_fore)
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

double_gap_clr_KLD_M_ets = double_gap_clr_KLD_F_ets = matrix(NA, 20, n_state)
for(iwj in 1:20)
{
    dum = double_gap_clr(horizon = iwj, fmethod = "ets", length_test_data = 20)
    double_gap_clr_KLD_M_ets[iwj,] = dum$KLD_M
    double_gap_clr_KLD_F_ets[iwj,] = dum$KLD_F
    print(iwj); rm(iwj); rm(dum)
}
colnames(double_gap_clr_KLD_M_ets) = colnames(double_gap_clr_KLD_F_ets) = state
rownames(double_gap_clr_KLD_M_ets) = rownames(double_gap_clr_KLD_F_ets) = 1:20

# by prefecture

double_gap_clr_KLD_M_ets_mean = colMeans(double_gap_clr_KLD_M_ets)
double_gap_clr_KLD_F_ets_mean = colMeans(double_gap_clr_KLD_F_ets)

# by horizon

horizon_double_gap_clr_KLD_M_ets_mean = rowMeans(double_gap_clr_KLD_M_ets)
horizon_double_gap_clr_KLD_F_ets_mean = rowMeans(double_gap_clr_KLD_F_ets)

## fmethod = "arima"

double_gap_clr_KLD_M_arima = double_gap_clr_KLD_F_arima = matrix(NA, 20, n_state)
for(iwj in 1:20)
{
    dum = double_gap_clr(horizon = iwj, fmethod = "arima", length_test_data = 20)
    double_gap_clr_KLD_M_arima[iwj,] = dum$KLD_M
    double_gap_clr_KLD_F_arima[iwj,] = dum$KLD_F
    print(iwj); rm(iwj); rm(dum)
}
colnames(double_gap_clr_KLD_M_arima) = colnames(double_gap_clr_KLD_F_arima) = state
rownames(double_gap_clr_KLD_M_arima) = rownames(double_gap_clr_KLD_F_arima) = 1:20

# by prefecture

double_gap_clr_KLD_M_arima_mean = colMeans(double_gap_clr_KLD_M_arima)
double_gap_clr_KLD_F_arima_mean = colMeans(double_gap_clr_KLD_F_arima)

# by horizon

horizon_double_gap_clr_KLD_M_arima_mean = rowMeans(double_gap_clr_KLD_M_arima)
horizon_double_gap_clr_KLD_F_arima_mean = rowMeans(double_gap_clr_KLD_F_arima)

