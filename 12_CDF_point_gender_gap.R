##########################
# F & M (subnational)
# years from 1973 to 2024
##########################

# ARIMA

subnational_gender_gap_KLD_JSD_M_ARIMA = subnational_gender_gap_KLD_JSD_F_ARIMA = 
                                          array(NA, dim = c(n_state, 20, 2), dimnames = list(state, 1:20, c("KLD", "JSD")))
for(iwj in 1:n_state)
{
    for(iwk in 1:20)
    {
        dum = gender_gap_fore_fun(CDF_M_data = subnational_male_prefecture_dx_CDF[[iwj]], 
                                  CDF_F_data = subnational_female_prefecture_dx_CDF[[iwj]],
                                  PDF_M_holdout_data = male_prefecture_dx[[iwj]], 
                                  PDF_F_holdout_data = female_prefecture_dx[[iwj]],
                                  horizon = iwk, fore_method = "arima", length_test_data = 20)
        subnational_gender_gap_KLD_JSD_M_ARIMA[iwj,iwk,] = dum$err_M
        subnational_gender_gap_KLD_JSD_F_ARIMA[iwj,iwk,] = dum$err_F
        rm(dum); rm(iwk); rm(dum)
    }
    print(iwj); rm(iwj)
}

# average by prefecture

subnational_gender_gap_KLD_JSD_M_ARIMA_prefecture_mean = apply(subnational_gender_gap_KLD_JSD_M_ARIMA, c(1, 3), mean)
subnational_gender_gap_KLD_JSD_F_ARIMA_prefecture_mean = apply(subnational_gender_gap_KLD_JSD_F_ARIMA, c(1, 3), mean)
rownames(subnational_gender_gap_KLD_JSD_M_ARIMA_prefecture_mean) = rownames(subnational_gender_gap_KLD_JSD_F_ARIMA_prefecture_mean) = state

# average by forecast horizon

subnational_gender_gap_KLD_JSD_M_ARIMA_mean = apply(subnational_gender_gap_KLD_JSD_M_ARIMA, c(2, 3), mean)
subnational_gender_gap_KLD_JSD_F_ARIMA_mean = apply(subnational_gender_gap_KLD_JSD_F_ARIMA, c(2, 3), mean)

######
# ETS
######

subnational_gender_gap_KLD_JSD_M_ETS = subnational_gender_gap_KLD_JSD_F_ETS = array(NA, dim = c(n_state, 20, 2), 
                                    dimnames = list(state, 1:20, c("KLD", "JSD")))
for(iwj in 1:n_state)
{
    for(iwk in 1:20)
    {
        dum = gender_gap_fore_fun(CDF_M = subnational_male_prefecture_dx_CDF[[iwj]], 
                                  CDF_F = subnational_female_prefecture_dx_CDF[[iwj]],
                                  PDF_M_holdout = male_prefecture_dx[[iwj]], 
                                  PDF_F_holdout = female_prefecture_dx[[iwj]],
                                  horizon = iwk, fore_method = "ets", length_test_data = 20)
        subnational_gender_gap_KLD_JSD_M_ETS[iwj,iwk,] = dum$err_M
        subnational_gender_gap_KLD_JSD_F_ETS[iwj,iwk,] = dum$err_F
        rm(dum); rm(iwk); rm(dum)
    }
    print(iwj); rm(iwj)
}

# average by prefecture

subnational_gender_gap_KLD_JSD_M_ETS_prefecture_mean = apply(subnational_gender_gap_KLD_JSD_M_ETS, c(1, 3), mean)
subnational_gender_gap_KLD_JSD_F_ETS_prefecture_mean = apply(subnational_gender_gap_KLD_JSD_F_ETS, c(1, 3), mean)
rownames(subnational_gender_gap_KLD_JSD_M_ETS_prefecture_mean) = rownames(subnational_gender_gap_KLD_JSD_F_ETS_prefecture_mean) = state

# average by forecast horizon

subnational_gender_gap_KLD_JSD_M_ETS_mean = apply(subnational_gender_gap_KLD_JSD_M_ETS, c(2, 3), mean)
subnational_gender_gap_KLD_JSD_F_ETS_mean = apply(subnational_gender_gap_KLD_JSD_F_ETS, c(2, 3), mean)

