#######################
# gender gap modelling
#######################

gender_gap_fore <- function(male_CDF, female_CDF, fh, fmethod)
{
    N_nrow = nrow(male_CDF)
    
    # gender gap
    
    gender_gap = male_CDF - female_CDF
    
    # take Fisher Z transformation
    
    gender_gap_fisherz = FisherZ(gender_gap)
    
    # fitting a FDM via the Fisher Z transformation
    
    gender_gap_forecast_val <- forecast(ftsm(fts(ages, gender_gap_fisherz)), h = fh, method = fmethod)
    
    # foreacsting female data via logit transformation
    
    if(any(female_CDF <= 0))
    {
        female_CDF = replace(female_CDF, which(female_CDF == 0), 10^-8)
    }
    
    female_forecast_val <- forecast(ftsm(fts(ages[1:(N_nrow - 1)], logit(female_CDF[1:(N_nrow - 1),])), 
                                         ngrid = 501), h = fh, method = fmethod)
    
    # obtaining male data
    
    gender_gap_fisherz_fts_forecast_transform <- FisherZInv(gender_gap_forecast_val$mean$y[,fh])
    female_pop_CDF_fts_forecast <- c(invlogit(female_forecast_val$mean$y[,fh]), 1)
    male_pop_CDF_fts_forecast <- female_pop_CDF_fts_forecast + gender_gap_fisherz_fts_forecast_transform
    
    # male data between 0 and 1
    
    male_pop_CDF_fts_forecast <- pmin(pmax(male_pop_CDF_fts_forecast, 0), 1)
    
    # turn CDF to PDF
    
    female_pop_PDF_fts_forecast <- c(female_pop_CDF_fts_forecast[1], diff(female_pop_CDF_fts_forecast))
    male_pop_PDF_fts_forecast   <- c(male_pop_CDF_fts_forecast[1],   diff(male_pop_CDF_fts_forecast))
    return(list(male_fore = pmax(male_pop_PDF_fts_forecast, 0) * 10^5, 
                female_fore = pmax(female_pop_PDF_fts_forecast, 0) * 10^5))
}

# compute the KLD

gender_gap_fore_fun <- function(CDF_M_data, CDF_F_data, PDF_M_holdout_data, PDF_F_holdout_data, horizon, fore_method, 
         length_test_data)
{  
    n_col = ncol(CDF_M_data)
    n_row = nrow(CDF_M_data)
    
    forecast_M = forecast_F = matrix(NA, n_row, (length_test_data + 1 - horizon))
    for(ik in 1:(length_test_data + 1 - horizon))
    {
        dum = gender_gap_fore(male_CDF = CDF_M_data[,1:(n_col - (length_test_data + 1) + ik)], 
                              female_CDF = CDF_F_data[,1:(n_col - (length_test_data + 1) + ik)], 
                              fh = horizon, fmethod = fore_method)
        forecast_M[,ik] = dum$male_fore
        forecast_F[,ik] = dum$female_fore
        rm(ik); rm(dum)
    }
    
    # holdout data
    
    PDF_M = matrix(t(PDF_M_holdout_data[(n_col - length_test_data + horizon):n_col,]), n_row, (length_test_data + 1 - horizon))
    PDF_F = matrix(t(PDF_F_holdout_data[(n_col - length_test_data + horizon):n_col,]), n_row, (length_test_data + 1 - horizon))
    
    # KLD and JSD
    
    KL_div_val_M = JS_div_val_M = KL_div_val_F = JS_div_val_F = vector("numeric", (length_test_data + 1 - horizon))
    for(ij in 1:(length_test_data + 1 - horizon))
    {
        # symmetric KL dist
        
        KL_div_val_M[ij] = mean(KLdiv(cbind(forecast_M[,ij], PDF_M[,ij]))[2:3])
        KL_div_val_F[ij] = mean(KLdiv(cbind(forecast_F[,ij], PDF_F[,ij]))[2:3])
        
        # Jensen-Shannon dist
        
        JS_div_val_M[ij] = mean(KLdiv(cbind(forecast_M[,ij], 
                                            apply(cbind(forecast_M[,ij], PDF_M[,ij]), 1, geometric.mean)))[2:3])
        JS_div_val_F[ij] = mean(KLdiv(cbind(forecast_F[,ij], 
                                            apply(cbind(forecast_F[,ij], PDF_F[,ij]), 1, geometric.mean)))[2:3])
    }
    err_M = c(mean(KL_div_val_M), mean(JS_div_val_M))
    err_F = c(mean(KL_div_val_F), mean(JS_div_val_F))
    return(list(err_M = err_M, err_F = err_F, forecast_M = forecast_M, forecast_F = forecast_F, 
                PDF_M = PDF_M, PDF_F = PDF_F))
}
