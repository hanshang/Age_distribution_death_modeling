#####################
# interval forecasts
#####################

## determining the optimal tuning parameter via standard deviation

# tune_para: tuning parameter
# resi_mat: residual functions
# sd_val_input: functional standard deviation (pointwise)
# alpha_level: level of significance

tune_para_find_function <- function(tune_para, resi_mat, sd_val_input, alpha_level)
{
    n_age = nrow(resi_mat)
    ind = matrix(NA, n_age, ncol(resi_mat))
    for(iw in 1:ncol(resi_mat))
    {
        ind[,iw] = ifelse(between(resi_mat[,iw], -tune_para * sd_val_input, tune_para * sd_val_input), 1, 0)
        rm(iw)
    }
    ecp = sum(ind)/(n_age * ncol(resi_mat))
    rm(ind)
    return(abs(ecp - alpha_level))
}

# interval score

interval_score <- function(holdout, lb, ub, alpha)
{
    lb_ind = ifelse(holdout < lb, 1, 0)
    ub_ind = ifelse(holdout > ub, 1, 0)
    score = (ub - lb) + 2/alpha * ((lb - holdout) * lb_ind + (holdout - ub) * ub_ind)
    cover = 1 - (length(which(lb_ind == 1)) + length(which(ub_ind == 1)))/length(holdout)
    cpd = abs(cover - (1 - alpha))
    return(c(cover, cpd, mean(score)))
}

# determining the optimal tuning parameter via standard deviation

tune_para_find_function_PI_type <- function(tune_para, resi_mat, sd_val_input, alpha_level, PI_type)
{
    n_age = nrow(resi_mat)
    if(PI_type == "pointwise")
    {
        ind = matrix(NA, n_age, ncol(resi_mat))
        for(iw in 1:ncol(resi_mat))
        {
            ind[,iw] = ifelse(between(resi_mat[,iw], -tune_para * sd_val_input, tune_para * sd_val_input), 1, 0)
            rm(iw)
        }
        ecp = sum(ind)/(n_age * ncol(resi_mat))
    }
    else if(PI_type == "uniform")
    {
        ind = vector("numeric", ncol(resi_mat))
        for(iw in 1:ncol(resi_mat))
        {
            ind[iw] = ifelse(all(between(resi_mat[,iw], -tune_para * sd_val_input, 
                                         tune_para * sd_val_input)), 1, 0)
            rm(iw)
        }
        ecp = sum(ind)/ncol(resi_mat)
    }
    else
    {
        warning("PI type must either be pointwise or uniform.")
    }
    rm(ind)
    return(abs(ecp - alpha_level))
}

# uniform cpd

uniform_cpd <- function(holdout, lb, ub, alpha)
{
    lb_ind = ifelse(any(holdout < lb), 1, 0)
    ub_ind = ifelse(any(holdout > ub), 1, 0)
    cover = 1 - (length(which(lb_ind == 1)) + length(which(ub_ind == 1)))/ncol(holdout)
    cpd = abs(cover - (1 - alpha))
    return(c(cover, cpd))
}

# interval forecast function for HDFPCA

int_hdfpca_fun <- function(fdata_F, fdata_M, horizon, first_order, second_order, transformation, level_sig, uni_fore_method)
{
    n_year = nrow(fdata_F)
    n_age = ncol(fdata_F)
    forecast_validation_F = forecast_validation_M = matrix(NA, ncol(fdata_F), (22 - horizon))
    if(transformation == "direct")
    {
        for(ij in 1:(22 - horizon))
        {
            data_F = fdata_F[1:(n_year - 42 + ij),]
            data_M = fdata_M[1:(n_year - 42 + ij),]
            data_comb = list()
            data_comb[[1]] = t(data_F)
            data_comb[[2]] = t(data_M)
            
            fore_val = forecast.hdfpca(hdfpca(y = data_comb, order = first_order, r = second_order), h = horizon, fmethod = uni_fore_method)$forecast
            forecast_validation_F[,ij] = (fore_val[[1]])[,horizon]
            forecast_validation_M[,ij] = (fore_val[[2]])[,horizon]
            rm(ij); rm(fore_val)
        }
    }
    else if(transformation == "CLR")
    {
        for(ij in 1:(22 - horizon))
        {
            data_F = as.matrix(clr(fdata_F[1:(n_year - 42 + ij),]))
            data_M = as.matrix(clr(fdata_M[1:(n_year - 42 + ij),]))
            data_comb = list()
            data_comb[[1]] = t(data_F)
            data_comb[[2]] = t(data_M)
            
            fore_val = forecast.hdfpca(hdfpca(y = data_comb, order = first_order, r = second_order), h = horizon, fmethod = uni_fore_method)$forecast
            forecast_validation_F[,ij] = as.numeric(clrInv((fore_val[[1]])[,horizon])) * 10^5
            forecast_validation_M[,ij] = as.numeric(clrInv((fore_val[[2]])[,horizon])) * 10^5
            rm(ij); rm(fore_val)
        }	
    }
    else if(transformation == "CDF")
    {
        for(ijk in 1:(22 - horizon))
        {
            data_F = fdata_F[1:(n_year - 42 + ijk),]/10^5
            data_M = fdata_M[1:(n_year - 42 + ijk),]/10^5
            
            data_F_cumsum_dum = data_M_cumsum_dum = matrix(NA, nrow(data_F), ncol(data_F))
            for(iw in 1:nrow(data_F))
            {
                data_F_cumsum_dum[iw,] = cumsum(data_F[iw,])
                data_M_cumsum_dum[iw,] = cumsum(data_M[iw,])
                rm(iw)
            }
            
            # check if any cumsum values equal to 0
            if(any(data_F_cumsum_dum == 0))
            {
                data_F_cumsum = replace(data_F_cumsum_dum, which(data_F_cumsum_dum == 0), 10^-5)
            }
            else
            {
                data_F_cumsum = data_F_cumsum_dum
            }
            
            if(any(data_M_cumsum_dum == 0))
            {
                data_M_cumsum = replace(data_M_cumsum_dum, which(data_M_cumsum_dum == 0), 10^-5)
            }
            else
            {
                data_M_cumsum = data_M_cumsum_dum
            }
            rm(data_F_cumsum_dum); rm(data_M_cumsum_dum)
            
            # logit transformation
            
            data_F_cumsum_logit = data_M_cumsum_logit = matrix(NA, nrow(data_F), (ncol(data_F) - 1))
            for(ij in 1:nrow(data_F))
            {
                data_F_cumsum_logit[ij,] = logit(data_F_cumsum[ij, 1:(ncol(data_F) - 1)])
                data_M_cumsum_logit[ij,] = logit(data_M_cumsum[ij, 1:(ncol(data_M) - 1)])
                rm(ij)
            }
            
            data_comb = list()
            data_comb[[1]] = t(data_F_cumsum_logit)
            data_comb[[2]] = t(data_M_cumsum_logit)
            
            fore_val = forecast.hdfpca(hdfpca(y = data_comb, order = first_order, r = second_order), h = horizon, fmethod = uni_fore_method)$forecast
            fore_val_F = (fore_val[[1]])[,horizon]
            fore_val_M = (fore_val[[2]])[,horizon]
            
            data_cumsum_logit_fore_add_F = c(invlogit(fore_val_F), 1)
            data_cumsum_logit_fore_add_M = c(invlogit(fore_val_M), 1)
            
            data_cumsum_logit_fore_add_diff_F = c(data_cumsum_logit_fore_add_F[1], diff(data_cumsum_logit_fore_add_F))
            data_cumsum_logit_fore_add_diff_M = c(data_cumsum_logit_fore_add_M[1], diff(data_cumsum_logit_fore_add_M))
            
            forecast_validation_F[,ijk] = data_cumsum_logit_fore_add_diff_F * 10^5
            forecast_validation_M[,ijk] = data_cumsum_logit_fore_add_diff_M * 10^5
            rm(ijk); rm(data_F); rm(data_M); rm(fore_val)
        }
    }
    else
    {
        warning("none, CLR, CDF transformation allowed only.")
    }
    
    # holdout validation data
    
    holdout_validation_F = t(matrix(fdata_F[(n_year - 41 + horizon):(n_year - 20),], length((n_year - 41 + horizon):(n_year - 20)), ncol(fdata_F)))
    holdout_validation_M = t(matrix(fdata_M[(n_year - 41 + horizon):(n_year - 20),], length((n_year - 41 + horizon):(n_year - 20)), ncol(fdata_M)))
    resi_mat_F = holdout_validation_F - forecast_validation_F
    resi_mat_M = holdout_validation_M - forecast_validation_M
    
    # compute standard deviation of residuals
    
    sd_val_F = apply(resi_mat_F, 1, sd)
    sd_val_M = apply(resi_mat_M, 1, sd)
    
    # find the optimal tuning parameter
    
    tune_para_find_val_F_1 = optimise(f = tune_para_find_function, interval = c(0, 1),
                                      resi_mat = resi_mat_F, sd_val_input = sd_val_F,
                                      alpha_level = level_sig)
    
    tune_para_find_val_F_2 = optimise(f = tune_para_find_function, interval = c(0, 5),
                                      resi_mat = resi_mat_F, sd_val_input = sd_val_F,
                                      alpha_level = level_sig)
    
    tune_para_find_val_F_3 = optimise(f = tune_para_find_function, interval = c(0, 10),
                                      resi_mat = resi_mat_F, sd_val_input = sd_val_F,
                                      alpha_level = level_sig)
    
    tune_para_find_val_F_4 = optimise(f = tune_para_find_function, interval = c(0, 20),
                                      resi_mat = resi_mat_F, sd_val_input = sd_val_F,
                                      alpha_level = level_sig)
    
    tune_para_find_val_F_5 = optim(par = 1, fn = tune_para_find_function, lower = 0, method = "L-BFGS-B",
                                   resi_mat = resi_mat_F, sd_val_input = sd_val_F,
                                   alpha_level = level_sig)
    
    tune_para_find_val_F_6 = optim(par = 1, fn = tune_para_find_function, 
                                   resi_mat = resi_mat_F, sd_val_input = sd_val_F,
                                   alpha_level = level_sig)
    
    obj_val = c(tune_para_find_val_F_1$objective, 
                tune_para_find_val_F_2$objective, 
                tune_para_find_val_F_3$objective, 
                tune_para_find_val_F_4$objective, 
                tune_para_find_val_F_5$value, 
                tune_para_find_val_F_6$value)
    obj_val_min_F = min(obj_val)
    
    tune_para_find_F = c(tune_para_find_val_F_1$minimum, 
                         tune_para_find_val_F_2$minimum, 
                         tune_para_find_val_F_3$minimum, 
                         tune_para_find_val_F_4$minimum, 
                         tune_para_find_val_F_5$par,
                         tune_para_find_val_F_6$par)[which.min(obj_val)]
    rm(obj_val)
    
    # male
    
    tune_para_find_val_M_1 = optimise(f = tune_para_find_function, interval = c(0, 1),
                                      resi_mat = resi_mat_M, sd_val_input = sd_val_M,
                                      alpha_level = level_sig)
    
    tune_para_find_val_M_2 = optimise(f = tune_para_find_function, interval = c(0, 5),
                                      resi_mat = resi_mat_M, sd_val_input = sd_val_M,
                                      alpha_level = level_sig)
    
    tune_para_find_val_M_3 = optimise(f = tune_para_find_function, interval = c(0, 10),
                                      resi_mat = resi_mat_M, sd_val_input = sd_val_M,
                                      alpha_level = level_sig)
    
    tune_para_find_val_M_4 = optimise(f = tune_para_find_function, interval = c(0, 20),
                                      resi_mat = resi_mat_M, sd_val_input = sd_val_M,
                                      alpha_level = level_sig)
    
    tune_para_find_val_M_5 = optim(par = 1, fn = tune_para_find_function, lower = 0, method = "L-BFGS-B",
                                   resi_mat = resi_mat_M, sd_val_input = sd_val_M,
                                   alpha_level = level_sig)
    
    tune_para_find_val_M_6 = optim(par = 1, fn = tune_para_find_function, 
                                   resi_mat = resi_mat_M, sd_val_input = sd_val_M,
                                   alpha_level = level_sig)
    
    obj_val = c(tune_para_find_val_M_1$objective, 
                tune_para_find_val_M_2$objective, 
                tune_para_find_val_M_3$objective, 
                tune_para_find_val_M_4$objective, 
                tune_para_find_val_M_5$value, 
                tune_para_find_val_M_6$value)
    obj_val_min_M = min(obj_val)
    
    tune_para_find_M = c(tune_para_find_val_M_1$minimum, 
                         tune_para_find_val_M_2$minimum, 
                         tune_para_find_val_M_3$minimum, 
                         tune_para_find_val_M_4$minimum, 
                         tune_para_find_val_M_5$par,
                         tune_para_find_val_M_6$par)[which.min(obj_val)]
    rm(obj_val)
    
    forecast_test_F = forecast_test_F_lb = forecast_test_F_ub = matrix(NA, ncol(fdata_F), (21 - horizon))
    forecast_test_M = forecast_test_M_lb = forecast_test_M_ub = matrix(NA, ncol(fdata_M), (21 - horizon))
    if(transformation == "direct")
    {
        for(ij in 1:(21 - horizon))
        {
            data_F = fdata_F[1:(n_year - 21 + ij),]
            data_M = fdata_M[1:(n_year - 21 + ij),]
            data_comb = list()
            data_comb[[1]] = t(data_F)
            data_comb[[2]] = t(data_M)
            
            fore_val = forecast.hdfpca(hdfpca(y = data_comb, order = first_order, r = second_order), h = horizon, fmethod = uni_fore_method)$forecast
            forecast_test_F[,ij] = (fore_val[[1]])[,horizon]
            forecast_test_F_lb[,ij] = forecast_test_F[,ij] - tune_para_find_F * sd_val_F
            forecast_test_F_ub[,ij] = forecast_test_F[,ij] + tune_para_find_F * sd_val_F
            
            forecast_test_M[,ij] = (fore_val[[2]])[,horizon]
            forecast_test_M_lb[,ij] = forecast_test_M[,ij] - tune_para_find_M * sd_val_M
            forecast_test_M_ub[,ij] = forecast_test_M[,ij] + tune_para_find_M * sd_val_M
            rm(ij)
        }		
    }
    else if(transformation == "CLR")
    {
        for(ij in 1:(21 - horizon))
        {
            data_F = as.matrix(clr(fdata_F[1:(n_year - 21 + ij),]))
            data_M = as.matrix(clr(fdata_M[1:(n_year - 21 + ij),]))
            data_comb = list()
            data_comb[[1]] = t(data_F)
            data_comb[[2]] = t(data_M)
            
            fore_val = forecast.hdfpca(hdfpca(y = data_comb, order = first_order, r = second_order), h = horizon, fmethod = uni_fore_method)$forecast
            forecast_test_F[,ij] = as.numeric(clrInv((fore_val[[1]])[,horizon])) * 10^5
            forecast_test_F_lb[,ij] = forecast_test_F[,ij] - tune_para_find_F * sd_val_F
            forecast_test_F_ub[,ij] = forecast_test_F[,ij] + tune_para_find_F * sd_val_F
            
            forecast_test_M[,ij] = as.numeric(clrInv((fore_val[[2]])[,horizon])) * 10^5
            forecast_test_M_lb[,ij] = forecast_test_M[,ij] - tune_para_find_M * sd_val_M
            forecast_test_M_ub[,ij] = forecast_test_M[,ij] + tune_para_find_M * sd_val_M
            rm(ij); rm(fore_val)
        }	
    }
    else if(transformation == "CDF")
    {
        for(ijk in 1:(21 - horizon))
        {
            data_F = fdata_F[1:(n_year - 21 + ijk),]/10^5
            data_M = fdata_M[1:(n_year - 21 + ijk),]/10^5
            
            data_F_cumsum_dum = data_M_cumsum_dum = matrix(NA, nrow(data_F), ncol(data_F))
            for(iw in 1:nrow(data_F))
            {
                data_F_cumsum_dum[iw,] = cumsum(data_F[iw,])
                data_M_cumsum_dum[iw,] = cumsum(data_M[iw,])
                rm(iw)
            }
            
            # check if any cumsum values equal to 0
            if(any(data_F_cumsum_dum == 0))
            {
                data_F_cumsum = replace(data_F_cumsum_dum, which(data_F_cumsum_dum == 0), 10^-5)
            }
            else
            {
                data_F_cumsum = data_F_cumsum_dum
            }
            
            if(any(data_M_cumsum_dum == 0))
            {
                data_M_cumsum = replace(data_M_cumsum_dum, which(data_M_cumsum_dum == 0), 10^-5)
            }
            else
            {
                data_M_cumsum = data_M_cumsum_dum
            }
            rm(data_F_cumsum_dum); rm(data_M_cumsum_dum)
            
            # logit transformation
            
            data_F_cumsum_logit = data_M_cumsum_logit = matrix(NA, nrow(data_F), (ncol(data_F) - 1))
            for(ij in 1:nrow(data_F))
            {
                data_F_cumsum_logit[ij,] = logit(data_F_cumsum[ij, 1:(ncol(data_F) - 1)])
                data_M_cumsum_logit[ij,] = logit(data_M_cumsum[ij, 1:(ncol(data_M) - 1)])
                rm(ij)
            }
            
            data_comb = list()
            data_comb[[1]] = t(data_F_cumsum_logit)
            data_comb[[2]] = t(data_M_cumsum_logit)
            
            fore_val = forecast.hdfpca(hdfpca(y = data_comb, order = first_order, r = second_order), h = horizon, fmethod = uni_fore_method)$forecast
            fore_val_F = (fore_val[[1]])[,horizon]
            fore_val_M = (fore_val[[2]])[,horizon]
            
            data_cumsum_logit_fore_add_F = c(invlogit(fore_val_F), 1)
            data_cumsum_logit_fore_add_M = c(invlogit(fore_val_M), 1)
            
            data_cumsum_logit_fore_add_diff_F = c(data_cumsum_logit_fore_add_F[1], diff(data_cumsum_logit_fore_add_F))
            data_cumsum_logit_fore_add_diff_M = c(data_cumsum_logit_fore_add_M[1], diff(data_cumsum_logit_fore_add_M))
            
            forecast_test_F[,ijk] = data_cumsum_logit_fore_add_diff_F * 10^5
            forecast_test_F_lb[,ijk] = forecast_test_F[,ijk] - tune_para_find_F * sd_val_F
            forecast_test_F_ub[,ijk] = forecast_test_F[,ijk] + tune_para_find_F * sd_val_F
            
            forecast_test_M[,ijk] = data_cumsum_logit_fore_add_diff_M * 10^5
            forecast_test_M_lb[,ijk] = forecast_test_M[,ijk] - tune_para_find_M * sd_val_M
            forecast_test_M_ub[,ijk] = forecast_test_M[,ijk] + tune_para_find_M * sd_val_M
            rm(ijk); rm(data_F); rm(data_M)
        }
    }	
    
    # holdout testing data
    
    holdout_val_F = t(matrix(fdata_F[(n_year - 20 + horizon):n_year,], length((n_year - 20 + horizon):n_year), ncol(fdata_F)))
    holdout_val_M = t(matrix(fdata_M[(n_year - 20 + horizon):n_year,], length((n_year - 20 + horizon):n_year), ncol(fdata_M)))
    
    int_F_err = interval_score(holdout = holdout_val_F, lb = forecast_test_F_lb, ub = forecast_test_F_ub, alpha = (1 - level_sig))
    int_M_err = interval_score(holdout = holdout_val_M, lb = forecast_test_M_lb, ub = forecast_test_M_ub, alpha = (1 - level_sig))

    return(list(int_F_err = int_F_err, tune_para_find_F = tune_para_find_F, 
                tune_para_find_F_obj = obj_val_min_F,
                
                int_M_err = int_M_err, tune_para_find_M = tune_para_find_M,
                tune_para_find_M_obj = obj_val_min_M))    
}

interval_fore_subnational_cdf_FANOVA <- function(fdata_F, fdata_M, fore_method, horizon, level_sig, uni_fore_method)
{
    n_year = nrow(fdata_F)
    n_age = ncol(fdata_F)
    forecast_val_F = forecast_val_M = array(NA, dim = c(n_age, (22 - horizon), n_state),
                                            dimnames = list(ages, 1:(22 - horizon), state))
    if(fore_method == "CDF")
    {
        for(ij in 1:(22 - horizon))
        {
            dum <- fore_national_cdf_FANOVA(data_set_F = fdata_F[1:(n_year - 42 + ij),,],
                                            data_set_M = fdata_M[1:(n_year - 42 + ij),,],
                                            fh = horizon, fmethod = uni_fore_method)
            forecast_val_F[,ij,] = dum$female_fore
            forecast_val_M[,ij,] = dum$male_fore
            rm(ij)
        }
    }
    else if(fore_method == "CLR")
    {
        for(ij in 1:(22 - horizon))
        {
            dum <- fore_national_clr_FANOVA(data_set_F = fdata_F[1:(n_year - 42 + ij),,],
                                            data_set_M = fdata_M[1:(n_year - 42 + ij),,],
                                            fh = horizon, fmethod = uni_fore_method)
            forecast_val_F[,ij,] = dum$female_fore
            forecast_val_M[,ij,] = dum$male_fore
            rm(ij)
        }
    }
    else
    {
        warning("transformation should be CDF or CLR transformation.")
    }

    tune_para_find_F = tune_para_find_M = vector("numeric", n_state)
    sd_val_F = sd_val_M = matrix(NA, n_age, n_state)
    for(ik in 1:n_state)
    {
        holdout_validation_F = t(matrix(fdata_F[(n_year - 41 + horizon):(n_year - 20),,ik], length((n_year - 41 + horizon):(n_year - 20)), ncol(fdata_F)))
        holdout_validation_M = t(matrix(fdata_M[(n_year - 41 + horizon):(n_year - 20),,ik], length((n_year - 41 + horizon):(n_year - 20)), ncol(fdata_F)))

        # compute residuals
        
        resi_mat_F = holdout_validation_F - forecast_val_F[,,ik]
        resi_mat_M = holdout_validation_M - forecast_val_M[,,ik]
        
        # compute standard deviation of residuals
        
        sd_val_F[,ik] = apply(resi_mat_F, 1, sd)
        sd_val_M[,ik] = apply(resi_mat_M, 1, sd)
        
        # find the optimal tuning parameters for females
        
        tune_para_find_val_F_1 = optimise(f = tune_para_find_function, interval = c(0, 1),
                                          resi_mat = resi_mat_F, sd_val_input = sd_val_F,
                                          alpha_level = level_sig)
        
        tune_para_find_val_F_2 = optimise(f = tune_para_find_function, interval = c(0, 5),
                                          resi_mat = resi_mat_F, sd_val_input = sd_val_F,
                                          alpha_level = level_sig)
        
        tune_para_find_val_F_3 = optimise(f = tune_para_find_function, interval = c(0, 10),
                                          resi_mat = resi_mat_F, sd_val_input = sd_val_F,
                                          alpha_level = level_sig)
        
        tune_para_find_val_F_4 = optimise(f = tune_para_find_function, interval = c(0, 20),
                                          resi_mat = resi_mat_F, sd_val_input = sd_val_F,
                                          alpha_level = level_sig)
        
        tune_para_find_val_F_5 = optim(par = 1, fn = tune_para_find_function, lower = 0, method = "L-BFGS-B",
                                       resi_mat = resi_mat_F, sd_val_input = sd_val_F,
                                       alpha_level = level_sig)
        
        tune_para_find_val_F_6 = optim(par = 1, fn = tune_para_find_function,
                                       resi_mat = resi_mat_F, sd_val_input = sd_val_F,
                                       alpha_level = level_sig)
        
        obj_val = c(tune_para_find_val_F_1$objective,
                    tune_para_find_val_F_2$objective,
                    tune_para_find_val_F_3$objective,
                    tune_para_find_val_F_4$objective,
                    tune_para_find_val_F_5$value,
                    tune_para_find_val_F_6$value)
        obj_val_min_F = min(obj_val)
        
        tune_para_find_F[ik] = c(tune_para_find_val_F_1$minimum,
                                 tune_para_find_val_F_2$minimum,
                                 tune_para_find_val_F_3$minimum,
                                 tune_para_find_val_F_4$minimum,
                                 tune_para_find_val_F_5$par,
                                 tune_para_find_val_F_6$par)[which.min(obj_val)]
        rm(obj_val)
        
        # find the optimal tuning parameters for males
        
        tune_para_find_val_M_1 = optimise(f = tune_para_find_function, interval = c(0, 1),
                                          resi_mat = resi_mat_M, sd_val_input = sd_val_M,
                                          alpha_level = level_sig)
        
        tune_para_find_val_M_2 = optimise(f = tune_para_find_function, interval = c(0, 5),
                                          resi_mat = resi_mat_M, sd_val_input = sd_val_M,
                                          alpha_level = level_sig)
        
        tune_para_find_val_M_3 = optimise(f = tune_para_find_function, interval = c(0, 10),
                                          resi_mat = resi_mat_M, sd_val_input = sd_val_M,
                                          alpha_level = level_sig)
        
        tune_para_find_val_M_4 = optimise(f = tune_para_find_function, interval = c(0, 20),
                                          resi_mat = resi_mat_M, sd_val_input = sd_val_M,
                                          alpha_level = level_sig)
        
        tune_para_find_val_M_5 = optim(par = 1, fn = tune_para_find_function, lower = 0, method = "L-BFGS-B",
                                       resi_mat = resi_mat_M, sd_val_input = sd_val_M,
                                       alpha_level = level_sig)
        
        tune_para_find_val_M_6 = optim(par = 1, fn = tune_para_find_function,
                                       resi_mat = resi_mat_M, sd_val_input = sd_val_M,
                                       alpha_level = level_sig)
        
        obj_val = c(tune_para_find_val_M_1$objective,
                    tune_para_find_val_M_2$objective,
                    tune_para_find_val_M_3$objective,
                    tune_para_find_val_M_4$objective,
                    tune_para_find_val_M_5$value,
                    tune_para_find_val_M_6$value)
        obj_val_min_M = min(obj_val)
        
        tune_para_find_M[ik] = c(tune_para_find_val_M_1$minimum,
                                 tune_para_find_val_M_2$minimum,
                                 tune_para_find_val_M_3$minimum,
                                 tune_para_find_val_M_4$minimum,
                                 tune_para_find_val_M_5$par,
                                 tune_para_find_val_M_6$par)[which.min(obj_val)]
        rm(ik); rm(obj_val); rm(holdout_validation_F); rm(holdout_validation_M)
    }

    forecast_test_F = forecast_test_F_lb = forecast_test_F_ub =
    forecast_test_M = forecast_test_M_lb = forecast_test_M_ub = array(NA, dim = c(n_age, (21 - horizon), n_state),
                                                dimnames = list(ages, 1:(21 - horizon), state))
    int_F_err = int_M_err = matrix(NA, n_state, 3)
    if(fore_method == "CDF")
    {
        for(ij in 1:(21 - horizon))
        {
            dum <- fore_national_cdf_FANOVA(data_set_F = fdata_F[1:(n_year - 21 + ij),,],
                                            data_set_M = fdata_M[1:(n_year - 21 + ij),,],
                                            fh = horizon, fmethod = uni_fore_method)
            forecast_test_F[,ij,] = dum$female_fore
            forecast_test_M[,ij,] = dum$male_fore
            rm(ij); rm(dum)
        }
      
        for(ij in 1:(21 - horizon))
        {
            for(ik in 1:n_state)
            {
                forecast_test_F_lb[,ij,ik] = forecast_test_F[,ij,ik] - tune_para_find_F[ik] * sd_val_F[,ik]
                forecast_test_F_ub[,ij,ik] = forecast_test_F[,ij,ik] + tune_para_find_F[ik] * sd_val_F[,ik]
                    
                forecast_test_M_lb[,ij,ik] = forecast_test_M[,ij,ik] - tune_para_find_M[ik] * sd_val_M[,ik]
                forecast_test_M_ub[,ij,ik] = forecast_test_M[,ij,ik] + tune_para_find_M[ik] * sd_val_M[,ik]
            }
        }
      
        # holdout testing data
        
        for(ik in 1:n_state)
        {
            holdout_val_F = t(matrix(fdata_F[(n_year - 20 + horizon):n_year,,ik], length((n_year - 20 + horizon):n_year), ncol(fdata_F)))
            holdout_val_M = t(matrix(fdata_M[(n_year - 20 + horizon):n_year,,ik], length((n_year - 20 + horizon):n_year), ncol(fdata_M)))
            
            int_F_err[ik,] = interval_score(holdout = holdout_val_F, lb = forecast_test_F_lb[,,ik], ub = forecast_test_F_ub[,,ik], alpha = (1 - level_sig))
            int_M_err[ik,] = interval_score(holdout = holdout_val_M, lb = forecast_test_M_lb[,,ik], ub = forecast_test_M_ub[,,ik], alpha = (1 - level_sig))
            print(ik); rm(ik)
        }
        colnames(int_F_err) = colnames(int_M_err) = c("ECP", "CPD", "MIS")
        rownames(int_F_err) = rownames(int_M_err) = state
    }
    else if(fore_method == "CLR")
    {
        for(ij in 1:(21 - horizon))
        {
            dum <- fore_national_clr_FANOVA(data_set_F = fdata_F[1:(n_year - 21 + ij),,],
                                            data_set_M = fdata_M[1:(n_year - 21 + ij),,],
                                            fh = horizon, fmethod = uni_fore_method)
            forecast_test_F[,ij,] = dum$female_fore
            forecast_test_M[,ij,] = dum$male_fore
            rm(ij); rm(dum)
        }
        for(ij in 1:(21 - horizon))
        {
            for(ik in 1:n_state)
            {
                forecast_test_F_lb[,ij,ik] = forecast_test_F[,ij,ik] - tune_para_find_F[ik] * sd_val_F[,ik]
                forecast_test_F_ub[,ij,ik] = forecast_test_F[,ij,ik] + tune_para_find_F[ik] * sd_val_F[,ik]
                    
                forecast_test_M_lb[,ij,ik] = forecast_test_M[,ij,ik] - tune_para_find_M[ik] * sd_val_M[,ik]
                forecast_test_M_ub[,ij,ik] = forecast_test_M[,ij,ik] + tune_para_find_M[ik] * sd_val_M[,ik]
            }
        }
      
        # holdout testing data
        
        for(ik in 1:n_state)
        {
            holdout_val_F = t(matrix(fdata_F[(n_year - 20 + horizon):n_year,,ik], length((n_year - 20 + horizon):n_year), ncol(fdata_F)))
            holdout_val_M = t(matrix(fdata_M[(n_year - 20 + horizon):n_year,,ik], length((n_year - 20 + horizon):n_year), ncol(fdata_M)))
            
            int_F_err[ik,] = interval_score(holdout = holdout_val_F, lb = forecast_test_F_lb[,,ik], ub = forecast_test_F_ub[,,ik], alpha = (1 - level_sig))
            int_M_err[ik,] = interval_score(holdout = holdout_val_M, lb = forecast_test_M_lb[,,ik], ub = forecast_test_M_ub[,,ik], alpha = (1 - level_sig))
            rm(ik)
        }
        colnames(int_F_err) = colnames(int_M_err) = c("ECP", "CPD", "MIS")
        rownames(int_F_err) = rownames(int_M_err) = state
    }
    else
    {
        warning("transformation should be CDF or CLR transformation.")
    }
    
    return(list(int_F_err = int_F_err, tune_para_find_F = tune_para_find_F,
                tune_para_find_F_obj = obj_val_min_F,
                int_M_err = int_M_err, tune_para_find_M = tune_para_find_M,
                tune_para_find_M_obj = obj_val_min_M))
}

# fdata: functional data n by p
# method_ncomp: EVR or K = 6
# horizon: forecast horizon; when h = 1, there are 21 years; when h = 20, there are 2 years
# fore_method: transformation
# uni_fore_method: forecasting method
# level_sig: level of significance

interval_fore_subnational_cdf <- function(fdata, method_ncomp, horizon, fore_method, 
                                          uni_fore_method, level_sig)
{
    n_year = nrow(fdata)
    n_age = ncol(fdata)
    fore_validation = matrix(NA, ncol(fdata), (22 - horizon))
    if(fore_method == "CDF")
    {
        for(ij in 1:(22 - horizon))
        {
            fore_validation[,ij] = fore_national_cdf(data_set = fdata[1:(n_year - 42 + ij),], 
                                                     ncomp_method = method_ncomp,
                                                     fh = horizon, fmethod = uni_fore_method)
            rm(ij)            
        }
    }
    else if(fore_method == "CLR")
    {
        for(ij in 1:(22 - horizon))
        {
            fore_validation[,ij] = as.numeric(clr_fun(fdata = fdata[1:(n_year - 42 + ij),], 
                                                      ncomp_selection = method_ncomp,
                                                      fh = horizon, fore_method = uni_fore_method)$fore_count)
            rm(ij)
        }
    }
    else
    {
        warning("forecasting method must either be CDF or CLR.")
    }
    
    # holdout validation data
    
    holdout_validation_dum = t(matrix(fdata[(n_year - 41 + horizon):(n_year - 20),], length((n_year - 41 + horizon):(n_year - 20)), ncol(fdata)))
    resi_mat = holdout_validation_dum - fore_validation
    
    # compute standard deviation of residuals (require at least two years of observations)
    
    sd_val_input = apply(resi_mat, 1, sd)
    
    # find the optimal tuning parameter
    
    tune_para_find_val_1 = optimise(f = tune_para_find_function, interval = c(0, 1), 
                                    resi_mat = resi_mat, sd_val_input = sd_val_input, 
                                    alpha_level = level_sig)
    
    tune_para_find_val_2 = optimise(f = tune_para_find_function, interval = c(0, 5), 
                                    resi_mat = resi_mat, sd_val_input = sd_val_input, 
                                    alpha_level = level_sig)
    
    tune_para_find_val_3 = optimise(f = tune_para_find_function, interval = c(0, 10), 
                                    resi_mat = resi_mat, sd_val_input = sd_val_input, 
                                    alpha_level = level_sig)
    
    tune_para_find_val_4 = optimise(f = tune_para_find_function, interval = c(0, 20), 
                                    resi_mat = resi_mat, sd_val_input = sd_val_input, 
                                    alpha_level = level_sig)
    
    tune_para_find_val_5 = optim(par = 1, fn = tune_para_find_function, lower = 0, method = "L-BFGS-B", 
                                 resi_mat = resi_mat, sd_val_input = sd_val_input, 
                                 alpha_level = level_sig)
    
    tune_para_find_val_6 = optim(par = 1, fn = tune_para_find_function, method = "Nelder-Mead",
                                 resi_mat = resi_mat, sd_val_input = sd_val_input, 
                                 alpha_level = level_sig)
    
    obj_val = c(tune_para_find_val_1$objective, 
                tune_para_find_val_2$objective, 
                tune_para_find_val_3$objective,
                tune_para_find_val_4$objective,
                tune_para_find_val_5$value,
                tune_para_find_val_6$value)
    obj_val_min = min(obj_val)
    
    tune_para_find = c(tune_para_find_val_1$minimum, 
                       tune_para_find_val_2$minimum,
                       tune_para_find_val_3$minimum,
                       tune_para_find_val_4$minimum,
                       tune_para_find_val_5$par,
                       tune_para_find_val_6$par)[which.min(obj_val)]
    
    fore_val = fore_val_lb = fore_val_ub = matrix(NA, ncol(fdata), (21 - horizon))
    if(fore_method == "CDF")
    {
        for(ij in 1:(21 - horizon))
        {
            fore_val[,ij] <- fore_national_cdf(data_set = fdata[1:(n_year - 21 + ij),], ncomp_method = method_ncomp,
                                               fh = horizon, fmethod = uni_fore_method)
            fore_val_lb[,ij] = fore_val[,ij] - tune_para_find * sd_val_input
            fore_val_ub[,ij] = fore_val[,ij] + tune_para_find * sd_val_input
            rm(ij)
        }
    }
    else if(fore_method == "CLR")
    {
        for(ij in 1:(21 - horizon))
        {
            fore_val[,ij] = as.numeric(clr_fun(fdata = fdata[1:(n_year - 21 + ij),], ncomp_selection = method_ncomp,
                                               fh = horizon, fore_method = uni_fore_method)$fore_count)
            fore_val_lb[,ij] = fore_val[,ij] - tune_para_find * sd_val_input
            fore_val_ub[,ij] = fore_val[,ij] + tune_para_find * sd_val_input
            rm(ij)
        }
    }
    
    # holdout testing data
    
    holdout_val_dum = t(matrix(fdata[(n_year - 20 + horizon):n_year,], length((n_year - 20 + horizon):n_year), ncol(fdata)))
    
    int_err = interval_score(holdout = holdout_val_dum, lb = fore_val_lb, ub = fore_val_ub, alpha = (1 - level_sig))
    return(list(int_err = int_err, tune_para_find = tune_para_find, tune_para_find_obj = obj_val_min))
}


# fdata_F: female data
# fdata_M: male data
# fdata_common: common data shared by both populations
# fore_method: CDF or CLR transformation
# horizon: forecast horizon
# way_ncomp: EVR or K = 6
# level_sig: level of significance

interval_fore_subnational_cdf_MLFTS <- function(fdata_F, fdata_M, fdata_common, fore_method, horizon, 
                                                way_ncomp, level_sig, uni_fore_method)
{
  n_year = nrow(fdata_F)
  n_age = ncol(fdata_F)
  forecast_validation_F = forecast_validation_M = matrix(NA, ncol(fdata_F), (22 - horizon))
  if(fore_method == "CDF")
  {
    for(ij in 1:(22 - horizon))
    {
      dum <- fore_national_cdf_MLFTS(data_set_F = fdata_F[1:(n_year - 42 + ij),], 
                                     data_set_M = fdata_M[1:(n_year - 42 + ij),],
                                     aux_variable = fdata_common[1:(n_year - 42 + ij),], 
                                     fh = horizon, fmethod = uni_fore_method,
                                     method_ncomp = way_ncomp)
      forecast_validation_F[,ij] = dum$mlfts_fore_F
      forecast_validation_M[,ij] = dum$mlfts_fore_M
      rm(ij); rm(dum)
    }
  }
  else if(fore_method == "CLR")
  {
    for(ij in 1:(22 - horizon))
    {
      dum = clr_MLFTS_fun(fdata_F = fdata_F[1:(n_year - 42 + ij),], 
                          fdata_M = fdata_M[1:(n_year - 42 + ij),],
                          ncomp_selection = way_ncomp, fh = horizon,
                          uni_fore_method = uni_fore_method)
      forecast_validation_F[,ij] = dum$MLFTS_res_fore_F
      forecast_validation_M[,ij] = dum$MLFTS_res_fore_M
      rm(ij); rm(dum)
    }
  }
  else
  {
    warning("Forecasting method must either be CDF or CLR.")
  }
  rownames(forecast_validation_F) = rownames(forecast_validation_M) = 1:ncol(fdata_F)
  colnames(forecast_validation_F) = colnames(forecast_validation_M) = 1:(22 - horizon)
  
  # holdout validation data
  
  holdout_validation_F = t(matrix(fdata_F[(n_year - 41 + horizon):(n_year - 20),], length((n_year - 41 + horizon):(n_year - 20)), ncol(fdata_F)))
  holdout_validation_M = t(matrix(fdata_M[(n_year - 41 + horizon):(n_year - 20),], length((n_year - 41 + horizon):(n_year - 20)), ncol(fdata_M)))
  resi_mat_F = holdout_validation_F - forecast_validation_F
  resi_mat_M = holdout_validation_M - forecast_validation_M
  
  # compute standard deviation of residuals
  
  sd_val_F = apply(resi_mat_F, 1, sd)
  sd_val_M = apply(resi_mat_M, 1, sd)
  
  # find the optimal tuning parameter for the female data
  
  tune_para_find_val_F_1 = optimise(f = tune_para_find_function, interval = c(0, 1),
                                    resi_mat = resi_mat_F, sd_val_input = sd_val_F,
                                    alpha_level = level_sig)
  
  tune_para_find_val_F_2 = optimise(f = tune_para_find_function, interval = c(0, 5),
                                    resi_mat = resi_mat_F, sd_val_input = sd_val_F,
                                    alpha_level = level_sig)
  
  tune_para_find_val_F_3 = optimise(f = tune_para_find_function, interval = c(0, 10),
                                    resi_mat = resi_mat_F, sd_val_input = sd_val_F,
                                    alpha_level = level_sig)
  
  tune_para_find_val_F_4 = optimise(f = tune_para_find_function, interval = c(0, 20),
                                    resi_mat = resi_mat_F, sd_val_input = sd_val_F,
                                    alpha_level = level_sig)
  
  tune_para_find_val_F_5 = optim(par = 1, fn = tune_para_find_function, lower = 0, method = "L-BFGS-B",
                                 resi_mat = resi_mat_F, sd_val_input = sd_val_F,
                                 alpha_level = level_sig)
  
  tune_para_find_val_F_6 = optim(par = 1, fn = tune_para_find_function,
                                 resi_mat = resi_mat_F, sd_val_input = sd_val_F,
                                 alpha_level = level_sig)
  
  obj_val = c(tune_para_find_val_F_1$objective, 
              tune_para_find_val_F_2$objective, 
              tune_para_find_val_F_3$objective, 
              tune_para_find_val_F_4$objective, 
              tune_para_find_val_F_5$value, 
              tune_para_find_val_F_6$value)
  obj_val_min_F = min(obj_val)
  
  tune_para_find_F = c(tune_para_find_val_F_1$minimum, 
                       tune_para_find_val_F_2$minimum,
                       tune_para_find_val_F_3$minimum,
                       tune_para_find_val_F_4$minimum,
                       tune_para_find_val_F_5$par,
                       tune_para_find_val_F_6$par)[which.min(obj_val)]
  rm(obj_val)
  
  # find the optimal tuning parameter for the male data
  
  tune_para_find_val_M_1 = optimise(f = tune_para_find_function, interval = c(0, 1),
                                    resi_mat = resi_mat_M, sd_val_input = sd_val_M,
                                    alpha_level = level_sig)
  
  tune_para_find_val_M_2 = optimise(f = tune_para_find_function, interval = c(0, 5),
                                    resi_mat = resi_mat_M, sd_val_input = sd_val_M,
                                    alpha_level = level_sig)
  
  tune_para_find_val_M_3 = optimise(f = tune_para_find_function, interval = c(0, 10),
                                    resi_mat = resi_mat_M, sd_val_input = sd_val_M,
                                    alpha_level = level_sig)
  
  tune_para_find_val_M_4 = optimise(f = tune_para_find_function, interval = c(0, 20),
                                    resi_mat = resi_mat_M, sd_val_input = sd_val_M,
                                    alpha_level = level_sig)
  
  tune_para_find_val_M_5 = optim(par = 1, fn = tune_para_find_function, lower = 0, method = "L-BFGS-B",
                                 resi_mat = resi_mat_M, sd_val_input = sd_val_M,
                                 alpha_level = level_sig)
  
  tune_para_find_val_M_6 = optim(par = 1, fn = tune_para_find_function, 
                                 resi_mat = resi_mat_M, sd_val_input = sd_val_M,
                                 alpha_level = level_sig)
  
  obj_val = c(tune_para_find_val_M_1$objective, 
              tune_para_find_val_M_2$objective, 
              tune_para_find_val_M_3$objective, 
              tune_para_find_val_M_4$objective, 
              tune_para_find_val_M_5$value, 
              tune_para_find_val_M_6$value)
  obj_val_min_M = min(obj_val)
  
  tune_para_find_M = c(tune_para_find_val_M_1$minimum, 
                       tune_para_find_val_M_2$minimum, 
                       tune_para_find_val_M_3$minimum, 
                       tune_para_find_val_M_4$minimum, 
                       tune_para_find_val_M_5$par,
                       tune_para_find_val_M_6$par)[which.min(obj_val)]
  rm(obj_val); rm(holdout_validation_F); rm(holdout_validation_M)
  
  forecast_test_F = forecast_test_F_lb = forecast_test_F_ub = matrix(NA, ncol(fdata_F), (21 - horizon))
  forecast_test_M = forecast_test_M_lb = forecast_test_M_ub = matrix(NA, ncol(fdata_M), (21 - horizon))
  if(fore_method == "CDF")
  {
    for(ij in 1:(21 - horizon))
    {
      dum <- fore_national_cdf_MLFTS(data_set_F = fdata_F[1:(n_year - 21 + ij),], 
                                     data_set_M = fdata_M[1:(n_year - 21 + ij),],
                                     aux_variable = fdata_common[1:(n_year - 21 + ij),], 
                                     fh = horizon, fmethod = uni_fore_method,
                                     method_ncomp = way_ncomp)
      forecast_test_F[,ij] = dum$mlfts_fore_F
      forecast_test_F_lb[,ij] = forecast_test_F[,ij] - tune_para_find_F * sd_val_F
      forecast_test_F_ub[,ij] = forecast_test_F[,ij] + tune_para_find_F * sd_val_F
      
      forecast_test_M[,ij] = dum$mlfts_fore_M
      forecast_test_M_lb[,ij] = forecast_test_M[,ij] - tune_para_find_M * sd_val_M
      forecast_test_M_ub[,ij] = forecast_test_M[,ij] + tune_para_find_M * sd_val_M
      rm(ij); rm(dum)
    }
  }
  else if(fore_method == "CLR")
  {
    for(ij in 1:(21 - horizon))
    {
      dum = clr_MLFTS_fun(fdata_F = fdata_F[1:(n_year - 21 + ij),], 
                          fdata_M = fdata_M[1:(n_year - 21 + ij),],
                          ncomp_selection = way_ncomp, fh = horizon,
                          uni_fore_method = uni_fore_method)
      forecast_test_F[,ij] = dum$MLFTS_res_fore_F
      forecast_test_F_lb[,ij] = forecast_test_F[,ij] - tune_para_find_F * sd_val_F
      forecast_test_F_ub[,ij] = forecast_test_F[,ij] + tune_para_find_F * sd_val_F
      
      forecast_test_M[,ij] = dum$MLFTS_res_fore_M
      forecast_test_M_lb[,ij] = forecast_test_M[,ij] - tune_para_find_M * sd_val_M
      forecast_test_M_ub[,ij] = forecast_test_M[,ij] + tune_para_find_M * sd_val_M
      rm(ij); rm(dum)
    }
  }
  
  # holdout testing data
  
  holdout_val_F = t(matrix(fdata_F[(n_year - 20 + horizon):n_year,], length((n_year - 20 + horizon):n_year), ncol(fdata_F)))
  holdout_val_M = t(matrix(fdata_M[(n_year - 20 + horizon):n_year,], length((n_year - 20 + horizon):n_year), ncol(fdata_M)))
  
  int_F_err = interval_score(holdout = holdout_val_F, lb = forecast_test_F_lb, ub = forecast_test_F_ub, alpha = (1 - level_sig))
  int_M_err = interval_score(holdout = holdout_val_M, lb = forecast_test_M_lb, ub = forecast_test_M_ub, alpha = (1 - level_sig))
  
  return(list(int_F_err = int_F_err, tune_para_find_F = tune_para_find_F,
              tune_para_find_F_obj = obj_val_min_F,
              int_M_err = int_M_err, tune_para_find_M = tune_para_find_M,
              tune_para_find_M_obj = obj_val_min_M))
}

# subnational data for handling zero counts

replace_zero <- function(x) 
{
  if(any(x == 0)) cmultRepl(x, method = "CZM") else x
}
