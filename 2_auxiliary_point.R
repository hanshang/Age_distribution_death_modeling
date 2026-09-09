####################################
# univariate functional time series 
####################################

# data_set: life-table death counts with radix of 10^5 (n by p data matrix)
# ncomp_method: way of selecting the number of components, EVR or K = 6
# fh: forecast horizon
# fmethod: ets (we also compare auto.arima, but prefer ets)

fore_national_cdf <- function(data_set, ncomp_method, fh, fmethod)
{
    data = data_set/10^5
    data_cumsum_dum = matrix(NA, nrow(data), ncol(data))
    for(ij in 1:nrow(data))
    {
        data_cumsum_dum[ij,] = cumsum(data[ij,])
        rm(ij)
    }
    
    # check if any cumsum values equal to 0
    
    if(any(data_cumsum_dum == 0))
    {
        data_cumsum = replace(data_cumsum_dum, which(data_cumsum_dum == 0), 10^-5)
    }
    else
    {
        data_cumsum = data_cumsum_dum
    }
    rm(data_cumsum_dum)
    
    # logit transformation
    
    data_cumsum_logit = matrix(NA, nrow(data), (ncol(data) - 1))
    for(ij in 1:nrow(data))
    {
        data_cumsum_logit[ij,] = logit(data_cumsum[ij, 1:(ncol(data) - 1)])
        rm(ij)
    }
    rm(data_cumsum)
    rownames(data_cumsum_logit) = years[1:nrow(data)]
    
    # fitting a functional time series forecasting method
    
    if(ncomp_method == "EVR")
    {
        ncomp = select_K(tau = 10^-3, eigenvalue = (svd(data_cumsum_logit)$d)^2)
    }
    else if(ncomp_method == "provide")
    {
        ncomp = 6
    }
    else
    {
        warning("The number of components is required.")
    }
    data_cumsum_logit_fore = forecast(ftsm(fts(ages[1:110], t(data_cumsum_logit)), order = ncomp), h = fh,
                                      method = fmethod)
    
    # h-step-ahead forecast
    
    data_cumsum_logit_fore_add = c(invlogit(data_cumsum_logit_fore$mean$y[,fh]), 1)
    data_cumsum_logit_fore_add_diff = c(data_cumsum_logit_fore_add[1], diff(data_cumsum_logit_fore_add))
    return(data_cumsum_logit_fore_add_diff * 10^5)
}

# selecting the number of components
# tau: a tuning parameter
# eigenvalue: estimated eigenvalues

select_K <- function(tau, eigenvalue)
{
    k_max = length(eigenvalue)
    k_all = rep(0, k_max-1)
    for(k in 1:(k_max-1))
    {
      k_all[k] = (eigenvalue[k+1]/eigenvalue[k])*ifelse(eigenvalue[k]/eigenvalue[1] > tau, 1, 0) + ifelse(eigenvalue[k]/eigenvalue[1] < tau, 1, 0)
    }
    K_hat = which.min(k_all)
    return(K_hat)
}


######################################
# multivariate functional time series
######################################

# data_input: multivariate functional time series
# ncomp_method: way of selecting the number of components
# fh: forecast horizon
# fore_method: forecasting method
# object_interest: point or interval forecast accuracy
# boot_number: number of bootstrap samples
# PI_level: nominal coverage probability

MFTS_model <- function(data_input, ncomp_method, fh, fore_method)
{
  n_age  = dim(data_input)[1]
  n_year = dim(data_input)[2]
  n_pop  = dim(data_input)[3]
  
  # clean the data
  
  data_set_array = array(NA, dim = c(n_age, n_year, n_pop))
  if(any(!is.finite(data_input)))
  {
    for(iw in 1:n_pop)
    {
      for(ij in 1:n_age)
      {
        data_set_array[ij,,iw] = na.interp(data_input[ij,,iw])
      }
    }
  }
  else
  {
    data_set_array = data_input
  }
  
  rowmeans_object = sd_object = decenter_object = list()
  for(ik in 1:n_pop)
  {
    # compute mean and sd function
    rowmeans_object[[ik]] = rowMeans(data_set_array[,,ik], na.rm = TRUE)
    sd_object[[ik]] = apply(data_set_array[,,ik], 1, sd, na.rm = TRUE)
    
    # de-center functional data
    decenter_object[[ik]] = t(scale(t(data_set_array[,,ik]), center = TRUE, scale = TRUE))
  }
  
  comb_object = do.call(rbind, decenter_object)
  # comb_object = do.call(rbind, comb_object_raw)
  colnames(comb_object) = 1:ncol(comb_object)
  
  eigen_value = eigen(cov(t(comb_object)))$values
  if(ncomp_method == "EVR")
  {
    ncomp = select_K(tau = 10^-2, eigenvalue = eigen_value)
  }
  else if(ncomp_method == "provide")
  {
    ncomp = 6
  }
  else
  {
    warning("The number of components is required.")
  }
  fore_ftsm = forecast(ftsm(fts(1:nrow(comb_object), comb_object), order = ncomp), h = fh, 
                       method = fore_method)
  res_fore = as.matrix(fore_ftsm$mean$y[,fh] * do.call(c, sd_object) + do.call(c, rowmeans_object))
  #res_fore = as.matrix(fore_ftsm$mean$y[,fh])
  return(res_fore)
}

####################################################
# Multivariate functional time series decomposition
####################################################

# data_F: female data
# data_M: male data
# fh: forecast horizon
# fmethod: forecasting method
# object_interest: point or interval forecasts
# alpha: level of significance
# method_ncomp: K = 6 or fixed

fore_national_cdf_MFTS <- function(data_set_F, data_set_M, fh, fmethod, method_ncomp)
{
  data_F = data_set_F/10^5
  data_M = data_set_M/10^5
  
  data_cumsum_dum_F = data_cumsum_dum_M = matrix(NA, nrow(data_F), ncol(data_F))
  for(ij in 1:nrow(data_F))
  {
    data_cumsum_dum_F[ij,] = cumsum(data_F[ij,])
    data_cumsum_dum_M[ij,] = cumsum(data_M[ij,])
    rm(ij)
  }
  
  if(any(data_cumsum_dum_F == 0))
  {
    data_cumsum_F = replace(data_cumsum_dum_F, which(data_cumsum_dum_F == 0), 10^-5)
  }
  else
  {
    data_cumsum_F = data_cumsum_dum_F
  }
  if(any(data_cumsum_dum_M == 0))
  {
    data_cumsum_M = replace(data_cumsum_dum_M, which(data_cumsum_dum_M == 0), 10^-5)
  }
  else
  {
    data_cumsum_M = data_cumsum_dum_M
  }
  rm(data_cumsum_dum_F); rm(data_cumsum_dum_M)
  
  data_cumsum_logit_F = data_cumsum_logit_M = matrix(NA, nrow(data_F), (ncol(data_F) - 1))
  for(ij in 1:nrow(data_F))
  {
    data_cumsum_logit_F[ij,] = logit(data_cumsum_F[ij, 1:(ncol(data_F) - 1)])
    data_cumsum_logit_M[ij,] = logit(data_cumsum_M[ij, 1:(ncol(data_M) - 1)])
    rm(ij)
  }
  rownames(data_cumsum_logit_F) = rownames(data_cumsum_logit_M) = years[1:nrow(data_F)]
  
  data_comb = array(NA, dim = c((ncol(data_F) - 1), nrow(data_F), 2))
  data_comb[,,1] = t(data_cumsum_logit_F)
  data_comb[,,2] = t(data_cumsum_logit_M)
  
  dum = MFTS_model(data_input = data_comb, ncomp_method = method_ncomp, fh = fh, fore_method = fmethod)
  
  data_cumsum_logit_F_fore = dum[1:(ncol(data_F) - 1),]
  data_cumsum_logit_M_fore = dum[ncol(data_F):(2 * (ncol(data_F) - 1)),]
  rm(dum)
  
  data_cumsum_logit_F_fore_add = c(invlogit(data_cumsum_logit_F_fore), 1)
  data_cumsum_logit_M_fore_add = c(invlogit(data_cumsum_logit_M_fore), 1)
  
  data_cumsum_logit_F_fore_add_diff = c(data_cumsum_logit_F_fore_add[1], diff(data_cumsum_logit_F_fore_add))
  data_cumsum_logit_M_fore_add_diff = c(data_cumsum_logit_M_fore_add[1], diff(data_cumsum_logit_M_fore_add))
  return(list(mfts_fore_F = data_cumsum_logit_F_fore_add_diff * 10^5, 
              mfts_fore_M = data_cumsum_logit_M_fore_add_diff * 10^5))
}

##########################################
# Multilevel functional time series model
##########################################

# data_set: a list of p by n data matrix
# aux_var: an aggregated p by n data matrix
# ncomp_method: method for selecting the number of components
# fh: forecast horizon
# fore_method: univariate time-series forecasting method, such as "ETS"

MLFTS_model <- function(data_input, aux_var, ncomp_method, fh, fore_method)
{
  n_age  = dim(data_input)[1]
  n_year = dim(data_input)[2]
  n_pop  = dim(data_input)[3]
  
  # clean the data
  
  data_set_array = array(NA, dim = c(n_age, n_year, n_pop))
  if(any(!is.finite(data_input)))
  {
    for(iw in 1:n_pop)
    {
      for(ij in 1:n_age)
      {
        data_set_array[ij,,iw] = na.interp(data_input[ij,,iw])
      }
    }
  }
  else
  {
    data_set_array = data_input
  }
  
  # compute the mean function
  
  mean_function_list = list()
  for(ik in 1:n_pop)
  {
    mean_function_list[[ik]] = rowMeans(data_set_array[,,ik], na.rm = TRUE)
    rm(ik)
  }
  
  data_set = array(NA, dim = c(n_age, n_year, n_pop))
  for(ik in 1:n_pop)
  {
    data_set[,,ik] = t(scale(t(data_set_array[,,ik]), center = TRUE, scale = FALSE))
    rm(ik)
  }
  
  if(missing(aux_var)|is.null(aux_var))
  {
    aggregate_data = apply(data_set, c(1, 2), mean)
  }
  else
  {
    aggregate_data = t(aux_var)
  }
  colnames(aggregate_data) = 1:n_year
  rownames(aggregate_data) = 1:n_age
  
  # 1st FPCA
  
  eigen_value_aggregate = eigen(cov(t(aggregate_data)))$values
  if(ncomp_method == "EVR")
  {
    ncomp_aggregate = select_K(tau = 10^-3, eigenvalue = eigen_value_aggregate)
  }
  else if(ncomp_method == "provide")
  {
    ncomp_aggregate = 6
  }
  ftsm_aggregate = ftsm(fts(1:n_age, aggregate_data), order = ncomp_aggregate)
  
  # calculate sum of lambda_k
  sum_lambda_k = sum(eigen_value_aggregate[1:ncomp_aggregate])
  
  # compute the residual trend
  data_residual = array(NA, dim = c(n_age, n_year, n_pop))
  for(iw in 1:n_pop)
  {
    data_residual[,,iw] = data_set[,,iw] - ftsm_aggregate$fitted$y
    colnames(data_residual[,,iw]) = 1:n_year
    rownames(data_residual[,,iw]) = 1:n_age
    rm(iw)
  }
  
  # 2nd FPCA
  
  if(ncomp_method == "EVR")
  {
    ncomp_resi = vector("numeric", n_pop)
    for(iw in 1:n_pop)
    {
      eigen_value_resi = eigen(cov(t(data_residual[,,iw])))$values
      ncomp_resi[iw] = select_K(tau = 10^-3, eigenvalue = eigen_value_resi)
    }
  }
  else if(ncomp_method == "provide")
  {
    ncomp_resi = rep(6, n_pop)
  }
  
  sum_lambda_l = vector("numeric", n_pop)
  for(iw in 1:n_pop)
  {
    eigen_value_resi = eigen(cov(t(data_residual[,,iw])))$values
    
    # calculate sum of lambda_l
    sum_lambda_l[iw] = sum(eigen_value_resi[1:(ncomp_resi[iw])])
  }
  
  ftsm_resi = list()
  for(iw in 1:n_pop)
  {
    ftsm_resi[[iw]] = ftsm(fts(1:n_age, data_residual[,,iw]), order = ncomp_resi[iw])
    rm(iw)
  }
  
  # within-cluster variability
  
  within_cluster_variability = vector("numeric", n_pop)
  for(iw in 1:n_pop)
  {
    within_cluster_variability[iw] = sum_lambda_k/(sum_lambda_k + sum_lambda_l[iw])
  }
  
  # reconstruction
  
  coef_fore = matrix(NA, ncomp_aggregate, fh)
  if(fore_method == "arima")
  {
    for(ik in 1:ncomp_aggregate)
    {
      coef_fore[ik,] = forecast(auto.arima(ftsm_aggregate$coeff[,ik+1]), h = fh)$mean
    }
  }
  else if(fore_method == "ets")
  {
    for(ik in 1:ncomp_aggregate)
    {
      coef_fore[ik,] = forecast(ets(ftsm_aggregate$coeff[,ik+1]), h = fh)$mean
    }
  }
  else
  {
    warning("Forecasting method can either be ARIMA or ETS.")
  }
  rownames(coef_fore) = 1:ncomp_aggregate
  colnames(coef_fore) = 1:fh
  
  if(ncomp_aggregate == 1)
  {
    aggregate_fore = as.matrix(ftsm_aggregate$basis[,2]) %*% matrix(coef_fore, nrow = 1)
  }
  else
  {
    aggregate_fore = ftsm_aggregate$basis[,2:(ncomp_aggregate+1)] %*% coef_fore
  }
  
  # residual forecasts
  
  coef_fore_resi_list = list()
  for(iw in 1:n_pop)
  {
    coef_fore_resi = matrix(NA, ncomp_resi[iw], fh)
    if(fore_method == "arima")
    {
      for(ik in 1:ncomp_resi[iw])
      {
        coef_fore_resi[ik,] = forecast(auto.arima(ftsm_resi[[iw]]$coeff[,ik+1]), h = fh)$mean
      }
    }
    else if(fore_method == "ets")
    {
      for(ik in 1:ncomp_resi[iw])
      {
        coef_fore_resi[ik,] = forecast(ets(ftsm_resi[[iw]]$coeff[,ik+1]), h = fh)$mean
      }
    }
    else
    {
      warning("Forecasting method can either be ARIMA or ETS.")
    }
    coef_fore_resi_list[[iw]] = coef_fore_resi
    rm(iw)
  }
  
  resi_fore = list()
  for(iw in 1:n_pop)
  {
    resi_fore[[iw]] = ftsm_resi[[iw]]$basis[,2:(ncomp_resi[iw] + 1)] %*% coef_fore_resi_list[[iw]]
    rm(iw)
  }
  
  final_fore = list()
  for(iw in 1:n_pop)
  {
    final_fore[[iw]] = mean_function_list[[iw]] + (aggregate_fore + resi_fore[[iw]])[,fh]
    rm(iw)
  }
  return(final_fore)
}

## producing MLFTS point forecasts
# data_set_F: female data
# data_set_M: male data
# aux_variable: common data
# fh: forecast horizon
# fmethod: forecasting method
# method_ncomp: way of selecting the number of components

fore_national_cdf_MLFTS <- function(data_set_F, data_set_M, aux_variable, fh, fmethod, method_ncomp)
{
    data_F = data_dum_F = data_set_F/10^5
    data_M = data_dum_M = data_set_M/10^5
    
    if(any(data_F[,1] == 0)|any(data_M[,1] == 0))
    {
        data_F[,1] = replace(data_dum_F[,1], which(data_dum_F[,1] == 0), 10^-5)
        data_M[,1] = replace(data_dum_M[,1], which(data_dum_M[,1] == 0), 10^-5)
    }
    else
    {
        data_F = data_dum_F
        data_M = data_dum_M
    }
    rm(data_dum_F); rm(data_dum_M)
    
    if(missing(aux_variable)|is.null(aux_variable))
    {
        data_cumsum_F = data_cumsum_M = matrix(NA, nrow(data_F), ncol(data_F))
        for(ij in 1:nrow(data_F))
        {
            data_cumsum_F[ij,] = cumsum(data_F[ij,])
            data_cumsum_M[ij,] = cumsum(data_M[ij,])
            rm(ij)
        }
        
        data_cumsum_logit_F = data_cumsum_logit_M = matrix(NA, nrow(data_F), (ncol(data_F) - 1))
        for(ij in 1:nrow(data_F))
        {
            data_cumsum_logit_F[ij,] = logit(data_cumsum_F[ij, 1:(ncol(data_F) - 1)])
            data_cumsum_logit_M[ij,] = logit(data_cumsum_M[ij, 1:(ncol(data_M) - 1)])
            rm(ij)
        }
        rownames(data_cumsum_logit_F) = rownames(data_cumsum_logit_M) = years[1:nrow(data_F)]
        colnames(data_cumsum_logit_F) = colnames(data_cumsum_logit_M) = 1:(ncol(data_F) - 1)
        data_common = NULL
    }
    else
    {
        data_cumsum_F = data_cumsum_M = data_cumsum_T = matrix(NA, nrow(data_F), ncol(data_F))
        for(ij in 1:nrow(data_F))
        {
            data_cumsum_F[ij,] = cumsum(data_F[ij,])
            data_cumsum_M[ij,] = cumsum(data_M[ij,])
            data_cumsum_T[ij,] = cumsum(aux_variable[ij,])
            rm(ij)
        }
        
        data_cumsum_logit_F = data_cumsum_logit_M = data_cumsum_logit_T = matrix(NA, nrow(data_F), (ncol(data_F) - 1))
        for(ij in 1:nrow(data_F))
        {
            data_cumsum_logit_F[ij,] = logit(data_cumsum_F[ij, 1:(ncol(data_F) - 1)])
            data_cumsum_logit_M[ij,] = logit(data_cumsum_M[ij, 1:(ncol(data_M) - 1)])
            data_cumsum_logit_T[ij,] = logit(data_cumsum_T[ij, 1:(ncol(data_M) - 1)])
            rm(ij)
        }
        rownames(data_cumsum_logit_F) = rownames(data_cumsum_logit_M) = rownames(data_cumsum_logit_T) = years[1:nrow(data_F)]
        colnames(data_cumsum_logit_F) = colnames(data_cumsum_logit_M) = colnames(data_cumsum_logit_T) = 1:(ncol(data_F) - 1)
        data_common = data_cumsum_logit_T
    }
    data_comb = array(NA, dim = c((ncol(data_F) - 1), nrow(data_F), 2))
    data_comb[,,1] = t(data_cumsum_logit_F)
    data_comb[,,2] = t(data_cumsum_logit_M)
    
    # implementing the multilevel functional data model
    
    dum = MLFTS_model(data_input = data_comb, aux_var = data_common, ncomp_method = method_ncomp,
                      fh = fh, fore_method = fmethod)
    data_cumsum_logit_F_fore = dum[[1]]
    data_cumsum_logit_M_fore = dum[[2]]
    rm(dum)
    
    data_cumsum_logit_F_fore_add = c(invlogit(data_cumsum_logit_F_fore), 1)
    data_cumsum_logit_M_fore_add = c(invlogit(data_cumsum_logit_M_fore), 1)
    
    data_cumsum_logit_F_fore_add_diff = c(data_cumsum_logit_F_fore_add[1], diff(data_cumsum_logit_F_fore_add))
    data_cumsum_logit_M_fore_add_diff = c(data_cumsum_logit_M_fore_add[1], diff(data_cumsum_logit_M_fore_add))
    return(list(mlfts_fore_F = data_cumsum_logit_F_fore_add_diff * 10^5,
                mlfts_fore_M = data_cumsum_logit_M_fore_add_diff * 10^5))
}

clr_MFTS_fun <- function(fdata_F, fdata_M, ncomp_selection, fh, fore_method)
{
    n_age = ncol(fdata_F)
    n_year = nrow(fdata_F)
    
    h_x_t_F = CLR(fdata_F)$LR
    h_x_t_M = CLR(fdata_M)$LR
    
    h_x_t_comb = array(NA, dim = c(n_age, n_year, 2))
    h_x_t_comb[,,1] = t(h_x_t_F)
    h_x_t_comb[,,2] = t(h_x_t_M)
    
    n_pop = dim(h_x_t_comb)[3]
    MFTS_res = MFTS_model(data_input = h_x_t_comb, ncomp_method = ncomp_selection, fh = fh, fore_method = fore_method)
    MFTS_res_F = MFTS_res[1:n_age,]
    MFTS_res_M = MFTS_res[(n_age + 1):(n_age * 2),]
    MFTS_res_fore_F = as.numeric(invCLR(t(MFTS_res_F))) * 10^5
    MFTS_res_fore_M = as.numeric(invCLR(t(MFTS_res_M))) * 10^5
    return(list(MFTS_res_fore_F = MFTS_res_fore_F, MFTS_res_fore_M = MFTS_res_fore_M))    
}

## CLR multilevel functional time series
# fdata_F: female data
# fdata_M: male data
# ncomp_selection: way of selecting number of components
# fh: forecast horizon
# uni_fore_method: univariate time series method

clr_MLFTS_fun <- function(fdata_F, fdata_M, ncomp_selection, fh, uni_fore_method)
{
    n_age = ncol(fdata_F)
    n_year = nrow(fdata_F)
    
    h_x_t_F = CLR(fdata_F)$LR
    h_x_t_M = CLR(fdata_M)$LR
    
    h_x_t_comb = array(NA, dim = c(n_age, n_year, 2))
    h_x_t_comb[,,1] = t(h_x_t_F)
    h_x_t_comb[,,2] = t(h_x_t_M)
    
    n_pop = dim(h_x_t_comb)[3]
    MLFTS_res = MLFTS_model(data_input = h_x_t_comb, aux_var = NULL, ncomp_method = ncomp_selection, fh = fh, 
                            fore_method = uni_fore_method)
    MLFTS_res_F = MLFTS_res[[1]]
    MLFTS_res_M = MLFTS_res[[2]]
    MLFTS_res_fore_F = as.numeric(invCLR(t(MLFTS_res_F))) * 10^5
    MLFTS_res_fore_M = as.numeric(invCLR(t(MLFTS_res_M))) * 10^5
    return(list(MLFTS_res_fore_F = MLFTS_res_fore_F, MLFTS_res_fore_M = MLFTS_res_fore_M))    
}

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

##########################################################################
# novel way for selecting the number of retained factor loadings in HDFTS
##########################################################################

select_K_new <- function(eigenvalue, index, sample_size, no_pop)
{
  return((eigenvalue[index])/sample_size + index * (max(sample_size, no_pop)^(-0.5)))
}

###############################
# Numbers of factors selection
###############################

### The method of den Reijer et al. 2021 ###

k_criterion <- function(eigenvalue, plot = TRUE)
{
  n = length(eigenvalue)
  Hn = sum(1/(1:n))
  
  eigen_diff = eigenvalue[-n] - eigenvalue[-1]
  lambda_bar = 1/((2:n)*Hn)
  
  n_cross = which(eigen_diff <= lambda_bar)[1]
  
  if(n_cross > 1)
  {
    n_return = n_cross - 1
  } 
  else
  {
    n_return = 1
  }
  
  if(plot)
  {
    plot(eigen_diff[1:(n_cross+6)], type = "b", xlab = expression("Eigenvalue number" ~ italic(k)), ylab = expression("Eigenvalue" ~~~ lambda))
    lines(lambda_bar[1:(n_cross+6)], type ="b", col = 2)
    abline(v = n_return, lty = 2, col = 4)
    legend("topright", lty = c(1,1), lwd = c(2,2), col = c(1,2), c(expression(lambda ~~ "(eigenvalue)  "), expression(bar(lambda) ~~ "(threshold)  ")))
  }
  return(n_return)
}

# Three-ways of selecting the number of factors
# data: (sample size x no_pop x no_grid)

HDFTS_factor_decomp <- function(data)
{
    sample_size = dim(data)[1]
    no_pop = dim(data)[2]
    D_val = dim(data)[3]
    
    Delta = matrix(0, sample_size, sample_size)
    for(ij in 1:no_pop)
    {
        temp = matrix(NA, sample_size, sample_size)
        for(t in 1:sample_size)
        {
            for(s in 1:sample_size)
            {
                temp[t,s] = matrix(data[t,ij,], nrow = 1) %*% matrix(data[s,ij,], ncol = 1)
            }
        }
        Delta = Delta + temp/D_val
        rm(temp)
    }
    rm(t); rm(s); rm(ij)
    
    Delta_mat = Delta/no_pop
    Delta_mat_eigen = eigen(Delta_mat)
    
    # Two ways of selecting the number of components
    
    K_val_k_criterion = k_criterion(eigenvalue = Delta_mat_eigen$values, plot = FALSE)
    K_val_select_K = select_K(tau = 10^-3, eigenvalue = Delta_mat_eigen$values)
    
    # new way of selecting the number of components
    
    K_val = vector("numeric", sample_size)
    for(ik in 1:sample_size)
    {
        K_val[ik] = select_K_new(eigenvalue = Delta_mat_eigen$values, index = ik, sample_size = sample_size,
                                 no_pop = no_pop)
        rm(ik)
    }
    
    q_val_est = max(c(which.min(K_val) - 1, K_val_k_criterion, K_val_select_K))
    if(q_val_est == 0)
    {
        warning("The number of components is zero.")
    }
    Delta_eigen_vector = as.matrix(Delta_mat_eigen$vectors[,1:q_val_est]) * sqrt(sample_size)
    
    ###############################
    # estimate the eigen-dimension
    ###############################
    
    factor_loading = array(NA, dim = c(q_val_est, D_val, no_pop))
    for(ij in 1:no_pop)
    {
        factor_loading[,,ij] = (crossprod(Delta_eigen_vector, data[,ij,]))/sample_size
        rm(ij)
    }
    
    # variance
    
    C_X = array(NA, dim = c(no_pop, no_pop, D_val, D_val))
    for(ij in 1:no_pop)
    {
        for(iw in 1:no_pop)
        {
            C_X[ij,iw,,] = crossprod(factor_loading[,,ij], factor_loading[,,iw])
        }
    }
    rm(ij); rm(iw)
    
    error = array(NA, dim = c(sample_size, no_pop, D_val))
    for(ij in 1:no_pop)
    {
        error[,ij,] = data[,ij,] - (Delta_eigen_vector %*% factor_loading[,,ij])
        rm(ij)
    }
    return(list(raw_data = data, factor_loading = factor_loading,
                factors = Delta_eigen_vector, error = error))
}

######################################################################
# Multivariate functional time series method (evaluation of accuracy)
######################################################################

# fdata_F: a data matrix of dimension (n by p)
# fdata_M: a data matrix of dimension (n by p)
# fore_method: type of transformation
# horizon: forecast horizon 1 to 16
# way_ncomp: K = 6 or EVR
# uni_fore_method: univariate time-series forecasting method

point_fore_national_cdf_MFTS <- function(fdata_F, fdata_M, fore_method, horizon, way_ncomp, uni_fore_method)
{
    n_year = nrow(fdata_F)
    forecast_val_F = forecast_val_M = matrix(NA, ncol(fdata_F), (21 - horizon))
    if(fore_method == "CDF")
    {
        for(ij in 1:(21 - horizon))
        {
            dum <- fore_national_cdf_MFTS(data_set_F = fdata_F[1:(n_year - 21 + ij),], 
                                          data_set_M = fdata_M[1:(n_year - 21 + ij),],
                                          fh = horizon, fmethod = uni_fore_method, method_ncomp = way_ncomp)
            forecast_val_F[,ij] = dum$mfts_fore_F
            forecast_val_M[,ij] = dum$mfts_fore_M
            rm(ij)
        }
    }
    else if(fore_method == "CLR")
    {
        for(ij in 1:(21 - horizon))
        {
            dum <- clr_MFTS_fun(fdata_F = fdata_F[1:(n_year - 21 + ij),], 
                                fdata_M = fdata_M[1:(n_year - 21 + ij),], 
                                ncomp_selection = way_ncomp, fh = horizon, fore_method = uni_fore_method)
            forecast_val_F[,ij] = dum$MFTS_res_fore_F
            forecast_val_M[,ij] = dum$MFTS_res_fore_M
            rm(ij)
        }
    }
    
    holdout_val_dum_F = t(matrix(fdata_F[(n_year - 20 + horizon):n_year,], length((n_year - 20 + horizon):n_year), ncol(fdata_F)))
    holdout_val_dum_M = t(matrix(fdata_M[(n_year - 20 + horizon):n_year,], length((n_year - 20 + horizon):n_year), ncol(fdata_F)))
    if(any(holdout_val_dum_F == 0))
    {
        holdout_val_F = replace(holdout_val_dum_F, which(holdout_val_dum_F == 0), 10^-5)
    }
    else
    {
        holdout_val_F = holdout_val_dum_F
    }
    if(any(holdout_val_dum_M == 0))
    {
        holdout_val_M = replace(holdout_val_dum_M, which(holdout_val_dum_M == 0), 10^-5)
    }
    else
    {
        holdout_val_M = holdout_val_dum_M
    }
    rm(holdout_val_dum_F); rm(holdout_val_dum_M)
    
    KL_div_val_F = JS_div_val_F = L1_dist_F = L2_dist_F =
    KL_div_val_M = JS_div_val_M = L1_dist_M = L2_dist_M = vector("numeric", (21 - horizon))
    for(ij in 1:(21 - horizon))
    {
        # symmetric KL dist (Female)
        
        KL_div_val_F[ij] = mean(KLdiv(cbind(forecast_val_F[,ij], holdout_val_F[,ij]))[2:3])
        
        # Jensen-Shannon dist
        
        JS_div_val_F[ij] = sqrt(mean(KLdiv(cbind(forecast_val_F[,ij], apply(cbind(forecast_val_F[,ij], holdout_val_F[,ij]), 1, geometric.mean)))[2:3]))
        
        # L1 dist
        
        L1_dist_F[ij] = wasserstein1d(a = forecast_val_F[,ij], b = holdout_val_F[,ij], p = 1)
        
        # L2 dist
        
        L2_dist_F[ij] = wasserstein1d(a = forecast_val_F[,ij], b = holdout_val_F[,ij], p = 2)
        
        # symmetric KL dist (Male)
        
        KL_div_val_M[ij] = mean(KLdiv(cbind(forecast_val_M[,ij], holdout_val_M[,ij]))[2:3])
        
        # Jensen-Shannon dist
        
        JS_div_val_M[ij] = sqrt(mean(KLdiv(cbind(forecast_val_M[,ij], apply(cbind(forecast_val_M[,ij], holdout_val_M[,ij]), 1, geometric.mean)))[2:3]))
        
        # L1 dist
        
        L1_dist_M[ij] = wasserstein1d(a = forecast_val_M[,ij], b = holdout_val_M[,ij], p = 1)
        
        # L2 dist
        
        L2_dist_M[ij] = wasserstein1d(a = forecast_val_M[,ij], b = holdout_val_M[,ij], p = 2)
        rm(ij)
    }
    err_F = c(mean(KL_div_val_F, na.rm = TRUE), mean(JS_div_val_F), sqrt(mean(L1_dist_F^2)), sqrt(mean(L2_dist_F^2)))
    err_M = c(mean(KL_div_val_M, na.rm = TRUE), mean(JS_div_val_M), sqrt(mean(L1_dist_M^2)), sqrt(mean(L2_dist_M^2)))
    
    return(list(forecast_pdf_F = forecast_val_F, forecast_pdf_M = forecast_val_M,
                holdout_pdf_F = holdout_val_F, holdout_pdf_M = holdout_val_M,
                err_F = err_F, err_M = err_M))
}

# fdata_F: a data matrix of dimension (n by p)
# fdata_M: a data matrix of dimension (n by p)
# fdata_common: a common data matrix
# fore_method: CDF or CLR
# horizon: forecast horizon 1 to 16
# way_ncomp: way of selecting the number of components
# forecasting_method: univariate time-series forecasting method

point_fore_national_cdf_MLFTS <- function(fdata_F, fdata_M, fdata_common, fore_method, horizon, way_ncomp, forecasting_method)
{
    n_year = nrow(fdata_F)
    forecast_val_F = forecast_val_M = matrix(NA, ncol(fdata_F), (21 - horizon))
    if(fore_method == "CDF")
    {
        for(ij in 1:(21 - horizon))
        {
            dum <- fore_national_cdf_MLFTS(data_set_F = fdata_F[1:(n_year - 21 + ij),], 
                                           data_set_M = fdata_M[1:(n_year - 21 + ij),],
                                           aux_variable = fdata_common[1:(n_year - 21 + ij),], 
                                           fh = horizon, fmethod = forecasting_method,
                                           method_ncomp = way_ncomp)
            forecast_val_F[,ij] = dum$mlfts_fore_F
            forecast_val_M[,ij] = dum$mlfts_fore_M
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
                                uni_fore_method = forecasting_method)
            forecast_val_F[,ij] = dum$MLFTS_res_fore_F
            forecast_val_M[,ij] = dum$MLFTS_res_fore_M
            rm(ij); rm(dum)
        }
    }
    rownames(forecast_val_F) = rownames(forecast_val_M) = 1:ncol(fdata_F)
    colnames(forecast_val_F) = colnames(forecast_val_M) = 1:(21 - horizon)
    
    holdout_val_dum_F = t(matrix(fdata_F[(n_year - 20 + horizon):n_year,], length((n_year - 20 + horizon):n_year), ncol(fdata_F)))
    holdout_val_dum_M = t(matrix(fdata_M[(n_year - 20 + horizon):n_year,], length((n_year - 20 + horizon):n_year), ncol(fdata_F)))
    if(any(holdout_val_dum_F == 0))
    {
        holdout_val_F = replace(holdout_val_dum_F, which(holdout_val_dum_F == 0), 10^-5)
    }
    else
    {
        holdout_val_F = holdout_val_dum_F
    }
    if(any(holdout_val_dum_M == 0))
    {
        holdout_val_M = replace(holdout_val_dum_M, which(holdout_val_dum_M == 0), 10^-5)
    }
    else
    {
        holdout_val_M = holdout_val_dum_M
    }
    rm(holdout_val_dum_F); rm(holdout_val_dum_M)
    
    KL_div_val_F = JS_div_val_F = L1_dist_F = L2_dist_F = 
    KL_div_val_M = JS_div_val_M = L1_dist_M = L2_dist_M = vector("numeric", (21 - horizon))
    for(ij in 1:(21 - horizon))
    {
        # symmetric KL dist
        
        KL_div_val_F[ij] = mean(KLdiv(cbind(forecast_val_F[,ij], holdout_val_F[,ij]))[2:3])
        
        # Jensen-Shannon dist
        
        JS_div_val_F[ij] = sqrt(mean(KLdiv(cbind(forecast_val_F[,ij], apply(cbind(forecast_val_F[,ij], holdout_val_F[,ij]), 1, geometric.mean)))[2:3]))
        
        # L1_dist
        
        L1_dist_F[ij] = wasserstein1d(a = forecast_val_F[,ij], b = holdout_val_F[,ij], p = 1)
        
        # L2_dist
        
        L2_dist_F[ij] = wasserstein1d(a = forecast_val_F[,ij], b = holdout_val_F[,ij], p = 2)
        
        # symmetric KL dist
        
        KL_div_val_M[ij] = mean(KLdiv(cbind(forecast_val_M[,ij], holdout_val_M[,ij]))[2:3])
        
        # Jensen-Shannon dist
        
        JS_div_val_M[ij] = sqrt(mean(KLdiv(cbind(forecast_val_M[,ij], apply(cbind(forecast_val_M[,ij], holdout_val_M[,ij]), 1, geometric.mean)))[2:3]))
        
        # L1_dist
        
        L1_dist_M[ij] = wasserstein1d(a = forecast_val_M[,ij], b = holdout_val_M[,ij], p = 1)
        
        # L2_dist
        
        L2_dist_M[ij] = wasserstein1d(a = forecast_val_M[,ij], b = holdout_val_M[,ij], p = 2)
    }
    
    err_F = c(mean(KL_div_val_F, na.rm = TRUE), mean(JS_div_val_F, na.rm = TRUE), 
              sqrt(mean(L1_dist_F^2, na.rm = TRUE)), sqrt(mean(L2_dist_F^2, na.rm = TRUE)))
    err_M = c(mean(KL_div_val_M, na.rm = TRUE), mean(JS_div_val_M, na.rm = TRUE), 
              sqrt(mean(L1_dist_M^2, na.rm = TRUE)), sqrt(mean(L2_dist_M^2, na.rm = TRUE)))
    return(list(forecast_pdf_F = forecast_val_F, forecast_pdf_M = forecast_val_M,
                holdout_pdf_F = holdout_val_F, holdout_pdf_M = holdout_val_M,
                err_F = err_F, err_M = err_M))
}

clr_MLFTS_fun <- function(fdata_F, fdata_M, ncomp_selection, fh, uni_fore_method)
{
    n_age = ncol(fdata_F)
    n_year = nrow(fdata_F)
    
    h_x_t_F = CLR(fdata_F)$LR
    h_x_t_M = CLR(fdata_M)$LR
    
    h_x_t_comb = array(NA, dim = c(n_age, n_year, 2))
    h_x_t_comb[,,1] = t(h_x_t_F)
    h_x_t_comb[,,2] = t(h_x_t_M)
    
    n_pop = dim(h_x_t_comb)[3]
    MLFTS_res = MLFTS_model(data_input = h_x_t_comb, aux_var = NULL, ncomp_method = ncomp_selection, 
                            fh = fh, fore_method = uni_fore_method)
    MLFTS_res_F = MLFTS_res[[1]]
    MLFTS_res_M = MLFTS_res[[2]]
    MLFTS_res_fore_F = as.numeric(invCLR(t(MLFTS_res_F))) * 10^5
    MLFTS_res_fore_M = as.numeric(invCLR(t(MLFTS_res_M))) * 10^5
    return(list(MLFTS_res_fore_F = MLFTS_res_fore_F, MLFTS_res_fore_M = MLFTS_res_fore_M))    
}


# data_set_F: (n_year x n_age x n_state) female data
# data_set_M: (n_year x n_age x n_state) male data
# fh: forecast horizon
# fmethod: univariate time-series forecasting method

fore_national_clr_FANOVA <- function(data_set_F, data_set_M, fh, fmethod)
{
    n_year = nrow(data_set_F)
    n_age = ncol(data_set_F)
    
    data_F_dum = data_set_F/10^5
    data_M_dum = data_set_M/10^5
    
    if(any(data_F_dum == 0))
    {
        data_F = replace(data_F_dum, which(data_F_dum == 0), 10^-5)
    }
    else
    {
        data_F = data_F_dum	
    }
    if(any(data_M_dum == 0))
    {
        data_M = replace(data_M_dum, which(data_M_dum == 0), 10^-5)
    }
    else
    {
        data_M = data_M_dum	
    }
    rm(data_F_dum); rm(data_M_dum)
    
    h_x_t_F = h_x_t_M = array(NA, dim = dim(data_set_F))	
    for(ik in 1:n_state)
    {
        h_x_t_F[,,ik] = CLR(data_F[,,ik])$LR	
        h_x_t_M[,,ik] = CLR(data_M[,,ik])$LR
        rm(ik)
    }
    
    h_x_t_F_mat = h_x_t_M_mat = matrix(NA, (n_year * n_state), n_age)
    for(ik in 1:n_state)
    {
        h_x_t_F_mat[((ik - 1) * n_year + 1):(ik * n_year), ] = h_x_t_F[,,ik]
        h_x_t_M_mat[((ik - 1) * n_year + 1):(ik * n_year), ] = h_x_t_M[,,ik]
        rm(ik)
    }
    
    FANOVA_means <- Two_way_mean(data_pop1 = t(h_x_t_M_mat), data_pop2 = t(h_x_t_F_mat),
                                 year = years[1:n_year], age = ages, n_prefectures = n_state, n_populations = 2)
    
    # grand effect + row effect + column effect
    
    FANOVA_means_decomp_combo = array(NA, dim = c(n_age, n_state, 2), dimnames = list(ages, state, c("M", "F")))
    for(iw in 1:n_state)
    {
        for(ij in 1:2)
        {
            FANOVA_means_decomp_combo[,iw,ij] = FANOVA_means$FGE_mean + FANOVA_means$FRE_mean[iw,] + FANOVA_means$FCE_mean[ij,]
            rm(ij)
        }
        rm(iw)
    }
    
    # FANOVA residuals
    
    FANOVA_residuals <- Two_way_mean_residuals(data_pop1 = t(h_x_t_M_mat), data_pop2 = t(h_x_t_F_mat),
                                               year = years[1:n_year], age = ages, 
                                               n_prefectures = n_state, n_populations = 2)
    
    FANOVA_residuals_male <- array(FANOVA_residuals$residuals1_mean, dim = c(n_year, n_state, n_age), 
                                   dimnames = list(years[1:n_year], state, ages))
    
    FANOVA_residuals_female <- array(FANOVA_residuals$residuals2_mean, dim = c(n_year, n_state, n_age), 
                                     dimnames = list(years[1:n_year], state, ages))
    
    # stacking male and female residuals
    
    FANOVA_residuals_combo = array(NA, dim = c(n_year, (n_state * 2), n_age), 
                                   dimnames = list(years[1:n_year], c(1:n_state, 1:n_state), ages))
    
    FANOVA_residuals_combo[,1:n_state,] = FANOVA_residuals_male
    FANOVA_residuals_combo[,(n_state + 1):(n_state * 2),] = FANOVA_residuals_female
    
    # functional factor model
    
    HDFTS_factor_decomp_combo <- HDFTS_factor_decomp(FANOVA_residuals_combo)
    HDFTS_factor_decomp_combo_factor_loading <- HDFTS_factor_decomp_combo$factor_loading
    HDFTS_factor_decomp_combo_factors <- HDFTS_factor_decomp_combo$factors
    HDFTS_factor_decomp_combo_residuals <- HDFTS_factor_decomp_combo$error
    
    # forecasting
    
    n_factor <- ncol(HDFTS_factor_decomp_combo_factors)
    HDFTS_factor_decomp_combo_factors_forecast = matrix(NA, 1, n_factor)
    for(ik in 1:n_factor)
    {
        if(fmethod == "ets")
        {
            HDFTS_factor_decomp_combo_factors_forecast[,ik] = forecast(ets(HDFTS_factor_decomp_combo_factors[,ik]), h = fh)$mean[fh]
        }
        else if(fmethod == "arima")
        {
            HDFTS_factor_decomp_combo_factors_forecast[,ik] = forecast(auto.arima(HDFTS_factor_decomp_combo_factors[,ik]), h = fh)$mean[fh]
        }
        rm(ik)
    }
    HDFTS_factor_decomp_combo_forecast <- matrix(NA, n_age, (n_state * 2))
    for(ik in 1:(n_state * 2))
    {
        HDFTS_factor_decomp_combo_forecast[,ik] = as.numeric(HDFTS_factor_decomp_combo_factors_forecast %*% HDFTS_factor_decomp_combo_factor_loading[,,ik])
        rm(ik)
    }
    HDFTS_factor_decomp_combo_forecast_male = HDFTS_factor_decomp_combo_forecast[,1:n_state]
    HDFTS_factor_decomp_combo_forecast_female = HDFTS_factor_decomp_combo_forecast[,(n_state + 1):(n_state * 2)]
    
    HDFTS_factor_decomp_combo_forecast_combo = array(NA, dim = c(n_age, n_state, 2))
    HDFTS_factor_decomp_combo_forecast_combo[,,1] = HDFTS_factor_decomp_combo_forecast_male
    HDFTS_factor_decomp_combo_forecast_combo[,,2] = HDFTS_factor_decomp_combo_forecast_female
    
    # adding FANOVA means back
    
    FANOVA_FFM_forecast = HDFTS_factor_decomp_combo_forecast_combo + FANOVA_means_decomp_combo
    
    FANOVA_FFM_forecast_array = array(NA, dim = dim(FANOVA_FFM_forecast),
                                      dimnames = list(ages, state, c("M", "F")))
    for(ik in 1:n_state)
    {
        for(ij in 1:2)
        {
            FANOVA_FFM_forecast_array[,ik,ij] = invCLR(t(FANOVA_FFM_forecast[,ik,ij])) * 10^5
            rm(ij)
        }
        rm(ik)
    }
    return(list(male_fore = FANOVA_FFM_forecast_array[,,1], female_fore = FANOVA_FFM_forecast_array[,,2]))
}

#######################
# Two-way FANOVA + FFM
#######################

# data_set_F: (n_year x n_age x n_state) female data
# data_set_M: (n_year x n_age x n_state) male data
# fh: forecast horizon
# fmethod: univariate time-series forecasting method

fore_national_cdf_FANOVA <- function(data_set_F, data_set_M, fh, fmethod)
{
    n_year = nrow(data_set_F)
    
    data_F = data_set_F/10^5
    data_M = data_set_M/10^5
    
    data_cumsum_dum_F = data_cumsum_dum_M = array(NA, dim = c(n_year, ncol(data_F), n_state),
                                                  dimnames = list(years[1:n_year], ages, state))
    for(ik in 1:n_state)
    {
        for(ij in 1:n_year)
        {
            data_cumsum_dum_F[ij,,ik] = cumsum(data_F[ij,,ik])
            data_cumsum_dum_M[ij,,ik] = cumsum(data_M[ij,,ik])
            rm(ij)
        }
        rm(ik)
    }
    
    if(any(data_cumsum_dum_F == 0))
    {
        data_cumsum_F = replace(data_cumsum_dum_F, which(data_cumsum_dum_F == 0), 10^-5)
    }
    else
    {
        data_cumsum_F = data_cumsum_dum_F
    }
    if(any(data_cumsum_dum_M == 0))
    {
        data_cumsum_M = replace(data_cumsum_dum_M, which(data_cumsum_dum_M == 0), 10^-5)
    }
    else
    {
        data_cumsum_M = data_cumsum_dum_M
    }
    rm(data_cumsum_dum_F); rm(data_cumsum_dum_M)
    
    data_cumsum_logit_F = data_cumsum_logit_M = array(NA, dim = c(n_year, (n_age - 1), n_state),
                                                      dimnames = list(years[1:n_year], ages[1:(n_age - 1)], state))
    for(ik in 1:n_state)
    {
        for(ij in 1:nrow(data_F))
        {
            data_cumsum_logit_F[ij,,ik] = logit(data_cumsum_F[ij, 1:(ncol(data_F) - 1), ik])
            data_cumsum_logit_M[ij,,ik] = logit(data_cumsum_M[ij, 1:(ncol(data_M) - 1), ik])
            rm(ij)
        }
    }
    
    data_cumsum_logit_F_mat = data_cumsum_logit_M_mat = matrix(NA, (n_year * n_state), (n_age - 1))
    for(ik in 1:n_state)
    {
        data_cumsum_logit_F_mat[((ik - 1) * n_year + 1):(ik * n_year), ] = data_cumsum_logit_F[,,ik]
        data_cumsum_logit_M_mat[((ik - 1) * n_year + 1):(ik * n_year), ] = data_cumsum_logit_M[,,ik]
        rm(ik)
    }
    
    FANOVA_means <- Two_way_mean(data_pop1 = t(data_cumsum_logit_M_mat), data_pop2 = t(data_cumsum_logit_F_mat),
                    year = years[1:n_year], age = ages[1:(n_age - 1)], n_prefectures = n_state, n_populations = 2)
    
    # grand effect + row effect + column effect
    
    FANOVA_means_decomp_combo = array(NA, dim = c(length(ages[1:(n_age - 1)]), n_state, 2),
                                      dimnames = list(ages[1:(n_age - 1)], 1:n_state, c("M", "F")))
    for(iw in 1:n_state)
    {
        for(ij in 1:2)
        {
            FANOVA_means_decomp_combo[,iw,ij] = FANOVA_means$FGE_mean + FANOVA_means$FRE_mean[iw,] + FANOVA_means$FCE_mean[ij,]
            rm(ij)
        }
        rm(iw)
    }
    
    # FANOVA residuals
    
    FANOVA_residuals <- Two_way_mean_residuals(data_pop1 = t(data_cumsum_logit_M_mat), data_pop2 = t(data_cumsum_logit_F_mat),
                                year = years[1:n_year], age = ages[1:(n_age - 1)], n_prefectures = n_state, n_populations = 2)
    
    FANOVA_residuals_male <- array(FANOVA_residuals$residuals1_mean, dim = c(n_year, n_state, length(ages[1:110])),
                                   dimnames = list(years[1:n_year], state, ages[1:(n_age - 1)]))
    
    FANOVA_residuals_female <- array(FANOVA_residuals$residuals2_mean, dim = c(n_year, n_state, length(ages[1:110])),
                                     dimnames = list(years[1:n_year], state, ages[1:(n_age - 1)]))

    # stacking male and female residuals
    
    FANOVA_residuals_combo = array(NA, dim = c(n_year, (n_state * 2), length(ages[1:(n_age - 1)])),
                              dimnames = list(years[1:n_year], c(1:n_state, 1:n_state), ages[1:(n_age - 1)]))
    
    FANOVA_residuals_combo[,1:n_state,] = FANOVA_residuals_male
    FANOVA_residuals_combo[,(n_state + 1):(n_state * 2),] = FANOVA_residuals_female
    
    # functional factor model
    
    HDFTS_factor_decomp_combo <- HDFTS_factor_decomp(FANOVA_residuals_combo)
    HDFTS_factor_decomp_combo_factor_loading <- HDFTS_factor_decomp_combo$factor_loading
    HDFTS_factor_decomp_combo_factors <- HDFTS_factor_decomp_combo$factors
    HDFTS_factor_decomp_combo_residuals <- HDFTS_factor_decomp_combo$error
    
    # forecasting
    
    n_factor <- ncol(HDFTS_factor_decomp_combo_factors)
    HDFTS_factor_decomp_combo_factors_forecast = matrix(NA, 1, n_factor)
    for(ik in 1:n_factor)
    {
        if(fmethod == "ets")
        {
            HDFTS_factor_decomp_combo_factors_forecast[,ik] = forecast(ets(HDFTS_factor_decomp_combo_factors[,ik]), h = fh)$mean[fh]
        }
        else if(fmethod == "arima")
        {
            HDFTS_factor_decomp_combo_factors_forecast[,ik] = forecast(auto.arima(HDFTS_factor_decomp_combo_factors[,ik]), h = fh)$mean[fh]
        }
        rm(ik)
    }
  
    HDFTS_factor_decomp_combo_forecast <- matrix(NA, length(ages[1:(n_age - 1)]), (n_state * 2))
    for(ik in 1:(n_state * 2))
    {
        HDFTS_factor_decomp_combo_forecast[,ik] = as.numeric(HDFTS_factor_decomp_combo_factors_forecast %*% HDFTS_factor_decomp_combo_factor_loading[,,ik])
        rm(ik)
    }
    HDFTS_factor_decomp_combo_forecast_male = HDFTS_factor_decomp_combo_forecast[,1:n_state]
    HDFTS_factor_decomp_combo_forecast_female = HDFTS_factor_decomp_combo_forecast[,(n_state + 1):(n_state * 2)]
    
    HDFTS_factor_decomp_combo_forecast_combo = array(NA, dim = c(length(ages[1:(n_age - 1)]), n_state, 2))
    HDFTS_factor_decomp_combo_forecast_combo[,,1] = HDFTS_factor_decomp_combo_forecast_male
    HDFTS_factor_decomp_combo_forecast_combo[,,2] = HDFTS_factor_decomp_combo_forecast_female
    
    # adding FANOVA means back
    
    FANOVA_FFM_forecast = HDFTS_factor_decomp_combo_forecast_combo + FANOVA_means_decomp_combo
    
    data_cumsum_logit_F_fore_add = array(NA, dim = c(n_age, n_state, 2))
    for(ik in 1:n_state)
    {
        for(ij in 1:2)
        {
            data_cumsum_logit_F_fore_add[,ik,ij] = c(invlogit(FANOVA_FFM_forecast[,ik,ij]), 1)
            rm(ij)
        }
        rm(ik)
    }
    
    # split the combined forecasts into male and female forecasts
    
    data_cumsum_logit_male_fore_add   = data_cumsum_logit_F_fore_add[,,1]
    data_cumsum_logit_female_fore_add = data_cumsum_logit_F_fore_add[,,2]
    
    data_cumsum_logit_male_fore_add_diff = data_cumsum_logit_female_fore_add_diff = matrix(NA, n_age, n_state)
    for(ik in 1:n_state)
    {
        data_cumsum_logit_male_fore_add_diff[,ik] = c(data_cumsum_logit_male_fore_add[1,ik], diff(data_cumsum_logit_male_fore_add[,ik]))
        data_cumsum_logit_female_fore_add_diff[,ik] = c(data_cumsum_logit_female_fore_add[1,ik], diff(data_cumsum_logit_female_fore_add[,ik]))
        rm(ik)
    }
    colnames(data_cumsum_logit_male_fore_add_diff) = colnames(data_cumsum_logit_female_fore_add_diff) = state
    rownames(data_cumsum_logit_male_fore_add_diff) = rownames(data_cumsum_logit_female_fore_add_diff) = ages
    
    return(list(male_fore = data_cumsum_logit_male_fore_add_diff * 10^5,
                female_fore = data_cumsum_logit_female_fore_add_diff * 10^5))
}

# fdata_F: (n_year x n_age x n_state) female data
# fdata_M: (n_year x n_age x n_state) male data
# fore_method: CDF or CLR
# horizon: forecast horizon
# uni_fore_method: univariate time-series forecasting method, ets or arima

point_fore_national_cdf_FANOVA_FFM <- function(fdata_F, fdata_M, fore_method, horizon, uni_fore_method)
{
    n_year = nrow(fdata_F)
    n_age = ncol(fdata_F)
    forecast_val_F = forecast_val_M = array(NA, dim = c(n_age, (21 - horizon), n_state),
                                            dimnames = list(ages, 1:(21 - horizon), state))
    if(fore_method == "CDF")
    {
        for(ij in 1:(21 - horizon))
        {
            dum <- fore_national_cdf_FANOVA(data_set_F = fdata_F[1:(n_year - 21 + ij),,],
                                            data_set_M = fdata_M[1:(n_year - 21 + ij),,],
                                            fh = horizon, fmethod = uni_fore_method)
            forecast_val_F[,ij,] = dum$female_fore
            forecast_val_M[,ij,] = dum$male_fore
            rm(ij)
        }
    }
    else if(fore_method == "CLR")
    {
        for(ij in 1:(21 - horizon))
        {
            dum <- fore_national_clr_FANOVA(data_set_F = fdata_F[1:(n_year - 21 + ij),,],
                                            data_set_M = fdata_M[1:(n_year - 21 + ij),,],
                                            fh = horizon, fmethod = uni_fore_method)
            forecast_val_F[,ij,] = dum$female_fore
            forecast_val_M[,ij,] = dum$male_fore
            rm(ij)
        }
    }
 
    KL_div_val_F = JS_div_val_F = L1_dist_F = L2_dist_F =
    KL_div_val_M = JS_div_val_M = L1_dist_M = L2_dist_M = matrix(NA, (21 - horizon), n_state)
    for(ik in 1:n_state)
    {
        holdout_val_dum_F = t(matrix(fdata_F[(n_year - 20 + horizon):n_year,,ik], length((n_year - 20 + horizon):n_year), ncol(fdata_F)))
        holdout_val_dum_M = t(matrix(fdata_M[(n_year - 20 + horizon):n_year,,ik], length((n_year - 20 + horizon):n_year), ncol(fdata_F)))
    
        if(any(holdout_val_dum_F == 0))
        {
            holdout_val_F = replace(holdout_val_dum_F, which(holdout_val_dum_F == 0), 10^-5)
        }
        else
        {
            holdout_val_F = holdout_val_dum_F
        }
        if(any(holdout_val_dum_M == 0))
        {
            holdout_val_M = replace(holdout_val_dum_M, which(holdout_val_dum_M == 0), 10^-5)
        }
        else
        {
            holdout_val_M = holdout_val_dum_M
        }
        rm(holdout_val_dum_F); rm(holdout_val_dum_M)
    
        for(ij in 1:(21 - horizon))
        {
            # symmetric KL dist (Female)
        
            KL_div_val_F[ij,ik] = mean(KLdiv(cbind(forecast_val_F[,ij,ik], holdout_val_F[,ij]))[2:3])
        
            # Jensen-Shannon dist
            
            JS_div_val_F[ij,ik] = sqrt(mean(KLdiv(cbind(forecast_val_F[,ij,ik], apply(cbind(forecast_val_F[,ij,ik], holdout_val_F[,ij]), 1, geometric.mean)))[2:3]))
            
            # L1 dist
            
            L1_dist_F[ij,ik] = wasserstein1d(a = forecast_val_F[,ij,ik], b = holdout_val_F[,ij], p = 1)
            
            # L2 dist
            
            L2_dist_F[ij,ik] = wasserstein1d(a = forecast_val_F[,ij,ik], b = holdout_val_F[,ij], p = 2)
            
            # symmetric KL dist (Male)
            
            KL_div_val_M[ij,ik] = mean(KLdiv(cbind(forecast_val_M[,ij,ik], holdout_val_M[,ij]))[2:3])
            
            # Jensen-Shannon dist
            
            JS_div_val_M[ij,ik] = sqrt(mean(KLdiv(cbind(forecast_val_M[,ij,ik], apply(cbind(forecast_val_M[,ij,ik], holdout_val_M[,ij]), 1, geometric.mean)))[2:3]))
            
            # L1 dist
            
            L1_dist_M[ij,ik] = wasserstein1d(a = forecast_val_M[,ij,ik], b = holdout_val_M[,ij], p = 1)
            
            # L2 dist
            
            L2_dist_M[ij,ik] = wasserstein1d(a = forecast_val_M[,ij,ik], b = holdout_val_M[,ij], p = 2)
            rm(ij)
        }
        rm(ik)
    }
    rownames(KL_div_val_F) = rownames(JS_div_val_F) = rownames(L1_dist_F) = rownames(L2_dist_F) =
    rownames(KL_div_val_M) = rownames(JS_div_val_M) = rownames(L1_dist_M) = rownames(L2_dist_M) = 1:(21 - horizon)
    
    colnames(KL_div_val_F) = colnames(JS_div_val_F) = colnames(L1_dist_F) = colnames(L2_dist_F) =
    colnames(KL_div_val_M) = colnames(JS_div_val_M) = colnames(L1_dist_M) = colnames(L2_dist_M) = state
    
    err_F = err_M = matrix(NA, 4, n_state)
    for(ik in 1:n_state)
    {
        err_F[,ik] = c(mean(KL_div_val_F[,ik]), mean(JS_div_val_F[,ik]), sqrt(mean(L1_dist_F[,ik]^2)), sqrt(mean(L2_dist_F[,ik]^2)))
        err_M[,ik] = c(mean(KL_div_val_M[,ik]), mean(JS_div_val_M[,ik]), sqrt(mean(L1_dist_M[,ik]^2)), sqrt(mean(L2_dist_M[,ik]^2)))
        rm(ik)
    }
    rownames(err_F) = rownames(err_M) = c("KLD", "JSD", "W1", "W2")
    colnames(err_F) = colnames(err_M) = state
    
    return(list(forecast_pdf_F = forecast_val_F, forecast_pdf_M = forecast_val_M,
                holdout_pdf_F = holdout_val_F, holdout_pdf_M = holdout_val_M,
                err_F = err_F, err_M = err_M,
                KL_div_val_F = KL_div_val_F, KL_div_val_M = KL_div_val_M,
                JS_div_val_F = JS_div_val_F, JS_div_val_M = JS_div_val_M,
                L1_dist_F = L1_dist_F, L1_dist_M = L1_dist_M,
                L2_dist_F = L2_dist_F, L2_dist_M = L2_dist_M))
}

# subnational data for handling zero counts for the clr transformation

replace_zero <- function(x) 
{
  if(any(x == 0)) cmultRepl(x, method = "CZM") else x
}

#################################
# Forecasting via two-stage FPCA
#################################

# object:
# h: forecast horizon
# level: confidence level
# B: bootstrap numbers
# fmethod: univariate time-series forecasting method, arima or ets

forecast.hdfpca <- function(object, h, level, B, fmethod)
{
    order = object$order
    r = object$r
    m = object$m
    p = object$p
    n = dim(object$y[[1]])[2]
    resid <- list()
    for(im in 1:m)
    {
        resid[[im]] <- object$y[[im]] - object$fitted[[im]]
    }
    mod.fore = load.fore <- list()
    for(io in 1:order)
    {
        load.fore[[io]] <- array(NA, dim = c(h, r))
        mod.fore[[io]] <- list()
        for(ir in 1:r)
        {
            if(fmethod == "arima")
            {
                mod <- auto.arima(object$model$model2[[io]]$coef[,ir])
            }
            else if(fmethod == "ets")
            {
                mod <- ets(object$model$model2[[io]]$coef[,ir])
            }
            mod.fore[[io]][[ir]] <- forecast(mod, h)
            load.fore[[io]][, ir] <- mod.fore[[io]][[ir]]$mean
        }
    }
    score.fore <- list()
    for(io in 1:order)
    {
        score.fore[[io]] <- object$model$model2[[io]]$basis[, 1] + object$model$model2[[io]]$basis[, 2:(1 + r)] %*% t(load.fore[[io]])
    }
    score <- list()
    for(im in 1:m)
    {
        score[[im]] <- sapply(score.fore, "[", ((im - 1) * h + 1):(im * h))
    }
    fun.fore <- list()
    if(h == 1)
    {
        for(im in 1:m)
        {
            fun.fore[[im]] <- object$model$model1[[im]]$basis[, 1] + object$model$model1[[im]]$basis[, 2:(1 + order)] %*% score[[im]]
        }
    }
    else
    {
        for(im in 1:m)
        {
            fun.fore[[im]] <- object$model$model1[[im]]$basis[, 1] + object$model$model1[[im]]$basis[, 2:(1 + order)] %*% t(score[[im]])
        }
    }
    return(structure(list(forecast = fun.fore), class = "forecast.hdfpca"))
}

#####################
# Forecasting HDFPCA
#####################

# fdata_F: female data
# fdata_M: male data
# horizon: forecast horizon
# first_order: 1st FPCA
# second_order: 2nd FPCA
# transformation: CDF or CLR

hdfpca_fun <- function(fdata_F, fdata_M, horizon, first_order, second_order, transformation, forecasting_method)
{
    n_year = nrow(fdata_F)
    forecast_val_F = forecast_val_M = matrix(NA, ncol(fdata_F), (21 - horizon))
    if(transformation == "direct")
    {
        for(ij in 1:(21 - horizon))
        {
            data_F = fdata_F[1:(n_year - 21 + ij),]
            data_M = fdata_M[1:(n_year - 21 + ij),]
            data_comb = list()
            data_comb[[1]] = t(data_F)
            data_comb[[2]] = t(data_M)
            
            fore_val = forecast.hdfpca(hdfpca(y = data_comb, order = first_order, r = second_order), h = horizon, fmethod = forecasting_method)$forecast
            forecast_val_F[,ij] = (fore_val[[1]])[,horizon]
            forecast_val_M[,ij] = (fore_val[[2]])[,horizon]
            rm(ij)
        }
    }
    else if(transformation == "clr")
    {
        for(ij in 1:(21 - horizon))
        {
            data_F = as.matrix(clr(fdata_F[1:(n_year - 21 + ij),]))
            data_M = as.matrix(clr(fdata_M[1:(n_year - 21 + ij),]))
            data_comb = list()
            data_comb[[1]] = t(data_F)
            data_comb[[2]] = t(data_M)
            
            fore_val = forecast.hdfpca(hdfpca(y = data_comb, order = first_order, r = second_order), h = horizon, fmethod = forecasting_method)$forecast
            forecast_val_F[,ij] = as.numeric(clrInv((fore_val[[1]])[,horizon]))
            forecast_val_M[,ij] = as.numeric(clrInv((fore_val[[2]])[,horizon]))
            rm(ij)
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
            
            fore_val = forecast.hdfpca(hdfpca(y = data_comb, order = first_order, r = second_order), h = horizon, fmethod = forecasting_method)$forecast
            fore_val_F = (fore_val[[1]])[,horizon]
            fore_val_M = (fore_val[[2]])[,horizon]
            
            data_cumsum_logit_fore_add_F = c(invlogit(fore_val_F), 1)
            data_cumsum_logit_fore_add_M = c(invlogit(fore_val_M), 1)
            
            data_cumsum_logit_fore_add_diff_F = c(data_cumsum_logit_fore_add_F[1], diff(data_cumsum_logit_fore_add_F))
            data_cumsum_logit_fore_add_diff_M = c(data_cumsum_logit_fore_add_M[1], diff(data_cumsum_logit_fore_add_M))
            
            forecast_val_F[,ijk] = data_cumsum_logit_fore_add_diff_F * 10^5
            forecast_val_M[,ijk] = data_cumsum_logit_fore_add_diff_M * 10^5
            rm(ijk); rm(data_F); rm(data_M)
        }
    }
    holdout_val_dum_F = t(matrix(fdata_F[(n_year - 20 + horizon):n_year,], length((n_year - 20 + horizon):n_year), ncol(fdata_F)))
    holdout_val_dum_M = t(matrix(fdata_M[(n_year - 20 + horizon):n_year,], length((n_year - 20 + horizon):n_year), ncol(fdata_F)))
    if(any(holdout_val_dum_F == 0))
    {
        holdout_val_F = replace(holdout_val_dum_F, which(holdout_val_dum_F == 0), 10^-5)
    }
    else
    {
        holdout_val_F = holdout_val_dum_F
    }
    if(any(holdout_val_dum_M == 0))
    {
        holdout_val_M = replace(holdout_val_dum_M, which(holdout_val_dum_M == 0), 10^-5)
    }
    else
    {
        holdout_val_M = holdout_val_dum_M
    }
    rm(holdout_val_dum_F); rm(holdout_val_dum_M)
    
    KL_div_val_F = JS_div_val_F = L1_dist_F = L2_dist_F =
    KL_div_val_M = JS_div_val_M = L1_dist_M = L2_dist_M = vector("numeric", (21 - horizon))
    for(ij in 1:(21 - horizon))
    {
        # symmetric KL dist
      
        KL_div_val_F[ij] = mean(KLdiv(cbind(forecast_val_F[,ij], holdout_val_F[,ij]))[2:3])
        
        # Jensen-Shannon dist
        
        JS_div_val_F[ij] = sqrt(mean(KLdiv(cbind(forecast_val_F[,ij], apply(cbind(forecast_val_F[,ij], holdout_val_F[,ij]), 1, geometric.mean)))[2:3]))
        
        # L1 dist
        
        L1_dist_F[ij] = wasserstein1d(a = forecast_val_F[,ij], b = holdout_val_F[,ij], p = 1)
        
        # L2 dist
        
        L2_dist_F[ij] = wasserstein1d(a = forecast_val_F[,ij], b = holdout_val_F[,ij], p = 2)
        
        # symmetric KL dist
        
        KL_div_val_M[ij] = mean(KLdiv(cbind(forecast_val_M[,ij], holdout_val_M[,ij]))[2:3])
        
        # Jensen-Shannon dist
        
        JS_div_val_M[ij] = sqrt(mean(KLdiv(cbind(forecast_val_M[,ij], apply(cbind(forecast_val_M[,ij], holdout_val_M[,ij]), 1, geometric.mean)))[2:3]))
        
        # L1 dist
        
        L1_dist_M[ij] = wasserstein1d(a = forecast_val_M[,ij], b = holdout_val_M[,ij], p = 1)

        # L2 dist
        
        L2_dist_M[ij] = wasserstein1d(a = forecast_val_M[,ij], b = holdout_val_M[,ij], p = 2)
        rm(ij)
    }
    err_F = c(mean(KL_div_val_F), mean(JS_div_val_F), sqrt(mean(L1_dist_F^2)), sqrt(mean(L2_dist_F^2)))
    err_M = c(mean(KL_div_val_M), mean(JS_div_val_M), sqrt(mean(L1_dist_M^2)), sqrt(mean(L2_dist_M^2)))
    
    return(list(forecast_pdf_F = forecast_val_F, forecast_pdf_M = forecast_val_M,
                holdout_pdf_F = holdout_val_F, holdout_pdf_M = holdout_val_M,
                err_F = err_F, err_M = err_M))
}

### Eigenratio $k$ selection method ###
# tau is the threshold of eigenvalue to cut, such as 0.001

select_K <- function(tau, eigenvalue)
{
    k_max = length(eigenvalue)
    k_all = rep(0, k_max-1)
    for(k in 1:(k_max-1))
    {
        k_all[k] = (eigenvalue[k+1]/eigenvalue[k])*ifelse(eigenvalue[k]/eigenvalue[1] > tau, 1, 0) + ifelse(eigenvalue[k]/eigenvalue[1] < tau, 1, 0)
    }
    K_hat = which.min(k_all)
    return(K_hat)
}

