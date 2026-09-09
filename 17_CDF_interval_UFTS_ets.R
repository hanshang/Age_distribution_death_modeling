##################
# load R packages
##################

source("auxiliary_interval.R")
source("load_packages.R")


########
### CDF
########

## level_sig = 0.8

# EVR

int_fore_subnational_err_F_EVR_ETS = int_fore_subnational_err_M_EVR_ETS = array(NA, dim = c(20, 3, 47), dimnames = list(1:20, c("ECP", "CPD", "MIS"), state))
int_fore_subnational_err_F_EVR_ETS_tune_para = int_fore_subnational_err_F_EVR_ETS_tune_para_obj = 
int_fore_subnational_err_M_EVR_ETS_tune_para = int_fore_subnational_err_M_EVR_ETS_tune_para_obj = matrix(NA, 20, 47)
for(ij in 1:47)
{
    for(iw in 1:20)
    {
        ## Female
        
        dum = interval_fore_subnational_cdf(fdata = female_prefecture_dx[[ij]], method_ncomp = "EVR",
                                            horizon = iw, fore_method = "CDF", uni_fore_method = "ets",
                                            level_sig = 0.8)
        int_fore_subnational_err_F_EVR_ETS[iw,,ij] = dum$int_err
        int_fore_subnational_err_F_EVR_ETS_tune_para[iw,ij] = dum$tune_para_find
        int_fore_subnational_err_F_EVR_ETS_tune_para_obj[iw,ij] = dum$tune_para_find_obj
        rm(dum)
        
        ## Male
        
        dum = interval_fore_subnational_cdf(fdata = male_prefecture_dx[[ij]], method_ncomp = "EVR",
                                            horizon = iw, fore_method = "CDF", uni_fore_method = "ets",
                                            level_sig = 0.8)
        int_fore_subnational_err_M_EVR_ETS[iw,,ij] = dum$int_err
        int_fore_subnational_err_M_EVR_ETS_tune_para[iw,ij] = dum$tune_para_find
        int_fore_subnational_err_M_EVR_ETS_tune_para_obj[iw,ij] = dum$tune_para_find_obj
        rm(dum); print(iw); rm(iw)
    }
    print(ij); rm(ij)
}

int_fore_subnational_err_F_EVR_ETS_mean = apply(int_fore_subnational_err_F_EVR_ETS, c(1, 2), mean)
int_fore_subnational_err_M_EVR_ETS_mean = apply(int_fore_subnational_err_M_EVR_ETS, c(1, 2), mean)

colnames(int_fore_subnational_err_F_EVR_ETS_mean) = colnames(int_fore_subnational_err_M_EVR_ETS_mean) = c("ECP", "CPD", "score")
rownames(int_fore_subnational_err_F_EVR_ETS_mean) = rownames(int_fore_subnational_err_M_EVR_ETS_mean) = 1:20

int_fore_subnational_err_F_EVR_ETS_mean_prefecture = t(apply(int_fore_subnational_err_F_EVR_ETS, c(2, 3), mean))
int_fore_subnational_err_M_EVR_ETS_mean_prefecture = t(apply(int_fore_subnational_err_M_EVR_ETS, c(2, 3), mean))

# K = 6

int_fore_subnational_err_F_K6_ETS = int_fore_subnational_err_M_K6_ETS = array(NA, dim = c(20, 3, 47), dimnames = list(1:20, c("ECP", "CPD", "MIS"), state))
int_fore_subnational_err_F_K6_ETS_tune_para = int_fore_subnational_err_F_K6_ETS_tune_para_obj = 
int_fore_subnational_err_M_K6_ETS_tune_para = int_fore_subnational_err_M_K6_ETS_tune_para_obj = matrix(NA, 20, 47)
for(ij in 1:47)
{
    for(iw in 1:20)
    {
        ## Female
        
        dum = interval_fore_subnational_cdf(fdata = female_prefecture_dx[[ij]], method_ncomp = "provide",
                                            horizon = iw, fore_method = "CDF", uni_fore_method = "ets",
                                            level_sig = 0.8)
        int_fore_subnational_err_F_K6_ETS[iw,,ij] = dum$int_err
        int_fore_subnational_err_F_K6_ETS_tune_para[iw,ij] = dum$tune_para_find
        int_fore_subnational_err_F_K6_ETS_tune_para_obj[iw,ij] = dum$tune_para_find_obj
        rm(dum)
        
        ## Male
        
        dum = interval_fore_subnational_cdf(fdata = male_prefecture_dx[[ij]], method_ncomp = "provide",
                                            horizon = iw, fore_method = "CDF", uni_fore_method = "ets",
                                            level_sig = 0.8)
        int_fore_subnational_err_M_K6_ETS[iw,,ij] = dum$int_err
        int_fore_subnational_err_M_K6_ETS_tune_para[iw,ij] = dum$tune_para_find
        int_fore_subnational_err_M_K6_ETS_tune_para_obj[iw,ij] = dum$tune_para_find_obj
        rm(dum); print(iw); rm(iw)
    }
    print(ij); rm(ij)
}

int_fore_subnational_err_F_K6_ETS_mean = apply(int_fore_subnational_err_F_K6_ETS, c(1, 2), mean)
int_fore_subnational_err_M_K6_ETS_mean = apply(int_fore_subnational_err_M_K6_ETS, c(1, 2), mean)

colnames(int_fore_subnational_err_F_K6_ETS_mean) = colnames(int_fore_subnational_err_M_K6_ETS_mean) = c("ECP", "CPD", "score")
rownames(int_fore_subnational_err_F_K6_ETS_mean) = rownames(int_fore_subnational_err_M_K6_ETS_mean) = 1:20

int_fore_subnational_err_F_K6_ETS_mean_prefecture = t(apply(int_fore_subnational_err_F_K6_ETS, c(2, 3), mean))
int_fore_subnational_err_M_K6_ETS_mean_prefecture = t(apply(int_fore_subnational_err_M_K6_ETS, c(2, 3), mean))

## level_sig = 0.95

# EVR

int_fore_subnational_err_F_EVR_ETS_alpha_0.95 = int_fore_subnational_err_M_EVR_ETS_alpha_0.95 = array(NA, dim = c(20, 3, 47), dimnames = list(1:20, c("ECP", "CPD", "MIS"), state))
int_fore_subnational_err_F_EVR_ETS_tune_para_alpha_0.95 = int_fore_subnational_err_F_EVR_ETS_tune_para_obj_alpha_0.95 = 
int_fore_subnational_err_M_EVR_ETS_tune_para_alpha_0.95 = int_fore_subnational_err_M_EVR_ETS_tune_para_obj_alpha_0.95 = matrix(NA, 20, 47)
for(ij in 1:47)
{
    for(iw in 1:20)
    {
        ## (F)
        
        dum = interval_fore_subnational_cdf(fdata = female_prefecture_dx[[ij]], method_ncomp = "EVR",
                                            horizon = iw, fore_method = "CDF", uni_fore_method = "ets",
                                            level_sig = 0.95)
        int_fore_subnational_err_F_EVR_ETS_alpha_0.95[iw,,ij] = dum$int_err
        int_fore_subnational_err_F_EVR_ETS_tune_para_alpha_0.95[iw,ij] = dum$tune_para_find
        int_fore_subnational_err_F_EVR_ETS_tune_para_obj_alpha_0.95[iw,ij] = dum$tune_para_find_obj
        rm(dum)
        
        ## (M)
        
        dum = interval_fore_subnational_cdf(fdata = male_prefecture_dx[[ij]], method_ncomp = "EVR",
                                            horizon = iw, fore_method = "CDF", uni_fore_method = "ets",
                                            level_sig = 0.95)
        int_fore_subnational_err_M_EVR_ETS_alpha_0.95[iw,,ij] = dum$int_err
        int_fore_subnational_err_M_EVR_ETS_tune_para_alpha_0.95[iw,ij] = dum$tune_para_find
        int_fore_subnational_err_M_EVR_ETS_tune_para_obj_alpha_0.95[iw,ij] = dum$tune_para_find_obj
        rm(dum); print(iw); rm(iw)
    }
    print(ij); rm(ij)
}

int_fore_subnational_err_F_EVR_ETS_alpha_0.95_mean = apply(int_fore_subnational_err_F_EVR_ETS_alpha_0.95, c(1, 2), mean)
int_fore_subnational_err_M_EVR_ETS_alpha_0.95_mean = apply(int_fore_subnational_err_M_EVR_ETS_alpha_0.95, c(1, 2), mean)

colnames(int_fore_subnational_err_F_EVR_ETS_alpha_0.95_mean) = colnames(int_fore_subnational_err_M_EVR_ETS_alpha_0.95_mean) = c("ECP", "CPD", "score")
rownames(int_fore_subnational_err_F_EVR_ETS_alpha_0.95_mean) = rownames(int_fore_subnational_err_M_EVR_ETS_alpha_0.95_mean) = 1:20

int_fore_subnational_err_F_EVR_ETS_alpha_0.95_mean_prefecture = t(apply(int_fore_subnational_err_F_EVR_ETS_alpha_0.95, c(2, 3), mean))
int_fore_subnational_err_M_EVR_ETS_alpha_0.95_mean_prefecture = t(apply(int_fore_subnational_err_M_EVR_ETS_alpha_0.95, c(2, 3), mean))

# K = 6

int_fore_subnational_err_F_K6_ETS_alpha_0.95 = int_fore_subnational_err_M_K6_ETS_alpha_0.95 = array(NA, dim = c(20, 3, 47), dimnames = list(1:20, c("ECP", "CPD", "MIS"), state))
int_fore_subnational_err_F_K6_ETS_tune_para_alpha_0.95 = int_fore_subnational_err_F_K6_ETS_tune_para_obj_alpha_0.95 = 
int_fore_subnational_err_M_K6_ETS_tune_para_alpha_0.95 = int_fore_subnational_err_M_K6_ETS_tune_para_obj_alpha_0.95 = matrix(NA, 20, 47)
for(ij in 1:47)
{
    for(iw in 1:20)
    {
        ## (F)
        
        dum = interval_fore_subnational_cdf(fdata = female_prefecture_dx[[ij]], method_ncomp = "provide",
                                            horizon = iw, fore_method = "CDF", uni_fore_method = "ets",
                                            level_sig = 0.95)
        int_fore_subnational_err_F_K6_ETS_alpha_0.95[iw,,ij] = dum$int_err
        int_fore_subnational_err_F_K6_ETS_tune_para_alpha_0.95[iw,ij] = dum$tune_para_find
        int_fore_subnational_err_F_K6_ETS_tune_para_obj_alpha_0.95[iw,ij] = dum$tune_para_find_obj
        rm(dum)
        
        ## (M)
        
        dum = interval_fore_subnational_cdf(fdata = male_prefecture_dx[[ij]], method_ncomp = "provide",
                                            horizon = iw, fore_method = "CDF", uni_fore_method = "ets",
                                            level_sig = 0.95)
        int_fore_subnational_err_M_K6_ETS_alpha_0.95[iw,,ij] = dum$int_err
        int_fore_subnational_err_M_K6_ETS_tune_para_alpha_0.95[iw,ij] = dum$tune_para_find
        int_fore_subnational_err_M_K6_ETS_tune_para_obj_alpha_0.95[iw,ij] = dum$tune_para_find_obj
        rm(dum); print(iw); rm(iw)
    }
    print(ij); rm(ij)
}

int_fore_subnational_err_F_K6_ETS_alpha_0.95_mean = apply(int_fore_subnational_err_F_K6_ETS_alpha_0.95, c(1, 2), mean)
int_fore_subnational_err_M_K6_ETS_alpha_0.95_mean = apply(int_fore_subnational_err_M_K6_ETS_alpha_0.95, c(1, 2), mean)

colnames(int_fore_subnational_err_F_K6_ETS_alpha_0.95_mean) = colnames(int_fore_subnational_err_M_K6_ETS_alpha_0.95_mean) = c("ECP", "CPD", "score")
rownames(int_fore_subnational_err_F_K6_ETS_alpha_0.95_mean) = rownames(int_fore_subnational_err_M_K6_ETS_alpha_0.95_mean) = 1:20

int_fore_subnational_err_F_K6_ETS_alpha_0.95_mean_prefecture = t(apply(int_fore_subnational_err_F_K6_ETS_alpha_0.95, c(2, 3), mean))
int_fore_subnational_err_M_K6_ETS_alpha_0.95_mean_prefecture = t(apply(int_fore_subnational_err_M_K6_ETS_alpha_0.95, c(2, 3), mean))

