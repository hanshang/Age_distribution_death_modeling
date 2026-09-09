###############
## alpha = 0.2
###############

## fmethod = "ets"

# Female

CDF_F_ARIMA_int_res_score = cbind(int_fore_subnational_err_F_EVR_ARIMA_mean[,3],
                                  MFTS_int_fore_subnational_err_F_EVR_ARIMA_mean[,3],
                                  MLFTS_int_fore_subnational_err_F_EVR_ARIMA_mean[,3],
                                  
                                  horizon_specific_hdfpca_int_fore_subnational_err_F_EVR_ARIMA[,3],
                                  FANOVA_FFM_F_arima_int_err_mean[,3],
                                  
                                  int_fore_subnational_err_F_K6_ARIMA_mean[,3],
                                  MFTS_int_fore_subnational_err_F_K6_ARIMA_mean[,3],
                                  MLFTS_int_fore_subnational_err_F_K6_ARIMA_mean[,3],
                                  
                                  horizon_gender_gap_arima_int_F_err_mean[,3],
                                  horizon_region_gap_arima_int_F_err_mean[,3],
                                  horizon_double_gap_arima_int_F_err_mean[,3])
colnames(CDF_F_ARIMA_int_res_score) = c("UFTS (EVR)", "MFTS (EVR)", "MLFTS (EVR)", "HDFPCA", "FANOVA+FFM",
                                        "UFTS (K=6)", "MFTS (K=6)", "MLFTS (K=6)", "Gender gap", "Region gap", "Double gap")

CDF_F_ARIMA_int_res_score_overall = rbind(CDF_F_ARIMA_int_res_score,
                                          apply(CDF_F_ARIMA_int_res_score, 2, mean),
                                          apply(CDF_F_ARIMA_int_res_score, 2, median))
rownames(CDF_F_ARIMA_int_res_score_overall) = c(1:20, "Mean", "Median")

# Male

CDF_M_ARIMA_int_res_score = cbind(int_fore_subnational_err_M_EVR_ARIMA_mean[,3],
                                  MFTS_int_fore_subnational_err_M_EVR_ARIMA_mean[,3],
                                  MLFTS_int_fore_subnational_err_M_EVR_ARIMA_mean[,3],
                                  
                                  horizon_specific_hdfpca_int_fore_subnational_err_M_EVR_ARIMA[,3],
                                  FANOVA_FFM_M_arima_int_err_mean[,3],
                                  
                                  int_fore_subnational_err_M_K6_ARIMA_mean[,3],
                                  MFTS_int_fore_subnational_err_M_K6_ARIMA_mean[,3],
                                  MLFTS_int_fore_subnational_err_M_K6_ARIMA_mean[,3],
                                  
                                  horizon_gender_gap_arima_int_M_err_mean[,3],
                                  horizon_region_gap_arima_int_M_err_mean[,3],
                                  horizon_double_gap_arima_int_M_err_mean[,3])
colnames(CDF_M_ARIMA_int_res_score) = c("UFTS (EVR)", "MFTS (EVR)", "MLFTS (EVR)", "HDFPCA", "FANOVA+FFM",
                                        "UFTS (K=6)", "MFTS (K=6)", "MLFTS (K=6)", "Gender gap", "Region gap", "Double gap")

CDF_M_ARIMA_int_res_score_overall = rbind(CDF_M_ARIMA_int_res_score,
                                          apply(CDF_M_ARIMA_int_res_score, 2, mean),
                                          apply(CDF_M_ARIMA_int_res_score, 2, median))
rownames(CDF_M_ARIMA_int_res_score_overall) = c(1:20, "Mean", "Median")

## prefecture

# Female

CDF_F_ARIMA_int_res_score_prefecture = cbind(int_fore_subnational_err_F_EVR_ARIMA_mean_prefecture[,3],
                                             MFTS_int_fore_subnational_err_F_EVR_ARIMA_mean_prefecture[,3],
                                             MLFTS_int_fore_subnational_err_F_EVR_ARIMA_mean_prefecture[,3],
                                            
                                             horizon_specific_hdfpca_int_fore_subnational_err_F_EVR_ARIMA_prefecture[,3],
                                             FANOVA_FFM_F_arima_int_err_mean_prefecture[,3],
                                            
                                             int_fore_subnational_err_F_K6_ARIMA_mean_prefecture[,3],
                                             MFTS_int_fore_subnational_err_F_K6_ARIMA_mean_prefecture[,3],
                                             MLFTS_int_fore_subnational_err_F_K6_ARIMA_mean_prefecture[,3],
                                            
                                             gender_gap_arima_int_F_err_mean[,3],
                                             region_gap_arima_int_F_err_mean[,3],
                                             double_gap_arima_int_F_err_mean[,3])
# Male

CDF_M_ARIMA_int_res_score_prefecture = cbind(int_fore_subnational_err_M_EVR_ARIMA_mean_prefecture[,3],
                                             MFTS_int_fore_subnational_err_M_EVR_ARIMA_mean_prefecture[,3],
                                             MLFTS_int_fore_subnational_err_M_EVR_ARIMA_mean_prefecture[,3],
                                            
                                             horizon_specific_hdfpca_int_fore_subnational_err_M_EVR_ARIMA_prefecture[,3],
                                             FANOVA_FFM_M_arima_int_err_mean_prefecture[,3],
                                            
                                             int_fore_subnational_err_M_K6_ARIMA_mean_prefecture[,3],
                                             MFTS_int_fore_subnational_err_M_K6_ARIMA_mean_prefecture[,3],
                                             MLFTS_int_fore_subnational_err_M_K6_ARIMA_mean_prefecture[,3],
                                            
                                             gender_gap_arima_int_M_err_mean[,3],
                                             region_gap_arima_int_M_err_mean[,3],
                                             double_gap_arima_int_M_err_mean[,3])
colnames(CDF_F_ARIMA_int_res_score_prefecture) = colnames(CDF_M_ARIMA_int_res_score_prefecture) = c("UFTS (EVR)", "MFTS (EVR)", "MLFTS (EVR)", "HDFPCA", "FANOVA+FFM",
               "UFTS (K=6)", "MFTS (K=6)", "MLFTS (K=6)", "Gender gap", "Region gap", "Double gap")


################
## alpha = 0.05
################

## fmethod = "arima"

# Female

CDF_F_ARIMA_int_res_score_95 = cbind(int_fore_subnational_err_F_EVR_ARIMA_alpha_0.95_mean[,3],
                                     MFTS_int_fore_subnational_err_F_EVR_ARIMA_alpha_0.95_mean[,3],
                                     MLFTS_int_fore_subnational_err_F_EVR_ARIMA_alpha_0.95_mean[,3],
                                     
                                     horizon_specific_hdfpca_int_fore_subnational_err_F_EVR_ARIMA_alpha_0.95[,3],
                                     FANOVA_FFM_F_arima_int_err_95_mean[,3],
                                     
                                     int_fore_subnational_err_F_K6_ARIMA_alpha_0.95_mean[,3],
                                     MFTS_int_fore_subnational_err_F_K6_ARIMA_alpha_0.95_mean[,3],
                                     MLFTS_int_fore_subnational_err_F_K6_ARIMA_alpha_0.95_mean[,3],
                                     
                                     horizon_gender_gap_arima_int_F_95_err_mean[,3],
                                     horizon_region_gap_arima_int_F_95_err_mean[,3],
                                     horizon_double_gap_arima_int_F_95_err_mean[,3])
colnames(CDF_F_ARIMA_int_res_score_95) = c("UFTS (EVR)", "MFTS (EVR)", "MLFTS (EVR)", "HDFPCA", "FANOVA+FFM",
                                           "UFTS (K=6)", "MFTS (K=6)", "MLFTS (K=6)", "Gender gap", "Region gap", "Double gap")

CDF_F_ARIMA_int_res_score_95_overall = rbind(CDF_F_ARIMA_int_res_score_95,
                                             apply(CDF_F_ARIMA_int_res_score_95, 2, mean),
                                             apply(CDF_F_ARIMA_int_res_score_95, 2, median))
rownames(CDF_F_ARIMA_int_res_score_95_overall) = c(1:20, "Mean", "Median")
  
# Male

CDF_M_ARIMA_int_res_score_95 = cbind(int_fore_subnational_err_M_EVR_ARIMA_alpha_0.95_mean[,3],
                                     MFTS_int_fore_subnational_err_M_EVR_ARIMA_alpha_0.95_mean[,3],
                                     MLFTS_int_fore_subnational_err_M_EVR_ARIMA_alpha_0.95_mean[,3],
                                     
                                     horizon_specific_hdfpca_int_fore_subnational_err_M_EVR_ARIMA_alpha_0.95[,3],
                                     FANOVA_FFM_M_arima_int_err_95_mean[,3],
                                     
                                     int_fore_subnational_err_M_K6_ARIMA_alpha_0.95_mean[,3],
                                     MFTS_int_fore_subnational_err_M_K6_ARIMA_alpha_0.95_mean[,3],
                                     MLFTS_int_fore_subnational_err_M_K6_ARIMA_alpha_0.95_mean[,3],
                                     
                                     horizon_gender_gap_arima_int_M_95_err_mean[,3],
                                     horizon_region_gap_arima_int_M_95_err_mean[,3],
                                     horizon_double_gap_arima_int_M_95_err_mean[,3])
colnames(CDF_M_ARIMA_int_res_score_95) = c("UFTS (EVR)", "MFTS (EVR)", "MLFTS (EVR)", "HDFPCA", "FANOVA+FFM",
                                           "UFTS (K=6)", "MFTS (K=6)", "MLFTS (K=6)", "Gender gap", "Region gap", "Double gap")

CDF_M_ARIMA_int_res_score_95_overall = rbind(CDF_M_ARIMA_int_res_score_95,
                                             apply(CDF_M_ARIMA_int_res_score_95, 2, mean),
                                             apply(CDF_M_ARIMA_int_res_score_95, 2, median))
rownames(CDF_M_ARIMA_int_res_score_95_overall) = c(1:20, "Mean", "Median")

which.min(colMeans(CDF_F_ARIMA_int_res_score)) # HDFPCA
which.min(colMeans(CDF_M_ARIMA_int_res_score)) # Region gap
which.min(colMeans(CDF_F_ARIMA_int_res_score_95)) # HDFPCA
which.min(colMeans(CDF_M_ARIMA_int_res_score_95)) # Region gap

## prefecture

# Female

CDF_F_ARIMA_int_res_score_95_prefecture = cbind(int_fore_subnational_err_F_EVR_ARIMA_alpha_0.95_mean_prefecture[,3],
                                                MFTS_int_fore_subnational_err_F_EVR_ARIMA_alpha_0.95_mean_prefecture[,3],
                                                MLFTS_int_fore_subnational_err_F_EVR_ARIMA_alpha_0.95_mean_prefecture[,3],
                                                
                                                horizon_specific_hdfpca_int_fore_subnational_err_F_EVR_ARIMA_alpha_0.95_prefecture[,3],
                                                FANOVA_FFM_F_arima_int_err_95_mean_prefecture[,3],
                                                
                                                int_fore_subnational_err_F_K6_ARIMA_alpha_0.95_mean_prefecture[,3],
                                                MFTS_int_fore_subnational_err_F_K6_ARIMA_alpha_0.95_mean_prefecture[,3],
                                                MLFTS_int_fore_subnational_err_F_K6_ARIMA_alpha_0.95_mean_prefecture[,3],
                                                
                                                gender_gap_arima_int_F_95_err_mean[,3],
                                                region_gap_arima_int_F_95_err_mean[,3],
                                                double_gap_arima_int_F_95_err_mean[,3])

# Male

CDF_M_ARIMA_int_res_score_95_prefecture = cbind(int_fore_subnational_err_M_EVR_ARIMA_alpha_0.95_mean_prefecture[,3],
                                                MFTS_int_fore_subnational_err_M_EVR_ARIMA_alpha_0.95_mean_prefecture[,3],
                                                MLFTS_int_fore_subnational_err_M_EVR_ARIMA_alpha_0.95_mean_prefecture[,3],
                                                
                                                horizon_specific_hdfpca_int_fore_subnational_err_M_EVR_ARIMA_alpha_0.95_prefecture[,3],
                                                FANOVA_FFM_M_arima_int_err_95_mean_prefecture[,3],
                                                
                                                int_fore_subnational_err_M_K6_ARIMA_alpha_0.95_mean_prefecture[,3],
                                                MFTS_int_fore_subnational_err_M_K6_ARIMA_alpha_0.95_mean_prefecture[,3],
                                                MLFTS_int_fore_subnational_err_M_K6_ARIMA_alpha_0.95_mean_prefecture[,3],
                                                
                                                gender_gap_arima_int_M_95_err_mean[,3],
                                                region_gap_arima_int_M_95_err_mean[,3],
                                                double_gap_arima_int_M_95_err_mean[,3])
colnames(CDF_F_ARIMA_int_res_score_95_prefecture) = colnames(CDF_M_ARIMA_int_res_score_95_prefecture) = c("UFTS (EVR)", "MFTS (EVR)", "MLFTS (EVR)", "HDFPCA", "FANOVA+FFM",
                                                "UFTS (K=6)", "MFTS (K=6)", "MLFTS (K=6)", "Gender gap", "Region gap", "Double gap")

#########
# xtable
#########

xtable(rbind(CDF_F_ARIMA_int_res_score_overall[c(1, 5, 10, 15, 20:22),],
             CDF_M_ARIMA_int_res_score_overall[c(1, 5, 10, 15, 20:22),],
             CDF_F_ARIMA_int_res_score_95_overall[c(1, 5, 10, 15, 20:22),],
             CDF_M_ARIMA_int_res_score_95_overall[c(1, 5, 10, 15, 20:22),]), digits = 0)

