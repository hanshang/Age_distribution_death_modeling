######
# CPD
######

## alpha = 0.2

# Female

clr_F_ARIMA_int_res_score = cbind(horizon_specific_int_fore_subnational_err_F_EVR_ARIMA_CLR[,3],
                                  horizon_specific_MFTS_int_fore_subnational_err_F_EVR_ARIMA_CLR[,3],
                                  horizon_specific_MLFTS_int_fore_subnational_err_F_EVR_ARIMA_CLR[,3],
                                  
                                  horizon_specific_hdfpca_int_fore_subnational_err_F_EVR_ARIMA_CLR[,3],
                                  FANOVA_FFM_F_arima_int_err_mean_CLR[,3],
                                  
                                  horizon_specific_int_fore_subnational_err_F_K6_ARIMA_CLR[,3],
                                  horizon_specific_MFTS_int_fore_subnational_err_F_K6_ARIMA_CLR[,3],
                                  horizon_specific_MLFTS_int_fore_subnational_err_F_K6_ARIMA_CLR[,3],
                                  
                                  horizon_clr_gender_gap_arima_int_F_err_mean[,3],
                                  horizon_clr_region_gap_arima_int_F_err_mean[,3],
                                  horizon_clr_double_gap_arima_int_F_err_mean[,3])
colnames(clr_F_ARIMA_int_res_score) = c("UFTS (EVR)", "MFTS (EVR)", "MLFTS (EVR)", "HDFPCA", "FANOVA+FFM",
                                        "UFTS (K=6)", "MFTS (K=6)", "MLFTS (K=6)", "Gender gap", "Region gap", "Double gap")

clr_F_ARIMA_int_res_score_overall = rbind(clr_F_ARIMA_int_res_score,
                                          apply(clr_F_ARIMA_int_res_score, 2, mean),
                                          apply(clr_F_ARIMA_int_res_score, 2, median))
rownames(clr_F_ARIMA_int_res_score_overall) = c(1:20, "Mean", "Median")

# Male

clr_M_ARIMA_int_res_score = cbind(horizon_specific_int_fore_subnational_err_M_EVR_ARIMA_CLR[,3],
                                  horizon_specific_MFTS_int_fore_subnational_err_M_EVR_ARIMA_CLR[,3],
                                  horizon_specific_MLFTS_int_fore_subnational_err_M_EVR_ARIMA_CLR[,3],
                                  
                                  horizon_specific_hdfpca_int_fore_subnational_err_M_EVR_ARIMA_CLR[,3],
                                  FANOVA_FFM_M_arima_int_err_mean_CLR[,3],
                                  
                                  horizon_specific_int_fore_subnational_err_M_K6_ARIMA_CLR[,3],
                                  horizon_specific_MFTS_int_fore_subnational_err_M_K6_ARIMA_CLR[,3],
                                  horizon_specific_MLFTS_int_fore_subnational_err_M_K6_ARIMA_CLR[,3],
                                  
                                  horizon_clr_gender_gap_arima_int_M_err_mean[,3],
                                  horizon_clr_region_gap_arima_int_M_err_mean[,3],
                                  horizon_clr_double_gap_arima_int_M_err_mean[,3])
colnames(clr_M_ARIMA_int_res_score) = c("UFTS (EVR)", "MFTS (EVR)", "MLFTS (EVR)", "HDFPCA", "FANOVA+FFM",
                                        "UFTS (K=6)", "MFTS (K=6)", "MLFTS (K=6)", "Gender gap", "Region gap", "Double gap")

clr_M_ARIMA_int_res_score_overall = rbind(clr_M_ARIMA_int_res_score, 
                                          apply(clr_M_ARIMA_int_res_score, 2, mean),
                                          apply(clr_M_ARIMA_int_res_score, 2, median))
rownames(clr_M_ARIMA_int_res_score_overall) = c(1:20, "Mean", "Median")

## Prefectures

# Female

clr_F_ARIMA_int_res_score_prefecture = cbind(horizon_specific_int_fore_subnational_err_F_EVR_ARIMA_CLR_prefecture[,3],
                                             horizon_specific_MFTS_int_fore_subnational_err_F_EVR_ARIMA_CLR_prefecture[,3],
                                             horizon_specific_MLFTS_int_fore_subnational_err_F_EVR_ARIMA_CLR_prefecture[,3],
                                             
                                             horizon_specific_hdfpca_int_fore_subnational_err_F_EVR_ARIMA_CLR_prefecture[,3],
                                             FANOVA_FFM_F_arima_int_err_mean_CLR_prefecture[,3],
                                             
                                             horizon_specific_int_fore_subnational_err_F_K6_ARIMA_CLR_prefecture[,3],
                                             horizon_specific_MFTS_int_fore_subnational_err_F_K6_ARIMA_CLR_prefecture[,3],
                                             horizon_specific_MLFTS_int_fore_subnational_err_F_K6_ARIMA_CLR_prefecture[,3],
                                             
                                             clr_gender_gap_arima_int_F_err_mean[,3],
                                             clr_region_gap_arima_int_F_err_mean[,3],
                                             clr_double_gap_arima_int_F_err_mean[,3])

# Male

clr_M_ARIMA_int_res_score_prefecture = cbind(horizon_specific_int_fore_subnational_err_M_EVR_ARIMA_CLR_prefecture[,3],
                                             horizon_specific_MFTS_int_fore_subnational_err_M_EVR_ARIMA_CLR_prefecture[,3],
                                             horizon_specific_MLFTS_int_fore_subnational_err_M_EVR_ARIMA_CLR_prefecture[,3],
                                             
                                             horizon_specific_hdfpca_int_fore_subnational_err_M_EVR_ARIMA_CLR_prefecture[,3],
                                             FANOVA_FFM_M_arima_int_err_mean_CLR_prefecture[,3],
                                             
                                             horizon_specific_int_fore_subnational_err_M_K6_ARIMA_CLR_prefecture[,3],
                                             horizon_specific_MFTS_int_fore_subnational_err_M_K6_ARIMA_CLR_prefecture[,3],
                                             horizon_specific_MLFTS_int_fore_subnational_err_M_K6_ARIMA_CLR_prefecture[,3],
                                             
                                             clr_gender_gap_arima_int_M_err_mean[,3],
                                             clr_region_gap_arima_int_M_err_mean[,3],
                                             clr_double_gap_arima_int_M_err_mean[,3])
colnames(clr_F_ARIMA_int_res_score_prefecture) = colnames(clr_M_ARIMA_int_res_score_prefecture) = c("UFTS (EVR)", "MFTS (EVR)", "MLFTS (EVR)", "HDFPCA", "FANOVA+FFM",
                                                                                                    "UFTS (K=6)", "MFTS (K=6)", "MLFTS (K=6)", "Gender gap", "Region gap", "Double gap")

## alpha = 0.05

# Female

clr_F_ARIMA_int_res_score_95 = cbind(horizon_specific_int_fore_subnational_err_F_EVR_ARIMA_CLR_alpha_0.95[,3],
                                     horizon_specific_MFTS_int_fore_subnational_err_F_EVR_ARIMA_CLR_95[,3],
                                     horizon_specific_MLFTS_int_fore_subnational_err_F_EVR_ARIMA_CLR_95[,3],
                                     
                                     horizon_specific_hdfpca_int_fore_subnational_err_F_EVR_ARIMA_alpha_0.95_CLR[,3],
                                     FANOVA_FFM_F_arima_int_err_mean_95_CLR[,3],
                                     
                                     horizon_specific_int_fore_subnational_err_F_K6_ARIMA_CLR_alpha_0.95[,3],
                                     horizon_specific_MFTS_int_fore_subnational_err_F_K6_ARIMA_CLR_95[,3],
                                     horizon_specific_MLFTS_int_fore_subnational_err_F_K6_ARIMA_CLR_95[,3],
                                     
                                     horizon_clr_gender_gap_arima_int_F_95_err_mean[,3],
                                     horizon_clr_region_gap_arima_int_F_95_err_mean[,3],
                                     horizon_clr_double_gap_arima_int_F_95_err_mean[,3])
colnames(clr_F_ARIMA_int_res_score_95) = c("UFTS (EVR)", "MFTS (EVR)", "MLFTS (EVR)", "HDFPCA", "FANOVA+FFM",
                                           "UFTS (K=6)", "MFTS (K=6)", "MLFTS (K=6)", "Gender gap", "Region gap", "Double gap")

clr_F_ARIMA_int_res_score_95_overall = rbind(clr_F_ARIMA_int_res_score_95,
                                             apply(clr_F_ARIMA_int_res_score_95, 2, mean),
                                             apply(clr_F_ARIMA_int_res_score_95, 2, median))
rownames(clr_F_ARIMA_int_res_score_95_overall) = c(1:20, "Mean", "Median")

# Male

clr_M_ARIMA_int_res_score_95 = cbind(horizon_specific_int_fore_subnational_err_M_EVR_ARIMA_CLR_alpha_0.95[,3],
                                     horizon_specific_MFTS_int_fore_subnational_err_M_EVR_ARIMA_CLR_95[,3],
                                     horizon_specific_MLFTS_int_fore_subnational_err_M_EVR_ARIMA_CLR_95[,3],
                                     
                                     horizon_specific_hdfpca_int_fore_subnational_err_M_EVR_ARIMA_alpha_0.95_CLR[,3],
                                     FANOVA_FFM_M_arima_int_err_mean_95_CLR[,3],
                                     
                                     horizon_specific_int_fore_subnational_err_M_K6_ARIMA_CLR_alpha_0.95[,3],
                                     horizon_specific_MFTS_int_fore_subnational_err_M_K6_ARIMA_CLR_95[,3],
                                     horizon_specific_MLFTS_int_fore_subnational_err_M_K6_ARIMA_CLR_95[,3],
                                     
                                     horizon_clr_gender_gap_arima_int_M_95_err_mean[,3],
                                     horizon_clr_region_gap_arima_int_M_95_err_mean[,3],
                                     horizon_clr_double_gap_arima_int_M_95_err_mean[,3])
colnames(clr_M_ARIMA_int_res_score_95) = c("UFTS (EVR)", "MFTS (EVR)", "MLFTS (EVR)", "HDFPCA", "FANOVA+FFM",
                                           "UFTS (K=6)", "MFTS (K=6)", "MLFTS (K=6)", "Gender gap", "Region gap", "Double gap")

clr_M_ARIMA_int_res_score_95_overall = rbind(clr_M_ARIMA_int_res_score_95,
                                             apply(clr_M_ARIMA_int_res_score_95, 2, mean),
                                             apply(clr_M_ARIMA_int_res_score_95, 2, median))
rownames(clr_M_ARIMA_int_res_score_95_overall) = c(1:20, "Mean", "Median")

which.min(colMeans(clr_F_ARIMA_int_res_score)) # HDFPCA
which.min(colMeans(clr_M_ARIMA_int_res_score)) # Region gap
which.min(colMeans(clr_F_ARIMA_int_res_score_95)) # HDFPCA
which.min(colMeans(clr_M_ARIMA_int_res_score_95)) # Region gap

## Prefectures


# Female

clr_F_ARIMA_int_res_score_95_prefecture = cbind(horizon_specific_int_fore_subnational_err_F_EVR_ARIMA_CLR_alpha_0.95_prefecture[,3],
                                                horizon_specific_MFTS_int_fore_subnational_err_F_EVR_ARIMA_CLR_95_prefecture[,3],
                                                horizon_specific_MLFTS_int_fore_subnational_err_F_EVR_ARIMA_CLR_prefecture_95[,3],
                                                
                                                horizon_specific_hdfpca_int_fore_subnational_err_F_EVR_ARIMA_alpha_0.95_CLR_prefecture[,3],
                                                FANOVA_FFM_F_arima_int_err_mean_95_CLR_prefecture[,3],
                                                
                                                horizon_specific_int_fore_subnational_err_F_K6_ARIMA_CLR_alpha_0.95_prefecture[,3],
                                                horizon_specific_MFTS_int_fore_subnational_err_F_K6_ARIMA_CLR_95_prefecture[,3],
                                                horizon_specific_MLFTS_int_fore_subnational_err_F_K6_ARIMA_CLR_95_prefecture[,3],
                                                
                                                clr_gender_gap_arima_int_F_95_err_mean[,3],
                                                clr_region_gap_arima_int_F_95_err_mean[,3],
                                                clr_double_gap_arima_int_F_95_err_mean[,3])
# Male

clr_M_ARIMA_int_res_score_95_prefecture = cbind(horizon_specific_int_fore_subnational_err_M_EVR_ARIMA_CLR_alpha_0.95_prefecture[,3],
                                                horizon_specific_MFTS_int_fore_subnational_err_M_EVR_ARIMA_CLR_95_prefecture[,3],
                                                horizon_specific_MLFTS_int_fore_subnational_err_M_EVR_ARIMA_CLR_prefecture_95[,3],
                                                
                                                horizon_specific_hdfpca_int_fore_subnational_err_M_EVR_ARIMA_alpha_0.95_CLR_prefecture[,3],
                                                FANOVA_FFM_M_arima_int_err_mean_95_CLR_prefecture[,3],
                                                
                                                horizon_specific_int_fore_subnational_err_M_K6_ARIMA_CLR_alpha_0.95_prefecture[,3],
                                                horizon_specific_MFTS_int_fore_subnational_err_M_K6_ARIMA_CLR_95_prefecture[,3],
                                                horizon_specific_MLFTS_int_fore_subnational_err_M_K6_ARIMA_CLR_95_prefecture[,3],
                                                
                                                clr_gender_gap_arima_int_M_95_err_mean[,3],
                                                clr_region_gap_arima_int_M_95_err_mean[,3],
                                                clr_double_gap_arima_int_M_95_err_mean[,3])
colnames(clr_F_ARIMA_int_res_score_95_prefecture) = colnames(clr_M_ARIMA_int_res_score_95_prefecture) = c("UFTS (EVR)", "MFTS (EVR)", "MLFTS (EVR)", "HDFPCA", "FANOVA+FFM",
                                                                                                          "UFTS (K=6)", "MFTS (K=6)", "MLFTS (K=6)", "Gender gap", "Region gap", "Double gap")

#########
# xtable
#########

xtable(rbind(clr_F_ARIMA_int_res_score_overall[c(1, 5, 10, 15, 20:22),],
             clr_M_ARIMA_int_res_score_overall[c(1, 5, 10, 15, 20:22),],
             clr_F_ARIMA_int_res_score_95_overall[c(1, 5, 10, 15, 20:22),],
             clr_M_ARIMA_int_res_score_95_overall[c(1, 5, 10, 15, 20:22),]), digits = 0)

