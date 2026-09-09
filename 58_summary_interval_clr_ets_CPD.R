## alpha = 0.2

# Female

clr_F_ETS_int_res_CPD = cbind(horizon_specific_int_fore_subnational_err_F_EVR_ETS_CLR[,2],
                              horizon_specific_MFTS_int_fore_subnational_err_F_EVR_ETS_CLR[,2],
                              horizon_specific_MLFTS_int_fore_subnational_err_F_EVR_ETS_CLR[,2],
                                         
                              horizon_specific_hdfpca_int_fore_subnational_err_F_EVR_ETS_CLR[,2],
                              FANOVA_FFM_F_ets_int_err_mean_CLR[,2],
                                         
                              horizon_specific_int_fore_subnational_err_F_K6_ETS_CLR[,2],
                              horizon_specific_MFTS_int_fore_subnational_err_F_K6_ETS_CLR[,2],
                              horizon_specific_MLFTS_int_fore_subnational_err_F_K6_ETS_CLR[,2],
                                         
                              horizon_clr_gender_gap_ets_int_F_err_mean[,2],
                              horizon_clr_region_gap_ets_int_F_err_mean[,2],
                              horizon_clr_double_gap_ets_int_F_err_mean[,2])
colnames(clr_F_ETS_int_res_CPD) = c("UFTS (EVR)", "MFTS (EVR)", "MLFTS (EVR)", "HDFPCA", "FANOVA+FFM",
                                    "UFTS (K=6)", "MFTS (K=6)", "MLFTS (K=6)", "Gender gap", "Region gap", "Double gap")

clr_F_ETS_int_res_CPD_overall = rbind(clr_F_ETS_int_res_CPD,
                                      apply(clr_F_ETS_int_res_CPD, 2, mean),
                                      apply(clr_F_ETS_int_res_CPD, 2, median))
rownames(clr_F_ETS_int_res_CPD_overall) = c(1:20, "Mean", "Median")

# Male

clr_M_ETS_int_res_CPD = cbind(horizon_specific_int_fore_subnational_err_M_EVR_ETS_CLR[,2],
                              horizon_specific_MFTS_int_fore_subnational_err_M_EVR_ETS_CLR[,2],
                              horizon_specific_MLFTS_int_fore_subnational_err_M_EVR_ETS_CLR[,2],
                                         
                              horizon_specific_hdfpca_int_fore_subnational_err_M_EVR_ETS_CLR[,2],
                              FANOVA_FFM_M_ets_int_err_mean_CLR[,2],
                                         
                              horizon_specific_int_fore_subnational_err_M_K6_ETS_CLR[,2],
                              horizon_specific_MFTS_int_fore_subnational_err_M_K6_ETS_CLR[,2],
                              horizon_specific_MLFTS_int_fore_subnational_err_M_K6_ETS_CLR[,2],
                                         
                              horizon_clr_gender_gap_ets_int_M_err_mean[,2],
                              horizon_clr_region_gap_ets_int_M_err_mean[,2],
                              horizon_clr_double_gap_ets_int_M_err_mean[,2])
colnames(clr_M_ETS_int_res_CPD) = c("UFTS (EVR)", "MFTS (EVR)", "MLFTS (EVR)", "HDFPCA", "FANOVA+FFM",
                                    "UFTS (K=6)", "MFTS (K=6)", "MLFTS (K=6)", "Gender gap", "Region gap", "Double gap")

clr_M_ETS_int_res_CPD_overall = rbind(clr_M_ETS_int_res_CPD, 
                                      apply(clr_M_ETS_int_res_CPD, 2, mean),
                                      apply(clr_M_ETS_int_res_CPD, 2, median))
rownames(clr_M_ETS_int_res_CPD_overall) = c(1:20, "Mean", "Median")

## prefectures

# Female

clr_F_ETS_int_res_CPD_prefecture = cbind(horizon_specific_int_fore_subnational_err_F_EVR_ETS_CLR_prefecture[,2],
                                         horizon_specific_MFTS_int_fore_subnational_err_F_EVR_ETS_CLR_prefecture[,2],
                                         horizon_specific_MLFTS_int_fore_subnational_err_F_EVR_ETS_CLR_prefecture[,2],
                                         
                                         horizon_specific_hdfpca_int_fore_subnational_err_F_EVR_ETS_CLR_prefecture[,2],
                                         FANOVA_FFM_F_ets_int_err_mean_CLR_prefecture[,2],
                                         
                                         horizon_specific_int_fore_subnational_err_F_K6_ETS_CLR_prefecture[,2],
                                         horizon_specific_MFTS_int_fore_subnational_err_F_K6_ETS_CLR_prefecture[,2],
                                         horizon_specific_MLFTS_int_fore_subnational_err_F_K6_ETS_CLR_prefecture[,2],
                                         
                                         clr_gender_gap_ets_int_F_err_mean[,2],
                                         clr_region_gap_ets_int_F_err_mean[,2],
                                         clr_double_gap_ets_int_F_err_mean[,2])

# Male

clr_M_ETS_int_res_CPD_prefecture = cbind(horizon_specific_int_fore_subnational_err_M_EVR_ETS_CLR_prefecture[,2],
                                         horizon_specific_MFTS_int_fore_subnational_err_M_EVR_ETS_CLR_prefecture[,2],
                                         horizon_specific_MLFTS_int_fore_subnational_err_M_EVR_ETS_CLR_prefecture[,2],
                                         
                                         horizon_specific_hdfpca_int_fore_subnational_err_M_EVR_ETS_CLR_prefecture[,2],
                                         FANOVA_FFM_M_ets_int_err_mean_CLR_prefecture[,2],
                                         
                                         horizon_specific_int_fore_subnational_err_M_K6_ETS_CLR_prefecture[,2],
                                         horizon_specific_MFTS_int_fore_subnational_err_M_K6_ETS_CLR_prefecture[,2],
                                         horizon_specific_MLFTS_int_fore_subnational_err_M_K6_ETS_CLR_prefecture[,2],
                                         
                                         clr_gender_gap_ets_int_M_err_mean[,2],
                                         clr_region_gap_ets_int_M_err_mean[,2],
                                         clr_double_gap_ets_int_M_err_mean[,2])
colnames(clr_F_ETS_int_res_CPD_prefecture) = colnames(clr_M_ETS_int_res_CPD_prefecture) = c("UFTS (EVR)", "MFTS (EVR)", "MLFTS (EVR)", "HDFPCA", "FANOVA+FFM",
                                                                                            "UFTS (K=6)", "MFTS (K=6)", "MLFTS (K=6)", "Gender gap", "Region gap", "Double gap")


## alpha = 0.05

# Female

clr_F_ETS_int_res_CPD_95 = cbind(horizon_specific_int_fore_subnational_err_F_EVR_ETS_CLR_alpha_0.95[,2],
                                 horizon_specific_MFTS_int_fore_subnational_err_F_EVR_ETS_CLR_95[,2],
                                 horizon_specific_MLFTS_int_fore_subnational_err_F_EVR_ETS_CLR_95[,2],
                                            
                                 horizon_specific_hdfpca_int_fore_subnational_err_F_EVR_ETS_alpha_0.95_CLR[,2],
                                 FANOVA_FFM_F_ets_int_err_95_mean_CLR[,2],
                                            
                                 horizon_specific_int_fore_subnational_err_F_K6_ETS_CLR_K6_alpha_0.95[,2],
                                 horizon_specific_MFTS_int_fore_subnational_err_F_K6_ETS_CLR_95[,2],
                                 horizon_specific_MLFTS_int_fore_subnational_err_F_K6_ETS_CLR_95[,2],
                                            
                                 horizon_clr_gender_gap_ets_int_F_95_err_mean[,2],
                                 horizon_clr_region_gap_ets_int_F_95_err_mean[,2],
                                 horizon_clr_double_gap_ets_int_F_95_err_mean[,2])
colnames(clr_F_ETS_int_res_CPD_95) = c("UFTS (EVR)", "MFTS (EVR)", "MLFTS (EVR)", "HDFPCA", "FANOVA+FFM",
                                       "UFTS (K=6)", "MFTS (K=6)", "MLFTS (K=6)", "Gender gap", "Region gap", "Double gap")

clr_F_ETS_int_res_CPD_95_overall = rbind(clr_F_ETS_int_res_CPD_95,
                                         apply(clr_F_ETS_int_res_CPD_95, 2, mean),
                                         apply(clr_F_ETS_int_res_CPD_95, 2, median))
rownames(clr_F_ETS_int_res_CPD_95_overall) = c(1:20, "Mean", "Median")

# Male

clr_M_ETS_int_res_CPD_95 = cbind(horizon_specific_int_fore_subnational_err_M_EVR_ETS_CLR_alpha_0.95[,2],
                                 horizon_specific_MFTS_int_fore_subnational_err_M_EVR_ETS_CLR_95[,2],
                                 horizon_specific_MLFTS_int_fore_subnational_err_M_EVR_ETS_CLR_95[,2],
                                            
                                 horizon_specific_hdfpca_int_fore_subnational_err_M_EVR_ETS_alpha_0.95_CLR[,2],
                                 FANOVA_FFM_M_ets_int_err_95_mean_CLR[,2],
                                            
                                 horizon_specific_int_fore_subnational_err_M_K6_ETS_CLR_K6_alpha_0.95[,2],
                                 horizon_specific_MFTS_int_fore_subnational_err_M_K6_ETS_CLR_95[,2],
                                 horizon_specific_MLFTS_int_fore_subnational_err_M_K6_ETS_CLR_95[,2],
                                            
                                 horizon_clr_gender_gap_ets_int_M_95_err_mean[,2],
                                 horizon_clr_region_gap_ets_int_M_95_err_mean[,2],
                                 horizon_clr_double_gap_ets_int_M_95_err_mean[,2])
colnames(clr_M_ETS_int_res_CPD_95) = c("UFTS (EVR)", "MFTS (EVR)", "MLFTS (EVR)", "HDFPCA", "FANOVA+FFM",
                                       "UFTS (K=6)", "MFTS (K=6)", "MLFTS (K=6)", "Gender gap", "Region gap", "Double gap")

clr_M_ETS_int_res_CPD_95_overall = rbind(clr_M_ETS_int_res_CPD_95,
                                         apply(clr_M_ETS_int_res_CPD_95, 2, mean),
                                         apply(clr_M_ETS_int_res_CPD_95, 2, median))
rownames(clr_M_ETS_int_res_CPD_95_overall) = c(1:20, "Mean", "Median")

which.min(colMeans(clr_F_ETS_int_res_CPD))
which.min(colMeans(clr_M_ETS_int_res_CPD))
which.min(colMeans(clr_F_ETS_int_res_CPD_95))
which.min(colMeans(clr_M_ETS_int_res_CPD_95))

## prefectures

# Female

clr_F_ETS_int_res_CPD_95_prefecture = cbind(horizon_specific_int_fore_subnational_err_F_EVR_ETS_CLR_alpha_0.95_prefecture[,2],
                                            horizon_specific_MFTS_int_fore_subnational_err_F_EVR_ETS_CLR_95_prefecture[,2],
                                            horizon_specific_MLFTS_int_fore_subnational_err_F_EVR_ETS_CLR_prefecture_95[,2],
                                            
                                            horizon_specific_hdfpca_int_fore_subnational_err_F_EVR_ETS_alpha_0.95_CLR_prefecture[,2],
                                            FANOVA_FFM_F_ets_int_err_95_mean_CLR_prefecture[,2],
                                            
                                            horizon_specific_int_fore_subnational_err_F_K6_ETS_CLR_K6_alpha_0.95_prefecture[,2],
                                            horizon_specific_MFTS_int_fore_subnational_err_F_K6_ETS_CLR_95_prefecture[,2],
                                            horizon_specific_MLFTS_int_fore_subnational_err_F_K6_ETS_CLR_95_prefecture[,2],
                                            
                                            clr_gender_gap_ets_int_F_95_err_mean[,2],
                                            clr_region_gap_ets_int_F_95_err_mean[,2],
                                            clr_double_gap_ets_int_F_95_err_mean[,2])
# Male

clr_M_ETS_int_res_CPD_95_prefecture = cbind(horizon_specific_int_fore_subnational_err_M_EVR_ETS_CLR_alpha_0.95_prefecture[,2],
                                            horizon_specific_MFTS_int_fore_subnational_err_M_EVR_ETS_CLR_95_prefecture[,2],
                                            horizon_specific_MLFTS_int_fore_subnational_err_M_EVR_ETS_CLR_prefecture_95[,2],
                                            
                                            horizon_specific_hdfpca_int_fore_subnational_err_M_EVR_ETS_alpha_0.95_CLR_prefecture[,2],
                                            FANOVA_FFM_M_ets_int_err_95_mean_CLR_prefecture[,2],
                                            
                                            horizon_specific_int_fore_subnational_err_M_K6_ETS_CLR_K6_alpha_0.95_prefecture[,2],
                                            horizon_specific_MFTS_int_fore_subnational_err_M_K6_ETS_CLR_95_prefecture[,2],
                                            horizon_specific_MLFTS_int_fore_subnational_err_M_K6_ETS_CLR_95_prefecture[,2],
                                            
                                            clr_gender_gap_ets_int_M_95_err_mean[,2],
                                            clr_region_gap_ets_int_M_95_err_mean[,2],
                                            clr_double_gap_ets_int_M_95_err_mean[,2])
colnames(clr_F_ETS_int_res_CPD_95_prefecture) = colnames(clr_M_ETS_int_res_CPD_95_prefecture) = c("UFTS (EVR)", "MFTS (EVR)", "MLFTS (EVR)", "HDFPCA", "FANOVA+FFM",
                                                                                                  "UFTS (K=6)", "MFTS (K=6)", "MLFTS (K=6)", "Gender gap", "Region gap", "Double gap")

  
#########
# xtable
#########

xtable(rbind(clr_F_ETS_int_res_CPD_overall[c(1, 5, 10, 15, 20:22),],
             clr_M_ETS_int_res_CPD_overall[c(1, 5, 10, 15, 20:22),],
             clr_F_ETS_int_res_CPD_95_overall[c(1, 5, 10, 15, 20:22),],
             clr_M_ETS_int_res_CPD_95_overall[c(1, 5, 10, 15, 20:22),]), digits = 4)

