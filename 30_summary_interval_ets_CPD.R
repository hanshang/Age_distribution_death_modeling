###############
## alpha = 0.2
###############

## fmethod = "ets"

# Female

CDF_F_ETS_int_res_CPD = cbind(int_fore_subnational_err_F_EVR_ETS_mean[,2],
                              MFTS_int_fore_subnational_err_F_EVR_ETS_mean[,2],
                              MLFTS_int_fore_subnational_err_F_EVR_ETS_mean[,2],
                                     
                              horizon_specific_hdfpca_int_fore_subnational_err_F_EVR_ETS[,2],
                              FANOVA_FFM_F_ets_int_err_mean[,2],
                                     
                              int_fore_subnational_err_F_K6_ETS_mean[,2],
                              MFTS_int_fore_subnational_err_F_K6_ETS_mean[,2],
                              MLFTS_int_fore_subnational_err_F_K6_ETS_mean[,2],
                                     
                              horizon_gender_gap_ets_int_F_err_mean[,2],
                              horizon_region_gap_ets_int_F_err_mean[,2],
                              horizon_double_gap_ets_int_F_err_mean[,2])
colnames(CDF_F_ETS_int_res_CPD) = c("UFTS (EVR)", "MFTS (EVR)", "MLFTS (EVR)", "HDFPCA", "FANOVA+FFM",
                                    "UFTS (K=6)", "MFTS (K=6)", "MLFTS (K=6)", "Gender gap", "Region gap", "Double gap")

CDF_F_ETS_int_res_CPD_overall = rbind(CDF_F_ETS_int_res_CPD, 
                                      apply(CDF_F_ETS_int_res_CPD, 2, mean),
                                      apply(CDF_F_ETS_int_res_CPD, 2, median))
rownames(CDF_F_ETS_int_res_CPD_overall) = c(1:20, "Mean", "Median")

# Male

CDF_M_ETS_int_res_CPD = cbind(int_fore_subnational_err_M_EVR_ETS_mean[,2],
                              MFTS_int_fore_subnational_err_M_EVR_ETS_mean[,2],
                              MLFTS_int_fore_subnational_err_M_EVR_ETS_mean[,2],
                                     
                              horizon_specific_hdfpca_int_fore_subnational_err_M_EVR_ETS[,2],
                              FANOVA_FFM_M_ets_int_err_mean[,2],
                                     
                              int_fore_subnational_err_M_K6_ETS_mean[,2],
                              MFTS_int_fore_subnational_err_M_K6_ETS_mean[,2],
                              MLFTS_int_fore_subnational_err_M_K6_ETS_mean[,2],
                                     
                              horizon_gender_gap_ets_int_M_err_mean[,2],
                              horizon_region_gap_ets_int_M_err_mean[,2],
                              horizon_double_gap_ets_int_M_err_mean[,2])
colnames(CDF_M_ETS_int_res_CPD) = c("UFTS (EVR)", "MFTS (EVR)", "MLFTS (EVR)", "HDFPCA", "FANOVA+FFM",
                                    "UFTS (K=6)", "MFTS (K=6)", "MLFTS (K=6)", "Gender gap", "Region gap", "Double gap")

CDF_M_ETS_int_res_CPD_overall = rbind(CDF_M_ETS_int_res_CPD, 
                                      apply(CDF_M_ETS_int_res_CPD, 2, mean),
                                      apply(CDF_M_ETS_int_res_CPD, 2, median))
rownames(CDF_M_ETS_int_res_CPD_overall) = c(1:20, "Mean", "Median")

## prefectures

# Female

CDF_F_ETS_int_res_CPD_prefecture = cbind(int_fore_subnational_err_F_EVR_ETS_mean_prefecture[,2],
                                         MFTS_int_fore_subnational_err_F_EVR_ETS_mean_prefecture[,2],
                                         MLFTS_int_fore_subnational_err_F_EVR_ETS_mean_prefecture[,2],
                                         
                                         horizon_specific_hdfpca_int_fore_subnational_err_F_EVR_ETS_prefecture[,2],
                                         FANOVA_FFM_F_ets_int_err_mean_prefecture[,2],
                                         
                                         int_fore_subnational_err_F_K6_ETS_mean_prefecture[,2],
                                         MFTS_int_fore_subnational_err_F_K6_ETS_mean_prefecture[,2],
                                         MLFTS_int_fore_subnational_err_F_K6_ETS_mean_prefecture[,2],
                                         
                                         gender_gap_ets_int_F_err_mean[,2],
                                         region_gap_ets_int_F_err_mean[,2],
                                         double_gap_ets_int_F_err_mean[,2])

# Male

CDF_M_ETS_int_res_CPD_prefecture = cbind(int_fore_subnational_err_M_EVR_ETS_mean_prefecture[,2],
                                         MFTS_int_fore_subnational_err_M_EVR_ETS_mean_prefecture[,2],
                                         MLFTS_int_fore_subnational_err_M_EVR_ETS_mean_prefecture[,2],
                                         
                                         horizon_specific_hdfpca_int_fore_subnational_err_M_EVR_ETS_prefecture[,2],
                                         FANOVA_FFM_M_ets_int_err_mean_prefecture[,2],
                                         
                                         int_fore_subnational_err_M_K6_ETS_mean_prefecture[,2],
                                         MFTS_int_fore_subnational_err_M_K6_ETS_mean_prefecture[,2],
                                         MLFTS_int_fore_subnational_err_M_K6_ETS_mean_prefecture[,2],
                                         
                                         gender_gap_ets_int_M_err_mean[,2],
                                         region_gap_ets_int_M_err_mean[,2],
                                         double_gap_ets_int_M_err_mean[,2])
colnames(CDF_F_ETS_int_res_CPD_prefecture) = colnames(CDF_M_ETS_int_res_CPD_prefecture) = c("UFTS (EVR)", "MFTS (EVR)", "MLFTS (EVR)", "HDFPCA", "FANOVA+FFM",
                                               "UFTS (K=6)", "MFTS (K=6)", "MLFTS (K=6)", "Gender gap", "Region gap", "Double gap")

################
## alpha = 0.05
################

## fmethod = "ets"

# Female

CDF_F_ETS_int_res_CPD_95 = cbind(int_fore_subnational_err_F_EVR_ETS_alpha_0.95_mean[,2],
                                 MFTS_int_fore_subnational_err_F_EVR_ETS_alpha_0.95_mean[,2],
                                 MLFTS_int_fore_subnational_err_F_EVR_ETS_alpha_0.95_mean[,2],
                                        
                                 horizon_specific_hdfpca_int_fore_subnational_err_F_EVR_ETS_alpha_0.95[,2],
                                 FANOVA_FFM_F_ets_int_err_95_mean[,2],
                                        
                                 int_fore_subnational_err_F_K6_ETS_alpha_0.95_mean[,2],
                                 MFTS_int_fore_subnational_err_F_K6_ETS_alpha_0.95_mean[,2],
                                 MLFTS_int_fore_subnational_err_F_K6_ETS_alpha_0.95_mean[,2],
                                        
                                 horizon_gender_gap_ets_int_F_95_err_mean[,2],
                                 horizon_region_gap_ets_int_F_95_err_mean[,2],
                                 horizon_double_gap_ets_int_F_95_err_mean[,2])
colnames(CDF_F_ETS_int_res_95) = c("UFTS (EVR)", "MFTS (EVR)", "MLFTS (EVR)", "HDFPCA", "FANOVA+FFM",
                                   "UFTS (K=6)", "MFTS (K=6)", "MLFTS (K=6)", "Gender gap", "Region gap", "Double gap")

CDF_F_ETS_int_res_CPD_95_overall = rbind(CDF_F_ETS_int_res_CPD_95, 
                                         apply(CDF_F_ETS_int_res_CPD_95, 2, mean),
                                         apply(CDF_F_ETS_int_res_CPD_95, 2, median))
rownames(CDF_F_ETS_int_res_CPD_95_overall) = c(1:20, "Mean", "Median")

# Male

CDF_M_ETS_int_res_CPD_95 = cbind(int_fore_subnational_err_M_EVR_ETS_alpha_0.95_mean[,2],
                                 MFTS_int_fore_subnational_err_M_EVR_ETS_alpha_0.95_mean[,2],
                                 MLFTS_int_fore_subnational_err_M_EVR_ETS_alpha_0.95_mean[,2],
                                        
                                 horizon_specific_hdfpca_int_fore_subnational_err_M_EVR_ETS_alpha_0.95[,2],
                                 FANOVA_FFM_M_ets_int_err_95_mean[,2],
                                        
                                 int_fore_subnational_err_M_K6_ETS_alpha_0.95_mean[,2],
                                 MFTS_int_fore_subnational_err_M_K6_ETS_alpha_0.95_mean[,2],
                                 MLFTS_int_fore_subnational_err_M_K6_ETS_alpha_0.95_mean[,2],
                                        
                                 horizon_gender_gap_ets_int_M_95_err_mean[,2],
                                 horizon_region_gap_ets_int_M_95_err_mean[,2],
                                 horizon_double_gap_ets_int_M_95_err_mean[,2])
colnames(CDF_M_ETS_int_res_95) = c("UFTS (EVR)", "MFTS (EVR)", "MLFTS (EVR)", "HDFPCA", "FANOVA+FFM",
                                   "UFTS (K=6)", "MFTS (K=6)", "MLFTS (K=6)", "Gender gap", "Region gap", "Double gap")

CDF_M_ETS_int_res_CPD_95_overall = rbind(CDF_M_ETS_int_res_CPD_95, 
                                         apply(CDF_M_ETS_int_res_CPD_95, 2, mean),
                                         apply(CDF_M_ETS_int_res_CPD_95, 2, median))
rownames(CDF_M_ETS_int_res_CPD_95_overall) = c(1:20, "Mean", "Median")

## prefectures

# Female

CDF_F_ETS_int_res_CPD_95_prefecture = cbind(int_fore_subnational_err_F_EVR_ETS_alpha_0.95_mean_prefecture[,2],
                                            MFTS_int_fore_subnational_err_F_EVR_ETS_alpha_0.95_mean_prefecture[,2],
                                            MLFTS_int_fore_subnational_err_F_EVR_ETS_alpha_0.95_mean_prefecture[,2],
                                            
                                            horizon_specific_hdfpca_int_fore_subnational_err_F_EVR_ETS_alpha_0.95_prefecture[,2],
                                            FANOVA_FFM_F_ets_int_err_95_mean_prefecture[,2],
                                            
                                            int_fore_subnational_err_F_K6_ETS_alpha_0.95_mean_prefecture[,2],
                                            MFTS_int_fore_subnational_err_F_K6_ETS_alpha_0.95_mean_prefecture[,2],
                                            MLFTS_int_fore_subnational_err_F_K6_ETS_alpha_0.95_mean_prefecture[,2],
                                            
                                            gender_gap_ets_int_F_95_err_mean[,2],
                                            region_gap_ets_int_F_95_err_mean[,2],
                                            double_gap_ets_int_F_95_err_mean[,2])

# Male

CDF_M_ETS_int_res_CPD_95_prefecture = cbind(int_fore_subnational_err_M_EVR_ETS_alpha_0.95_mean_prefecture[,2],
                                            MFTS_int_fore_subnational_err_M_EVR_ETS_alpha_0.95_mean_prefecture[,2],
                                            MLFTS_int_fore_subnational_err_M_EVR_ETS_alpha_0.95_mean_prefecture[,2],
                                            
                                            horizon_specific_hdfpca_int_fore_subnational_err_M_EVR_ETS_alpha_0.95_prefecture[,2],
                                            FANOVA_FFM_M_ets_int_err_95_mean_prefecture[,2],
                                            
                                            int_fore_subnational_err_M_K6_ETS_alpha_0.95_mean_prefecture[,2],
                                            MFTS_int_fore_subnational_err_M_K6_ETS_alpha_0.95_mean_prefecture[,2],
                                            MLFTS_int_fore_subnational_err_M_K6_ETS_alpha_0.95_mean_prefecture[,2],
                                            
                                            gender_gap_ets_int_M_95_err_mean[,2],
                                            region_gap_ets_int_M_95_err_mean[,2],
                                            double_gap_ets_int_M_95_err_mean[,2])

colnames(CDF_F_ETS_int_res_CPD_95_prefecture) = colnames(CDF_M_ETS_int_res_CPD_95_prefecture) = c("UFTS (EVR)", "MFTS (EVR)", "MLFTS (EVR)", "HDFPCA", "FANOVA+FFM",
                                                                                                  "UFTS (K=6)", "MFTS (K=6)", "MLFTS (K=6)", "Gender gap", "Region gap", "Double gap")

#########
# xtable
#########

xtable(rbind(CDF_F_ETS_int_res_CPD_overall[c(1, 5, 10, 15, 20:22),],
             CDF_M_ETS_int_res_CPD_overall[c(1, 5, 10, 15, 20:22),],
             CDF_F_ETS_int_res_CPD_95_overall[c(1, 5, 10, 15, 20:22),],
             CDF_M_ETS_int_res_CPD_95_overall[c(1, 5, 10, 15, 20:22),]), digits = 4)

