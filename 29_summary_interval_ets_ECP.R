###############
## alpha = 0.2
###############

## fmethod = "ets"

## Female

# by prefecture

prefecture_F_ETS_int_res = cbind(int_fore_subnational_err_F_EVR_ETS_mean_prefecture[,1],
                                 MFTS_int_fore_subnational_err_F_EVR_ETS_mean_prefecture[,1],
                                 MLFTS_int_fore_subnational_err_F_EVR_ETS_mean_prefecture[,1],
                                
                                 horizon_specific_hdfpca_int_fore_subnational_err_F_EVR_ETS_prefecture[,1],
                                 FANOVA_FFM_F_ets_int_err_mean_prefecture[,1],
                                
                                 int_fore_subnational_err_F_K6_ETS_mean_prefecture[,1],
                                 MFTS_int_fore_subnational_err_F_K6_ETS_mean_prefecture[,1],
                                 MLFTS_int_fore_subnational_err_F_K6_ETS_mean_prefecture[,1],
                                
                                 gender_gap_ets_int_F_err_mean[,1],
                                 region_gap_ets_int_F_err_mean[,1],
                                 double_gap_ets_int_F_err_mean[,1])
colnames(prefecture_F_ETS_int_res) = c("UFTS (EVR)", "MFTS (EVR)", "MLFTS (EVR)", "HDFPCA", "FANOVA+FFM",
                                       "UFTS (K=6)", "MFTS (K=6)", "MLFTS (K=6)", "Gender gap", "Region gap", "Double gap")

savepdf("CDF_F_ETS_ECP", width = 12, height = 10, toplines = 0.8)
par(mar = c(5, 4, 3, 2))
boxplot(prefecture_F_ETS_int_res, ylab = "ECP", las = 1, xaxt = "n", main = "CDF transformation (Female data)", outline = FALSE)
text(x = 1:ncol(prefecture_F_ETS_int_res), y = par("usr")[3] - 0.02 * diff(par("usr")[3:4]),
     labels = colnames(prefecture_F_ETS_int_res), srt = 45, adj = 1, xpd = TRUE)
abline(h = 0.8, lty = 2)
dev.off()

# Male

prefecture_M_ETS_int_res = cbind(int_fore_subnational_err_M_EVR_ETS_mean_prefecture[,1],
                                 MFTS_int_fore_subnational_err_M_EVR_ETS_mean_prefecture[,1],
                                 MLFTS_int_fore_subnational_err_M_EVR_ETS_mean_prefecture[,1],
                                
                                 horizon_specific_hdfpca_int_fore_subnational_err_M_EVR_ETS_prefecture[,1],
                                 FANOVA_FFM_M_ets_int_err_mean_prefecture[,1],
                                
                                 int_fore_subnational_err_M_K6_ETS_mean_prefecture[,1],
                                 MFTS_int_fore_subnational_err_M_K6_ETS_mean_prefecture[,1],
                                 MLFTS_int_fore_subnational_err_M_K6_ETS_mean_prefecture[,1],
                                
                                 gender_gap_ets_int_M_err_mean[,1],
                                 region_gap_ets_int_M_err_mean[,1],
                                 double_gap_ets_int_M_err_mean[,1])
colnames(prefecture_M_ETS_int_res) = c("UFTS (EVR)", "MFTS (EVR)", "MLFTS (EVR)", "HDFPCA", "FANOVA+FFM",
                                       "UFTS (K=6)", "MFTS (K=6)", "MLFTS (K=6)", "Gender gap", "Region gap", "Double gap")

savepdf("CDF_M_ETS_ECP", width = 12, height = 10, toplines = 0.8)
par(mar = c(5, 4, 3, 2))
boxplot(prefecture_M_ETS_int_res, ylab = "ECP", las = 1, xaxt = "n", main = "CDF transformation (Male data)", outline = FALSE)
text(x = 1:ncol(prefecture_M_ETS_int_res), y = par("usr")[3] - 0.02 * diff(par("usr")[3:4]),
     labels = colnames(prefecture_M_ETS_int_res), srt = 45, adj = 1, xpd = TRUE)
abline(h = 0.8, lty = 2)
dev.off()

################
## alpha = 0.05
################

## fmethod = "ets"

# Female

prefecture_F_ETS_int_res_95 = cbind(int_fore_subnational_err_F_EVR_ETS_alpha_0.95_mean_prefecture[,1],
                                    MFTS_int_fore_subnational_err_F_EVR_ETS_alpha_0.95_mean_prefecture[,1],
                                    MLFTS_int_fore_subnational_err_F_EVR_ETS_alpha_0.95_mean_prefecture[,1],
                                    
                                    horizon_specific_hdfpca_int_fore_subnational_err_F_EVR_ETS_alpha_0.95_prefecture[,1],
                                    FANOVA_FFM_F_ets_int_err_95_mean_prefecture[,1],
                                    
                                    int_fore_subnational_err_F_K6_ETS_alpha_0.95_mean_prefecture[,1],
                                    MFTS_int_fore_subnational_err_F_K6_ETS_alpha_0.95_mean_prefecture[,1],
                                    MLFTS_int_fore_subnational_err_F_K6_ETS_alpha_0.95_mean_prefecture[,1],
                                    
                                    gender_gap_ets_int_F_95_err_mean[,1],
                                    region_gap_ets_int_F_95_err_mean[,1],
                                    double_gap_ets_int_F_95_err_mean[,1])
colnames(prefecture_F_ETS_int_res_95) = c("UFTS (EVR)", "MFTS (EVR)", "MLFTS (EVR)", "HDFPCA", "FANOVA+FFM",
                                          "UFTS (K=6)", "MFTS (K=6)", "MLFTS (K=6)", "Gender gap", "Region gap", "Double gap")


savepdf("CDF_F_ETS_ECP_95", width = 12, height = 10, toplines = 0.8)
par(mar = c(5, 4, 3, 2))
boxplot(prefecture_F_ETS_int_res_95, ylab = "ECP", las = 1, xaxt = "n", main = "CDF transformation (Female data)", outline = FALSE)
text(x = 1:ncol(prefecture_F_ETS_int_res_95), y = par("usr")[3] - 0.02 * diff(par("usr")[3:4]),
     labels = colnames(prefecture_F_ETS_int_res_95), srt = 45, adj = 1, xpd = TRUE)
abline(h = 0.95, lty = 2)
dev.off()

# Male

prefecture_M_ETS_int_res_95 = cbind(int_fore_subnational_err_M_EVR_ETS_alpha_0.95_mean_prefecture[,1],
                                    MFTS_int_fore_subnational_err_M_EVR_ETS_alpha_0.95_mean_prefecture[,1],
                                    MLFTS_int_fore_subnational_err_M_EVR_ETS_alpha_0.95_mean_prefecture[,1],
                                    
                                    horizon_specific_hdfpca_int_fore_subnational_err_M_EVR_ETS_alpha_0.95_prefecture[,1],
                                    FANOVA_FFM_M_ets_int_err_95_mean_prefecture[,1],
                                    
                                    int_fore_subnational_err_M_K6_ETS_alpha_0.95_mean_prefecture[,1],
                                    MFTS_int_fore_subnational_err_M_K6_ETS_alpha_0.95_mean_prefecture[,1],
                                    MLFTS_int_fore_subnational_err_M_K6_ETS_alpha_0.95_mean_prefecture[,1],
                                    
                                    gender_gap_ets_int_M_95_err_mean[,1],
                                    region_gap_ets_int_M_95_err_mean[,1],
                                    double_gap_ets_int_M_95_err_mean[,1])
colnames(prefecture_M_ETS_int_res_95) = c("UFTS (EVR)", "MFTS (EVR)", "MLFTS (EVR)", "HDFPCA", "FANOVA+FFM",
                                          "UFTS (K=6)", "MFTS (K=6)", "MLFTS (K=6)", "Gender gap", "Region gap", "Double gap")

savepdf("CDF_M_ETS_ECP_95", width = 12, height = 10, toplines = 0.8)
par(mar = c(5, 4, 3, 2))
boxplot(prefecture_M_ETS_int_res_95, ylab = "ECP", las = 1, xaxt = "n", main = "CDF transformation (Male data)", outline = FALSE)
text(x = 1:ncol(prefecture_M_ETS_int_res_95), y = par("usr")[3] - 0.02 * diff(par("usr")[3:4]),
     labels = colnames(prefecture_M_ETS_int_res_95), srt = 45, adj = 1, xpd = TRUE)
abline(h = 0.95, lty = 2)
dev.off()

