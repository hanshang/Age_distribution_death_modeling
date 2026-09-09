## alpha = 0.2

# Female

clr_prefecture_F_ARIMA_int_res = cbind(horizon_specific_int_fore_subnational_err_F_EVR_ARIMA_CLR_prefecture[,1],
                                       horizon_specific_MFTS_int_fore_subnational_err_F_EVR_ARIMA_CLR_prefecture[,1],
                                       horizon_specific_MLFTS_int_fore_subnational_err_F_EVR_ARIMA_CLR_prefecture[,1],
                                       
                                       horizon_specific_hdfpca_int_fore_subnational_err_F_EVR_ARIMA_CLR_prefecture[,1],
                                       FANOVA_FFM_F_arima_int_err_mean_CLR_prefecture[,1],
                                       
                                       horizon_specific_int_fore_subnational_err_F_K6_ARIMA_CLR_prefecture[,1],
                                       horizon_specific_MFTS_int_fore_subnational_err_F_K6_ARIMA_CLR_prefecture[,1],
                                       horizon_specific_MLFTS_int_fore_subnational_err_F_K6_ARIMA_CLR_prefecture[,1],
                                       
                                       clr_gender_gap_arima_int_F_err_mean[,1],
                                       clr_region_gap_arima_int_F_err_mean[,1],
                                       clr_double_gap_arima_int_F_err_mean[,1])
colnames(clr_prefecture_F_ARIMA_int_res) = c("UFTS (EVR)", "MFTS (EVR)", "MLFTS (EVR)", "HDFPCA", "FANOVA+FFM",
                                             "UFTS (K=6)", "MFTS (K=6)", "MLFTS (K=6)", "Gender gap", "Region gap", "Double gap")

savepdf("clr_F_ARIMA_ECP", width = 12, height = 10, toplines = 0.8)
par(mar = c(5, 4, 3, 2))
boxplot(clr_prefecture_F_ARIMA_int_res, ylab = "ECP", las = 1, xaxt = "n", main = "clr transformation (Female data)", outline = FALSE)
text(x = 1:ncol(clr_prefecture_F_ARIMA_int_res), y = par("usr")[3] - 0.02 * diff(par("usr")[3:4]),
     labels = colnames(clr_prefecture_F_ARIMA_int_res), srt = 45, adj = 1, xpd = TRUE)
abline(h = 0.8, lty = 2)
dev.off()


# Male

clr_prefecture_M_ARIMA_int_res = cbind(horizon_specific_int_fore_subnational_err_M_EVR_ARIMA_CLR_prefecture[,1],
                                       horizon_specific_MFTS_int_fore_subnational_err_M_EVR_ARIMA_CLR_prefecture[,1],
                                       horizon_specific_MLFTS_int_fore_subnational_err_M_EVR_ARIMA_CLR_prefecture[,1],
                                       
                                       horizon_specific_hdfpca_int_fore_subnational_err_M_EVR_ARIMA_CLR_prefecture[,1],
                                       FANOVA_FFM_M_arima_int_err_mean_CLR_prefecture[,1],
                                       
                                       horizon_specific_int_fore_subnational_err_M_K6_ARIMA_CLR_prefecture[,1],
                                       horizon_specific_MFTS_int_fore_subnational_err_M_K6_ARIMA_CLR_prefecture[,1],
                                       horizon_specific_MLFTS_int_fore_subnational_err_M_K6_ARIMA_CLR_prefecture[,1],
                                       
                                       clr_gender_gap_arima_int_M_err_mean[,1],
                                       clr_region_gap_arima_int_M_err_mean[,1],
                                       clr_double_gap_arima_int_M_err_mean[,1])
colnames(clr_prefecture_M_ARIMA_int_res) = c("UFTS (EVR)", "MFTS (EVR)", "MLFTS (EVR)", "HDFPCA", "FANOVA+FFM",
                                             "UFTS (K=6)", "MFTS (K=6)", "MLFTS (K=6)", "Gender gap", "Region gap", "Double gap")

savepdf("clr_M_ARIMA_ECP", width = 12, height = 10, toplines = 0.8)
par(mar = c(5, 4, 3, 2))
boxplot(clr_prefecture_M_ARIMA_int_res, ylab = "ECP", las = 1, xaxt = "n", main = "clr transformation (Male data)", outline = FALSE)
text(x = 1:ncol(clr_prefecture_M_ARIMA_int_res), y = par("usr")[3] - 0.02 * diff(par("usr")[3:4]),
     labels = colnames(clr_prefecture_M_ARIMA_int_res), srt = 45, adj = 1, xpd = TRUE)
abline(h = 0.8, lty = 2)
dev.off()


## alpha = 0.05

# Female

clr_prefecture_F_ARIMA_int_res_95 = cbind(horizon_specific_int_fore_subnational_err_F_EVR_ARIMA_CLR_alpha_0.95_prefecture[,1],
                                          horizon_specific_MFTS_int_fore_subnational_err_F_EVR_ARIMA_CLR_95_prefecture[,1],
                                          horizon_specific_MLFTS_int_fore_subnational_err_F_EVR_ARIMA_CLR_prefecture_95[,1],
                                          
                                          horizon_specific_hdfpca_int_fore_subnational_err_F_EVR_ARIMA_alpha_0.95_CLR_prefecture[,1],
                                          FANOVA_FFM_F_arima_int_err_mean_95_CLR_prefecture[,1],
                                          
                                          horizon_specific_int_fore_subnational_err_F_K6_ARIMA_CLR_alpha_0.95_prefecture[,1],
                                          horizon_specific_MFTS_int_fore_subnational_err_F_K6_ARIMA_CLR_95_prefecture[,1],
                                          horizon_specific_MLFTS_int_fore_subnational_err_F_K6_ARIMA_CLR_95_prefecture[,1],
                                          
                                          clr_gender_gap_arima_int_F_95_err_mean[,1],
                                          clr_region_gap_arima_int_F_95_err_mean[,1],
                                          clr_double_gap_arima_int_F_95_err_mean[,1])
colnames(clr_prefecture_F_ARIMA_int_res_95) = c("UFTS (EVR)", "MFTS (EVR)", "MLFTS (EVR)", "HDFPCA", "FANOVA+FFM",
                                                "UFTS (K=6)", "MFTS (K=6)", "MLFTS (K=6)", "Gender gap", "Region gap", "Double gap")

savepdf("clr_F_ARIMA_ECP_95", width = 12, height = 10, toplines = 0.8)
par(mar = c(5, 4, 3, 2))
boxplot(clr_prefecture_F_ARIMA_int_res_95, ylab = "ECP", las = 1, xaxt = "n", main = "clr transformation (Female data)", outline = FALSE)
text(x = 1:ncol(clr_prefecture_F_ARIMA_int_res_95), y = par("usr")[3] - 0.02 * diff(par("usr")[3:4]),
     labels = colnames(clr_prefecture_F_ARIMA_int_res_95), srt = 45, adj = 1, xpd = TRUE)
abline(h = 0.95, lty = 2)
dev.off()


# Male

clr_prefecture_M_ARIMA_int_res_95 = cbind(horizon_specific_int_fore_subnational_err_M_EVR_ARIMA_CLR_alpha_0.95_prefecture[,1],
                                          horizon_specific_MFTS_int_fore_subnational_err_M_EVR_ARIMA_CLR_95_prefecture[,1],
                                          horizon_specific_MLFTS_int_fore_subnational_err_M_EVR_ARIMA_CLR_prefecture_95[,1],
                                          
                                          horizon_specific_hdfpca_int_fore_subnational_err_M_EVR_ARIMA_alpha_0.95_CLR_prefecture[,1],
                                          FANOVA_FFM_M_arima_int_err_mean_95_CLR_prefecture[,1],
                                          
                                          horizon_specific_int_fore_subnational_err_M_K6_ARIMA_CLR_alpha_0.95_prefecture[,1],
                                          horizon_specific_MFTS_int_fore_subnational_err_M_K6_ARIMA_CLR_95_prefecture[,1],
                                          horizon_specific_MLFTS_int_fore_subnational_err_M_K6_ARIMA_CLR_95_prefecture[,1],
                                          
                                          clr_gender_gap_arima_int_M_95_err_mean[,1],
                                          clr_region_gap_arima_int_M_95_err_mean[,1],
                                          clr_double_gap_arima_int_M_95_err_mean[,1])
colnames(clr_prefecture_M_ARIMA_int_res_95) = c("UFTS (EVR)", "MFTS (EVR)", "MLFTS (EVR)", "HDFPCA", "FANOVA+FFM",
                                                "UFTS (K=6)", "MFTS (K=6)", "MLFTS (K=6)", "Gender gap", "Region gap", "Double gap")

savepdf("clr_M_ARIMA_ECP_95", width = 12, height = 10, toplines = 0.8)
par(mar = c(5, 4, 3, 2))
boxplot(clr_prefecture_M_ARIMA_int_res_95, ylab = "ECP", las = 1, xaxt = "n", main = "clr transformation (Male data)", outline = FALSE)
text(x = 1:ncol(clr_prefecture_M_ARIMA_int_res_95), y = par("usr")[3] - 0.02 * diff(par("usr")[3:4]),
     labels = colnames(clr_prefecture_M_ARIMA_int_res_95), srt = 45, adj = 1, xpd = TRUE)
abline(h = 0.95, lty = 2)
dev.off()

