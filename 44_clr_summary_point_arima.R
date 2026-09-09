##########
# summary
##########

## Female

prefecture_F_ARIMA_KLD_res_CLR = cbind(point_fore_subnational_err_F_EVR_ARIMA_clr_mean[1:47,1],
                                       point_fore_subnational_err_F_EVR_ARIMA_MFTS_clr_mean[1:47,1],
                                       point_fore_subnational_err_F_EVR_ARIMA_MLFTS_clr_mean[1:47,1],
                                       
                                       point_fore_subnational_err_ARIMA_HDFPCA_F_clr_mean[1:47,1],
                                       point_fore_subnational_err_FANOVA_FFM_F_ARIMA_clr_mean[1:47,1],
                                       
                                       point_fore_subnational_err_F_K6_ARIMA_clr_mean[1:47,1],
                                       point_fore_subnational_err_F_K6_ARIMA_MFTS_clr_mean[1:47,1],
                                       point_fore_subnational_err_F_K6_ARIMA_MLFTS_clr_mean[1:47,1],
                                       
                                       colMeans(gender_gap_clr_KLD_F_arima),
                                       colMeans(region_gap_clr_KLD_F_arima),
                                       colMeans(double_gap_clr_KLD_F_arima))

colnames(prefecture_F_ARIMA_KLD_res_CLR) = c("UFTS (EVR)", "MFTS (EVR)", "MLFTS (EVR)", "HDFPCA", "FANOVA+FFM",
                                             "UFTS (K=6)", "MFTS (K=6)", "MLFTS (K=6)", 
                                             "Gender gap", "Region gap", "Double gap")

# draw a boxplot

savepdf("KLD_F_ARIMA_CLR", width = 12, height = 10, toplines = 0.8, pointsize = 8)
par(mar = c(5, 4, 3, 2))
boxplot(prefecture_F_ARIMA_KLD_res_CLR, ylab = "KLD", las = 1, xaxt = "n", main = "clr transformation (Female data)", outline = FALSE,
        ylim = c(0, 0.25))
text(x = 1:ncol(prefecture_F_ARIMA_KLD_res_CLR), y = par("usr")[3] - 0.02 * diff(par("usr")[3:4]),
     labels = colnames(prefecture_F_ARIMA_KLD_res_CLR), srt = 45, adj = 1, xpd = TRUE)
dev.off()

# by horizon

horizon_F_ARIMA_KLD_res_CLR = cbind(horizon_point_fore_subnational_err_F_EVR_ARIMA_clr_mean[1:20,1],
                                    horizon_point_fore_subnational_err_F_EVR_ARIMA_MFTS_clr_mean[1:20,1],
                                    horizon_point_fore_subnational_err_F_EVR_ARIMA_MLFTS_clr_mean[1:20,1],
                                    
                                    horizon_point_fore_subnational_err_ARIMA_HDFPCA_F_clr_mean[1:20,1],
                                    horizon_point_fore_subnational_err_FANOVA_FFM_F_ARIMA_clr_mean[1:20,1],
                                    
                                    horizon_point_fore_subnational_err_F_K6_ARIMA_clr_mean[1:20,1],
                                    horizon_point_fore_subnational_err_F_K6_ARIMA_MFTS_clr_mean[1:20,1],
                                    horizon_point_fore_subnational_err_F_K6_ARIMA_MLFTS_clr_mean[1:20,1],
                                    
                                    horizon_gender_gap_clr_KLD_F_arima_mean,
                                    horizon_region_gap_clr_KLD_F_arima_mean,
                                    horizon_double_gap_clr_KLD_F_arima_mean)
colnames(horizon_F_ARIMA_KLD_res_CLR) = c("UFTS (EVR)", "MFTS (EVR)", "MLFTS (EVR)", "HDFPCA", "FANOVA+FFM",
                                          "UFTS (K=6)", "MFTS (K=6)", "MLFTS (K=6)", 
                                          "Gender gap", "Region gap", "Double gap")

horizon_F_ARIMA_KLD_res_CLR_mean = rbind(horizon_F_ARIMA_KLD_res_CLR, 
                                       apply(horizon_F_ARIMA_KLD_res_CLR, 2, mean),
                                       apply(horizon_F_ARIMA_KLD_res_CLR, 2, median))
rownames(horizon_F_ARIMA_KLD_res_CLR_mean) = c(1:20, "Mean", "Median")


## Male

prefecture_M_ARIMA_KLD_res_CLR = cbind(point_fore_subnational_err_M_EVR_ARIMA_clr_mean[1:47,1],
                                       point_fore_subnational_err_M_EVR_ARIMA_MFTS_clr_mean[1:47,1],
                                       point_fore_subnational_err_M_EVR_ARIMA_MLFTS_clr_mean[1:47,1],
                                       
                                       point_fore_subnational_err_ARIMA_HDFPCA_M_clr_mean[1:47,1],
                                       point_fore_subnational_err_FANOVA_FFM_M_ARIMA_clr_mean[1:47,1],
                                       
                                       point_fore_subnational_err_M_K6_ARIMA_clr_mean[1:47,1],
                                       point_fore_subnational_err_M_K6_ARIMA_MFTS_clr_mean[1:47,1],
                                       point_fore_subnational_err_M_K6_ARIMA_MLFTS_clr_mean[1:47,1],
                                       
                                       colMeans(gender_gap_clr_KLD_M_arima),
                                       colMeans(region_gap_clr_KLD_M_arima),
                                       colMeans(double_gap_clr_KLD_M_arima))

colnames(prefecture_M_ARIMA_KLD_res_CLR) = c("UFTS (EVR)", "MFTS (EVR)", "MLFTS (EVR)", "HDFPCA", "FANOVA+FFM",
                                             "UFTS (K=6)", "MFTS (K=6)", "MLFTS (K=6)", 
                                             "Gender gap", "Region gap", "Double gap")

# draw a boxplot

savepdf("KLD_M_ARIMA_CLR", width = 12, height = 10, toplines = 0.8, pointsize = 8)
par(mar = c(5, 4, 3, 2))
boxplot(prefecture_M_ARIMA_KLD_res_CLR, ylab = "KLD", las = 1, xaxt = "n", main = "clr transformation (Male data)", outline = FALSE,
        ylim = c(0, 0.17))
text(x = 1:ncol(prefecture_M_ARIMA_KLD_res_CLR), y = par("usr")[3] - 0.02 * diff(par("usr")[3:4]),
     labels = colnames(prefecture_M_ARIMA_KLD_res_CLR), srt = 45, adj = 1, xpd = TRUE)
dev.off()

# by horizon

horizon_M_ARIMA_KLD_res_CLR = cbind(horizon_point_fore_subnational_err_M_EVR_ARIMA_clr_mean[1:20,1],
                                    horizon_point_fore_subnational_err_M_EVR_ARIMA_MFTS_clr_mean[1:20,1],
                                    horizon_point_fore_subnational_err_M_EVR_ARIMA_MLFTS_clr_mean[1:20,1],
                                    
                                    horizon_point_fore_subnational_err_ARIMA_HDFPCA_M_clr_mean[1:20,1],
                                    horizon_point_fore_subnational_err_FANOVA_FFM_M_ARIMA_clr_mean[1:20,1],
                                    
                                    horizon_point_fore_subnational_err_M_K6_ARIMA_clr_mean[1:20,1],
                                    horizon_point_fore_subnational_err_M_K6_ARIMA_MFTS_clr_mean[1:20,1],
                                    horizon_point_fore_subnational_err_M_K6_ARIMA_MLFTS_clr_mean[1:20,1],
                                    
                                    horizon_gender_gap_clr_KLD_M_arima_mean,
                                    horizon_region_gap_clr_KLD_M_arima_mean,
                                    horizon_double_gap_clr_KLD_M_arima_mean)
colnames(horizon_M_ARIMA_KLD_res_CLR) = c("UFTS (EVR)", "MFTS (EVR)", "MLFTS (EVR)", "HDFPCA", "FANOVA+FFM",
                                          "UFTS (K=6)", "MFTS (K=6)", "MLFTS (K=6)", 
                                          "Gender gap", "Region gap", "Double gap")

horizon_M_ARIMA_KLD_res_CLR_mean = rbind(horizon_M_ARIMA_KLD_res_CLR, 
                                         apply(horizon_M_ARIMA_KLD_res_CLR, 2, mean),
                                         apply(horizon_M_ARIMA_KLD_res_CLR, 2, median))
rownames(horizon_M_ARIMA_KLD_res_CLR_mean) = c(1:20, "Mean", "Median")

#########
# xtable
#########

xtable(rbind(horizon_F_ARIMA_KLD_res_CLR_mean[c(1, 5, 10, 15, 20:22),],
             horizon_M_ARIMA_KLD_res_CLR_mean[c(1, 5, 10, 15, 20:22),]), digits = 4)

