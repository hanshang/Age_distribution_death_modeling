#########
# Female
#########

### ARIMA

## prefectures

prefecture_F_ARIMA_KLD_res = cbind(point_fore_subnational_err_F_EVR_ARIMA_mean[1:47, 1],
                                   point_fore_subnational_err_MFTS_F_EVR_ARIMA_mean[1:47, 1],
                                   point_fore_subnational_err_MLFTS_F_EVR_ARIMA_mean[1:47, 1],
                                   
                                   point_fore_subnational_err_HDFPCA_F_CDF_mean[1:47, 1],
                                   point_fore_subnational_err_FANOVA_FFM_F_ARIMA_mean[1:47, 1],
                                   
                                   point_fore_subnational_err_F_K6_ARIMA_mean[1:47, 1],
                                   point_fore_subnational_err_MFTS_F_K6_ARIMA_mean[1:47, 1],
                                   point_fore_subnational_err_MLFTS_F_K6_ARIMA_mean[1:47, 1],
                                   
                                   subnational_gender_gap_KLD_JSD_F_ARIMA_prefecture_mean[,1],
                                   subnational_region_gap_fore_female_ARIMA_prefecture_mean[,1],
                                   subnational_double_gap_fore_female_ARIMA_prefecture_mean[,1])
colnames(prefecture_F_ARIMA_KLD_res) = c("UFTS (EVR)", "MFTS (EVR)", "MLFTS (EVR)", "HDFPCA", "FANOVA+FFM",
                                         "UFTS (K=6)", "MFTS (K=6)", "MLFTS (K=6)", "Gender gap", 
                                         "Region gap", "Double gap")

# draw a boxplot

savepdf("KLD_F_ARIMA_CDF", width = 12, height = 10, toplines = 0.8, pointsize = 8)
par(mar = c(5, 4, 3, 2))
boxplot(prefecture_F_ARIMA_KLD_res, ylab = "KLD", las = 1, xaxt = "n", main = "CDF transformation (Female data)", outline = FALSE,
        ylim = c(0, 0.25))
text(x = 1:ncol(prefecture_F_ARIMA_KLD_res), y = par("usr")[3] - 0.02 * diff(par("usr")[3:4]),
     labels = colnames(prefecture_F_ARIMA_KLD_res), srt = 45, adj = 1, xpd = TRUE)
dev.off()

## horizons

plot(1:20,  horizon_point_fore_subnational_err_F_EVR_ARIMA_mean[1:20, 1], type = "l", col = 1, lty = 1, ylab = "KLD", xlab = "Forecast horizon (h)")
lines(1:20, horizon_point_fore_subnational_err_MFTS_F_EVR_ARIMA_mean[1:20, 1], col = 2, lty = 1)
lines(1:20, horizon_point_fore_subnational_err_MLFTS_F_EVR_ARIMA_mean[1:20, 1], col = 3, lty = 1)

lines(1:20, horizon_point_fore_subnational_err_HDFPCA_F_CDF_mean[1:20, 1], col = 4, lty = 3)
lines(1:20, horizon_point_fore_subnational_err_FANOVA_FFM_F_ARIMA_mean[1:20, 1], col = 5, lty = 4)

lines(1:20, horizon_point_fore_subnational_err_F_K6_ARIMA_mean[1:20, 1], col = 1, lty = 2)
lines(1:20, horizon_point_fore_subnational_err_MFTS_F_K6_ARIMA_mean[1:20, 1], col = 2, lty = 2)
lines(1:20, horizon_point_fore_subnational_err_MLFTS_F_K6_ARIMA_mean[1:20, 1], col = 3, lty = 2)

lines(1:20, subnational_gender_gap_KLD_JSD_F_ARIMA_mean[1:20, 1], col = 6, lty = 6)
lines(1:20, subnational_region_gap_fore_female_ARIMA_mean[1:20, 1], col = 7, lty = 7)
lines(1:20, subnational_double_gap_fore_female_ARIMA_mean[1:20, 1], col = 8, lty = 8)

horizon_point_fore_subnational_err_F_ARIMA_mean = cbind(horizon_point_fore_subnational_err_F_EVR_ARIMA_mean[1:20, 1],
                                                      horizon_point_fore_subnational_err_MFTS_F_EVR_ARIMA_mean[1:20, 1],
                                                      horizon_point_fore_subnational_err_MLFTS_F_EVR_ARIMA_mean[1:20, 1],
                                                      
                                                      horizon_point_fore_subnational_err_HDFPCA_F_CDF_mean[1:20, 1],
                                                      horizon_point_fore_subnational_err_FANOVA_FFM_F_ARIMA_mean[1:20, 1],
                                                      
                                                      horizon_point_fore_subnational_err_F_K6_ARIMA_mean[1:20, 1],
                                                      horizon_point_fore_subnational_err_MFTS_F_K6_ARIMA_mean[1:20, 1],
                                                      horizon_point_fore_subnational_err_MLFTS_F_K6_ARIMA_mean[1:20, 1],
                                                      
                                                      subnational_gender_gap_KLD_JSD_F_ARIMA_mean[1:20, 1],
                                                      subnational_region_gap_fore_female_ARIMA_mean[1:20, 1],
                                                      subnational_double_gap_fore_female_ARIMA_mean[1:20, 1])

colnames(horizon_point_fore_subnational_err_F_ARIMA_mean) = c("UFTS (EVR)", "MFTS (EVR)", "MLFTS (EVR)", "HDFPCA", "FANOVA+FFM",
                                                              "UFTS (K=6)", "MFTS (K=6)", "MLFTS (K=6)", "Gender gap", "Region gap", "Double gap")

#######
# Male
#######

## prefectures

prefecture_M_ARIMA_KLD_res = cbind(point_fore_subnational_err_M_EVR_ARIMA_mean[1:47, 1],
                                   point_fore_subnational_err_MFTS_M_EVR_ARIMA_mean[1:47, 1],
                                   point_fore_subnational_err_MLFTS_M_EVR_ARIMA_mean[1:47, 1],
                                   
                                   point_fore_subnational_err_HDFPCA_M_CDF_mean[1:47, 1],
                                   point_fore_subnational_err_FANOVA_FFM_M_ARIMA_mean[1:47, 1],
                                   
                                   point_fore_subnational_err_M_K6_ARIMA_mean[1:47, 1],
                                   point_fore_subnational_err_MFTS_M_K6_ARIMA_mean[1:47, 1],
                                   point_fore_subnational_err_MLFTS_M_K6_ARIMA_mean[1:47, 1],
                                   
                                   subnational_gender_gap_KLD_JSD_M_ARIMA_prefecture_mean[,1],
                                   subnational_region_gap_fore_male_ARIMA_prefecture_mean[,1],
                                   subnational_double_gap_fore_male_ARIMA_prefecture_mean[,1])
colnames(prefecture_M_ARIMA_KLD_res) = colnames(prefecture_F_ARIMA_KLD_res) 

# draw a boxplot

savepdf("KLD_M_ARIMA_CDF", width = 12, height = 10, toplines = 0.8, pointsize = 8)
par(mar = c(5, 4, 3, 2))
boxplot(prefecture_M_ARIMA_KLD_res, ylab = "KLD", las = 1, xaxt = "n", main = "CDF transformation (Male data)", 
        outline = FALSE, ylim = c(0, 0.35))
text(x = 1:ncol(prefecture_M_ARIMA_KLD_res), y = par("usr")[3] - 0.02 * diff(par("usr")[3:4]),
     labels = colnames(prefecture_M_ARIMA_KLD_res), srt = 45, adj = 1, xpd = TRUE)
dev.off()

## horizons

plot(1:20, horizon_point_fore_subnational_err_M_EVR_ARIMA_mean[1:20, 1], type = "l", col = 1, 
     lty = 1, ylab = "KLD", xlab = "Forecast horizon (h)", ylim = c(0, 0.12))
lines(1:20, horizon_point_fore_subnational_err_MFTS_M_EVR_ARIMA_mean[1:20, 1], col = 2, lty = 1)
lines(1:20, horizon_point_fore_subnational_err_MLFTS_M_EVR_ARIMA_mean[1:20, 1], col = 3, lty = 1)

lines(1:20, horizon_point_fore_subnational_err_HDFPCA_M_CDF_mean[1:20, 1], col = 4, lty = 3)
lines(1:20, horizon_point_fore_subnational_err_FANOVA_FFM_M_ARIMA_mean[1:20, 1], col = 5, lty = 4)

lines(1:20, horizon_point_fore_subnational_err_M_K6_ARIMA_mean[1:20, 1], col = 1, lty = 2)
lines(1:20, horizon_point_fore_subnational_err_MFTS_M_K6_ARIMA_mean[1:20, 1], col = 2, lty = 2)
lines(1:20, horizon_point_fore_subnational_err_MLFTS_M_K6_ARIMA_mean[1:20, 1], col = 3, lty = 2)

horizon_point_fore_subnational_err_M_ARIMA_mean = cbind(horizon_point_fore_subnational_err_M_EVR_ARIMA_mean[1:20, 1],
                                                        horizon_point_fore_subnational_err_MFTS_M_EVR_ARIMA_mean[1:20, 1],
                                                        horizon_point_fore_subnational_err_MLFTS_M_EVR_ARIMA_mean[1:20, 1],
                                                        
                                                        horizon_point_fore_subnational_err_HDFPCA_M_CDF_mean[1:20, 1],
                                                        horizon_point_fore_subnational_err_FANOVA_FFM_M_ARIMA_mean[1:20, 1],
                                                        
                                                        horizon_point_fore_subnational_err_M_K6_ARIMA_mean[1:20, 1],
                                                        horizon_point_fore_subnational_err_MFTS_M_K6_ARIMA_mean[1:20, 1],
                                                        horizon_point_fore_subnational_err_MLFTS_M_K6_ARIMA_mean[1:20, 1],
                                                        
                                                        subnational_gender_gap_KLD_JSD_M_ARIMA_mean[1:20, 1],
                                                        subnational_region_gap_fore_male_ARIMA_mean[1:20, 1],
                                                        subnational_double_gap_fore_male_ARIMA_mean[1:20, 1])

colnames(horizon_point_fore_subnational_err_M_ARIMA_mean) = c("UFTS (EVR)", "MFTS (EVR)", "MLFTS (EVR)", "HDFPCA", "FANOVA+FFM",
                                                              "UFTS (K=6)", "MFTS (K=6)", "MLFTS (K=6)", "Gender gap", "Region gap", "Double gap")

round(colMeans(horizon_point_fore_subnational_err_M_ARIMA_mean), 4)

#########
# xtable
#########

xtable(rbind(horizon_point_fore_subnational_err_F_ARIMA_mean[c(1, 5, 10, 15, 20),],
             apply(horizon_point_fore_subnational_err_F_ARIMA_mean, 2, mean),
             apply(horizon_point_fore_subnational_err_F_ARIMA_mean, 2, median),
             
             horizon_point_fore_subnational_err_M_ARIMA_mean[c(1, 5, 10, 15, 20),],
             apply(horizon_point_fore_subnational_err_M_ARIMA_mean, 2, mean),
             apply(horizon_point_fore_subnational_err_M_ARIMA_mean, 2, median)), digits = 4)
