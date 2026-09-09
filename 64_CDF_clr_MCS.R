#######################
# model confidence set
#######################

require(MCS)

## CDF transformation

# KLD

MCS_ETS_F_KLD = MCSprocedure(prefecture_F_ETS_KLD_res, alpha = 0.8, B = 5000) # HDFPCA
MCS_ETS_M_KLD = MCSprocedure(prefecture_M_ETS_KLD_res, alpha = 0.8, B = 5000) # FANOVA + FFM 

MCS_ARIMA_F_KLD = MCSprocedure(prefecture_F_ARIMA_KLD_res, alpha = 0.8, B = 5000) # HDFPCA
MCS_ARIMA_M_KLD = MCSprocedure(prefecture_M_ARIMA_KLD_res, alpha = 0.8, B = 5000) # Region gap

# CPD (80% nominal coverage)

MCS_ETS_F_CPD = MCSprocedure(CDF_F_ETS_int_res_CPD_prefecture, alpha = 0.8, B = 5000) # Region gap
MCS_ETS_M_CPD = MCSprocedure(CDF_M_ETS_int_res_CPD_prefecture, alpha = 0.8, B = 5000) # Double gap

MCS_ARIMA_F_CPD = MCSprocedure(CDF_F_ARIMA_int_res_CPD_prefecture, alpha = 0.8, B = 5000) # UFTS (K=6) Gender gap HDFPCA
MCS_ARIMA_M_CPD = MCSprocedure(CDF_M_ARIMA_int_res_CPD_prefecture, alpha = 0.8, B = 5000) # DOuble gap

# CPD (95% nominal coverage)

MCS_ETS_F_CPD_95 = MCSprocedure(CDF_F_ETS_int_res_CPD_95_prefecture, alpha = 0.8, B = 5000) # HDFPCA
MCS_ETS_M_CPD_95 = MCSprocedure(CDF_M_ETS_int_res_CPD_95_prefecture, alpha = 0.8, B = 5000) # UFTS (K=6)

MCS_ARIMA_F_CPD_95 = MCSprocedure(CDF_F_ARIMA_int_res_CPD_95_prefecture, alpha = 0.8, B = 5000) # HDFPCA
MCS_ARIMA_M_CPD_95 = MCSprocedure(CDF_M_ARIMA_int_res_CPD_95_prefecture, alpha = 0.8, B = 5000) # FANOVA + FFM

# MIS (80% nominal coverage)

MCS_ETS_F_MIS = MCSprocedure(CDF_F_ETS_int_res_score_prefecture, alpha = 0.8, B = 5000) # HDFPCA
MCS_ETS_M_MIS = MCSprocedure(CDF_M_ETS_int_res_score_prefecture, alpha = 0.8, B = 5000) # MFTS (K=6)

MCS_ARIMA_F_MIS = MCSprocedure(CDF_F_ARIMA_int_res_score_prefecture, alpha = 0.8, B = 5000) # HDFPCA
MCS_ARIMA_M_MIS = MCSprocedure(CDF_M_ARIMA_int_res_score_prefecture, alpha = 0.8, B = 5000) # Region gap

# MIS (95% nominal coverage)

MCS_ETS_F_MIS_95 = MCSprocedure(CDF_F_ETS_int_res_score_95_prefecture, alpha = 0.8, B = 5000) # Region gap
MCS_ETS_M_MIS_95 = MCSprocedure(CDF_M_ETS_int_res_score_95_prefecture, alpha = 0.8, B = 5000) # Region gap MLFTS(K=6) UFTS(EVR) MFTS(K=6) UFTS(K=6)

MCS_ARIMA_F_MIS_95 = MCSprocedure(CDF_F_ARIMA_int_res_score_95_prefecture, alpha = 0.8, B = 5000) # HDFPCA
MCS_ARIMA_M_MIS_95 = MCSprocedure(CDF_M_ARIMA_int_res_score_95_prefecture, alpha = 0.8, B = 5000) # Region gap


## clr transformation

# KLD

MCS_ETS_F_KLD_CLR = MCSprocedure(prefecture_F_ETS_KLD_res_CLR, alpha = 0.8, B = 5000) # MFTS (EVR)
MCS_ETS_M_KLD_CLR = MCSprocedure(prefecture_M_ETS_KLD_res_CLR, alpha = 0.8, B = 5000) # FANOVA + FFM

MCS_ARIMA_F_KLD_CLR = MCSprocedure(prefecture_F_ARIMA_KLD_res_CLR, alpha = 0.8, B = 5000) # HDFPCA
MCS_ARIMA_M_KLD_CLR = MCSprocedure(prefecture_M_ARIMA_KLD_res_CLR, alpha = 0.8, B = 5000) # MFTS (EVR) MFTS(K=6) Region gap

# CPD (80% nominal coverage)

MCS_ETS_F_CPD_CLR = MCSprocedure(clr_F_ETS_int_res_CPD_prefecture, alpha = 0.8, B = 5000) # MLFTS (K=6) Gender gap Region gap Double gap UFTS (EVR)
MCS_ETS_M_CPD_CLR = MCSprocedure(clr_M_ETS_int_res_CPD_prefecture, alpha = 0.8, B = 5000) # Region gap

MCS_ARIMA_F_CPD_CLR = MCSprocedure(clr_F_ARIMA_int_res_CPD_prefecture, alpha = 0.8, B = 5000) # UFTS (EVR)
MCS_ARIMA_M_CPD_CLR = MCSprocedure(clr_M_ARIMA_int_res_CPD_prefecture, alpha = 0.8, B = 5000) # Region gap

# CPD (95% nominal coverage)

MCS_ETS_F_CPD_95_CLR = MCSprocedure(clr_F_ETS_int_res_CPD_95_prefecture, alpha = 0.8, B = 5000) # Region gap
MCS_ETS_M_CPD_95_CLR = MCSprocedure(clr_M_ETS_int_res_CPD_95_prefecture, alpha = 0.8, B = 5000) # Region gap

MCS_ARIMA_F_CPD_95_CLR = MCSprocedure(clr_F_ARIMA_int_res_CPD_95_prefecture, alpha = 0.8, B = 5000) # UFTS (EVR)
MCS_ARIMA_M_CPD_95_CLR = MCSprocedure(clr_M_ARIMA_int_res_CPD_95_prefecture, alpha = 0.8, B = 5000) # MFTS (EVR)

# MIS (80% nominal coverage)

MCS_ETS_F_MIS_CLR = MCSprocedure(clr_F_ETS_int_res_score_prefecture, alpha = 0.8, B = 5000) # MFTS (EVR)
MCS_ETS_M_MIS_CLR = MCSprocedure(clr_M_ETS_int_res_score_prefecture, alpha = 0.8, B = 5000) # MFTS (EVR)

MCS_ARIMA_F_MIS_CLR = MCSprocedure(clr_F_ARIMA_int_res_score_prefecture, alpha = 0.8, B = 5000) # HDFPCA
MCS_ARIMA_M_MIS_CLR = MCSprocedure(clr_M_ARIMA_int_res_score_prefecture, alpha = 0.8, B = 5000) # Region gap

# MIS (95% nominal coverage)

MCS_ETS_F_MIS_95_CLR = MCSprocedure(clr_F_ETS_int_res_score_95_prefecture, alpha = 0.8, B = 5000) # Region gap
MCS_ETS_M_MIS_95_CLR = MCSprocedure(clr_M_ETS_int_res_score_95_prefecture, alpha = 0.8, B = 5000) # MFTS (EVR)

MCS_ARIMA_F_MIS_95_CLR = MCSprocedure(clr_F_ARIMA_int_res_score_95_prefecture, alpha = 0.8, B = 5000) # HDFPCA
MCS_ARIMA_M_MIS_95_CLR = MCSprocedure(clr_M_ARIMA_int_res_score_95_prefecture, alpha = 0.8, B = 5000) # Region gap

