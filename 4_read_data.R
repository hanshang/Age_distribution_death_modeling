setwd("~/Library/CloudStorage/Dropbox/Todos/FANOVA_FFM_CDF/code")
source("load_packages.R")

######################
# model HDFTS of CDFs
######################

# read Japanese Subnational Human Mortality Data

state = c("Hokkaido", "Aomori", "Iwate", "Miyagi", "Akita", "Yamagata", "Fukushima", "Ibaraki", "Tochigi", 
          "Gunma", "Saitama", "Chiba", "Tokyo", "Kanagawa", "Niigata", "Toyama", "Ishikawa", "Fukui", 
          "Yamanashi", "Nagano", "Gifu", "Shizuoka", "Aichi", "Mie", "Shiga", "Kyoto", "Osaka", "Hyogo", 
          "Nara", "Wakayama", "Tottori", "Shimane", "Okayama", "Hiroshima", "Yamaguchi", "Tokushima", 
          "Kagawa", "Ehime", "Kochi", "Fukuoka", "Saga", "Nagasaki", "Kumamoto", "Oita", "Miyazaki", 
          "Kagoshima", "Okinawa")
n_state = length(state)

age = ages = 0:110
n_age = length(ages)
years = 1973:2024
n_year = length(years)

#############################################
# Okinawa prefecture has data only from 1973
# we retain data from 1973 onwards
#############################################

female_qx_list = male_qx_list = list()
for(ik in 1:n_state)
{
    if(ik < 10)
    {
        dum_F = matrix(readJMDweb(paste("0", ik, sep=""), "fltper_1x1", fixup = FALSE)$qx, n_age, )
        dum_M = matrix(readJMDweb(paste("0", ik, sep=""), "mltper_1x1", fixup = FALSE)$qx, n_age, )
    }
    else
    {
        dum_F = matrix(readJMDweb(as.character(ik), "fltper_1x1", fixup = FALSE)$qx, n_age, )
        dum_M = matrix(readJMDweb(as.character(ik), "mltper_1x1", fixup = FALSE)$qx, n_age, )
    }
    if(ncol(dum_F) > n_year)
    {
        dum_F_mat = dum_F[,tail((1:ncol(dum_F)), n_year)]
        dum_M_mat = dum_M[,tail((1:ncol(dum_F)), n_year)]
        colnames(dum_F_mat) = colnames(dum_M_mat) = years
        rownames(dum_F_mat) = rownames(dum_M_mat) = ages
        female_qx_list[[ik]] = t(dum_F_mat)
        male_qx_list[[ik]] = t(dum_M_mat)
    }
    else if(ncol(dum_F) == n_year)
    {
        colnames(dum_F) = colnames(dum_M) = years
        rownames(dum_F) = rownames(dum_M) = ages
        female_qx_list[[ik]] = t(dum_F)
        male_qx_list[[ik]] = t(dum_M)
    }
    else
    {
        stop("year is not matching.")
    }
    print(ik); rm(ik)
}

################################################
# Japanese national life-table death count data
################################################

dum_F = matrix(readJMDweb("00", "fltper_1x1", fixup = FALSE)$qx, n_age, )
dum_M = matrix(readJMDweb("00", "mltper_1x1", fixup = FALSE)$qx, n_age, )
               
dum_F_mat = dum_F[,tail((1:ncol(dum_F)), n_year)]
dum_M_mat = dum_M[,tail((1:ncol(dum_M)), n_year)]

colnames(dum_F_mat) = colnames(dum_M_mat) = years
rownames(dum_F_mat) = rownames(dum_M_mat) = ages

Japan_female_dum = Japan_male_dum = matrix(NA, n_year, n_age)
for(ij in 1:n_year)
{
    # set radix (normalising to 1 or keep it as 10^5)
    start_pop_female = start_pop_male = start_pop_total = 10^5
    for(ik in 1:n_age)
    {
        Japan_female_dum[ij,ik] = t(dum_F_mat)[ij,ik] * start_pop_female
        start_pop_female = start_pop_female - Japan_female_dum[ij,ik]
        
        Japan_male_dum[ij,ik] = t(dum_M_mat)[ij,ik] * start_pop_male
        start_pop_male = start_pop_male - Japan_male_dum[ij,ik]
        rm(ik)
    }
    rm(ij)
}
rownames(Japan_female_dum) = rownames(Japan_male_dum) = years
colnames(Japan_female_dum) = colnames(Japan_male_dum) = ages
rm(dum_F); rm(dum_M); rm(dum_F_mat); rm(dum_M_mat)

##########################
# Figure 1 (rainbow plot)
##########################

savepdf("Japan_female_LTDC", width = 12, height = 10, toplines = 0.8)
plot(fts(0:110, t(Japan_female_dum)), xlab = "Age", ylab = "Life-table death count", main = "Japanese national female data (1973 - 2024)",
     ylim = range(range(Japan_female_dum), range(Japan_male_dum)))
dev.off()

savepdf("Japan_male_LTDC", width = 12, height = 10, toplines = 0.8)
plot(fts(0:110, t(Japan_male_dum)), xlab = "Age", ylab = "Life-table death count", main = "Japanese national male data (1973 - 2024)",
     ylim = range(range(Japan_female_dum), range(Japan_male_dum)))
dev.off()

savepdf("Japan_female_CDF", width = 12, height = 10, toplines = 0.8)
plot(fts(0:110, Japan_female_pop_CDF), xlab = "Age", ylab = "CDF", main = "Japanese national female data (1973 - 2024)")
dev.off()

savepdf("Japan_male_CDF", width = 12, height = 10, toplines = 0.8)
plot(fts(0:110, Japan_male_pop_CDF), xlab = "Age", ylab = "CDF", main = "Japanese national male data (1973 - 2024)")
dev.off()

savepdf("Japan_gender_gap", width = 12, height = 10, toplines = 0.8)
plot(fts(0:110, (Japan_male_pop_CDF - Japan_female_pop_CDF)), xlab = "Age", 
     ylab = expression(CDF[M-F]), main = "Gender gap (1973 - 2024)")
dev.off()

####################################################
# Japanese subnational life-table death count data
####################################################

female_prefecture_dx = male_prefecture_dx = list()
for(iw in 1:length(state))
{
    female_prefecture_dum = male_prefecture_dum = matrix(NA, n_year, n_age)
    for(ij in 1:n_year)
    {
        # set radix (normalising to 1 or keep it as 10^5)
        start_pop_female = start_pop_male = start_pop_total = 10^5
        for(ik in 1:n_age)
        {
            female_prefecture_dum[ij,ik] = (female_qx_list[[iw]])[ij,ik] * start_pop_female
            start_pop_female = start_pop_female - female_prefecture_dum[ij,ik]
            
            male_prefecture_dum[ij,ik] = (male_qx_list[[iw]])[ij,ik] * start_pop_male
            start_pop_male = start_pop_male - male_prefecture_dum[ij,ik]
            rm(ik)
        }
        rm(ij)
    }
    female_prefecture_dx[[iw]] = female_prefecture_dum
    male_prefecture_dx[[iw]]   = male_prefecture_dum
    rm(female_prefecture_dum); rm(male_prefecture_dum)
    print(iw); rm(iw)
}

# from list to array

female_prefecture_dx_array = male_prefecture_dx_array = array(NA, dim = c(n_year, n_age, n_state), 
                                                              dimnames = list(years, ages, state))
for(ij in 1:n_state)
{
    female_prefecture_dx_array[,,ij] = female_prefecture_dx[[ij]]
    male_prefecture_dx_array[,,ij] = male_prefecture_dx[[ij]]
    rm(ij)
}

# from array to long matrix

female_prefecture_dx_array_mat = male_prefecture_dx_array_mat = matrix(NA, 52 * 47, 111)
for(ij in 1:47)
{
    female_prefecture_dx_array_mat[((ij - 1) * 52 + 1):(ij * 52),] = female_prefecture_dx_array[,,ij]
    male_prefecture_dx_array_mat[((ij - 1) * 52 + 1):(ij * 52),] = male_prefecture_dx_array[,,ij]
    rm(ij)
}

##################################
# subnational gender gap in Japan
##################################

subnational_female_prefecture_dx_CDF = subnational_male_prefecture_dx_CDF =
subnational_gender_diff_CDF = subnational_gender_diff_CDF_integral_measure =
subnational_gender_diff_CDF_W1 = list()
for(ik in 1:47)
{
    # compute cumulative relative life-table death counts
    
    subnational_female_pop_CDF = subnational_male_pop_CDF = matrix(NA, n_age, n_year)
    for(ij in 1:n_year)
    {
        subnational_female_pop_CDF[,ij] = cumsum((female_prefecture_dx[[ik]]/10^5)[ij,])
        subnational_male_pop_CDF[,ij]   = cumsum((male_prefecture_dx[[ik]]/10^5)[ij,])
        rm(ij)
    }
    subnational_female_prefecture_dx_CDF[[ik]] = subnational_female_pop_CDF
    subnational_male_prefecture_dx_CDF[[ik]]   = subnational_male_pop_CDF
    
    # compute subnational gender gap
    
    subnational_gender_diff_CDF[[ik]] = subnational_male_prefecture_dx_CDF[[ik]] - subnational_female_prefecture_dx_CDF[[ik]]
    
    # integral measure
    
    subnational_gender_diff_CDF_integral_measure[[ik]] = apply(subnational_gender_diff_CDF[[ik]], 2, sum)
    
    # Wasserstein of order 1
    
    wasserstein1d_value = vector("numeric", n_year)
    for(ij in 1:n_year)
    {
        wasserstein1d_value[ij] = wasserstein1d(a = (female_prefecture_dx[[ik]])[ij,], b = (male_prefecture_dx[[ik]])[ij,])
        rm(ij)
    }
    subnational_gender_diff_CDF_W1[[ik]] = wasserstein1d_value
    
    rm(subnational_female_pop_CDF); rm(subnational_male_pop_CDF)
    rm(ik); rm(wasserstein1d_value)
}

###########################
# from a list to an matrix
###########################

subnational_gender_diff_CDF_integral_measure_mat = subnational_gender_diff_CDF_W1_measure_mat = matrix(NA, n_year, 47)
for(ik in 1:47)
{
    subnational_gender_diff_CDF_integral_measure_mat[,ik] = subnational_gender_diff_CDF_integral_measure[[ik]]
    subnational_gender_diff_CDF_W1_measure_mat[,ik] = subnational_gender_diff_CDF_W1[[ik]]
    rm(ik)
}
rownames(subnational_gender_diff_CDF_integral_measure_mat) = rownames(subnational_gender_diff_CDF_W1_measure_mat) = years
colnames(subnational_gender_diff_CDF_integral_measure_mat) = colnames(subnational_gender_diff_CDF_W1_measure_mat) = state

####################################################################################
# CDF (cumulative relative life-table death counts)
# compute region gap, most prefectures from 1947 to 2024, Okinawa from 1973 to 2024
####################################################################################

# national

Japan_female_pop_CDF = Japan_male_pop_CDF = matrix(NA, n_age, n_year)
for(ik in 1:n_year)
{
    Japan_female_pop_CDF[,ik] = cumsum((Japan_female_dum/10^5)[ik,])
    Japan_male_pop_CDF[,ik]   = cumsum((Japan_male_dum/10^5)[ik,])
    rm(ik)
}
colnames(Japan_female_pop_CDF) = colnames(Japan_male_pop_CDF) = years
rownames(Japan_female_pop_CDF) = rownames(Japan_male_pop_CDF) = ages

# subnational

subnational_region_M_diff_CDF = subnational_region_F_diff_CDF = list()
for(ik in 1:n_state)
{
    # region gap of (subnational - national CDFs)
    
    subnational_region_M_diff_CDF[[ik]] = (subnational_male_prefecture_dx_CDF[[ik]]) - Japan_male_pop_CDF
    subnational_region_F_diff_CDF[[ik]] = (subnational_female_prefecture_dx_CDF[[ik]]) - Japan_female_pop_CDF
    rm(ik)
}

