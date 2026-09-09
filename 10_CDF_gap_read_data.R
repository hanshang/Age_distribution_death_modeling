######################
# model HDFTS of CDFs
######################

# load R package

source("load_packages.R")

# read Japanese Subnational Human Mortality Data

state = c("Hokkaido", "Aomori", "Iwate", "Miyagi", "Akita", "Yamagata", "Fukushima","Ibaraki", "Tochigi", 
          "Gunma", "Saitama", "Chiba", "Tokyo", "Kanagawa", "Niigata", "Toyama", "Ishikawa", "Fukui", 
          "Yamanashi", "Nagano", "Gifu", "Shizuoka", "Aichi", "Mie", "Shiga", "Kyoto", "Osaka", "Hyogo", 
          "Nara", "Wakayama", "Tottori", "Shimane", "Okayama", "Hiroshima", "Yamaguchi", "Tokushima", 
          "Kagawa", "Ehime", "Kochi", "Fukuoka", "Saga", "Nagasaki", "Kumamoto", "Oita", "Miyazaki", 
          "Kagoshima", "Okinawa")

# change file directory

ages = 0:110
n_age = length(ages)
years = 1947:2024
n_year = length(years)

female_prefecture_qx = male_prefecture_qx = list()
prefecture_n_year = vector("numeric", length(state))
for(ik in 1:length(state))
{
    if(ik < 10)
    {
        code = paste("0",ik,sep="")
    }
    else
    {
        code = as.character(ik)
    }
    F_qx = readJMDweb(code, item = "fltper_1x1", fixup = TRUE)$qx
    female_prefecture_qx[[ik]] = t(matrix(F_qx, n_age, length(F_qx)/n_age))
    prefecture_n_year[ik] = length(F_qx)/n_age
    rm(F_qx)
    
    M_qx = readJMDweb(code, item = "mltper_1x1", fixup = TRUE)$qx
    male_prefecture_qx[[ik]]   = t(matrix(M_qx, n_age, length(M_qx)/n_age))
    print(ik); rm(ik); rm(code)
}

# Japanese subnational data (normalized life-table death counts)

female_prefecture_dx = male_prefecture_dx = list()
for(iw in 1:length(state))
{
    female_prefecture_dum = male_prefecture_dum = matrix(NA, prefecture_n_year[iw], n_age)
    for(ij in 1:prefecture_n_year[iw])
    {
        start_pop_female = start_pop_male = 1
        for(ik in 1:n_age)
        {
            female_prefecture_dum[ij,ik] = (female_prefecture_qx[[iw]])[ij,ik] * start_pop_female
            start_pop_female = start_pop_female - female_prefecture_dum[ij,ik]
            
            male_prefecture_dum[ij,ik] = (male_prefecture_qx[[iw]])[ij,ik] * start_pop_male
            start_pop_male = start_pop_male - male_prefecture_dum[ij,ik]
        }
    }
    rownames(female_prefecture_dum) = rownames(male_prefecture_dum) = tail(years, prefecture_n_year[iw])
    colnames(female_prefecture_dum) = colnames(male_prefecture_dum) = ages
    
    female_prefecture_dx[[iw]] = female_prefecture_dum
    male_prefecture_dx[[iw]]   = male_prefecture_dum
    rm(female_prefecture_dum); rm(male_prefecture_dum)
    print(iw); rm(iw)
}

######################
# Japan national data
######################

Japan_female_qx = t(matrix(readJMDweb(prefID = "00", item = "fltper_1x1", fixup = TRUE)$qx, n_age, n_year))
Japan_male_qx   = t(matrix(readJMDweb(prefID = "00", item = "mltper_1x1", fixup = TRUE)$qx, n_age, n_year))

Japan_female_pop = Japan_male_pop = Japan_total_pop = matrix(NA, n_year, n_age)
for(ij in 1:n_year)
{
    # radix is 1
    start_pop_female = start_pop_male = start_pop_total = 1
    for(ik in 1:n_age)
    {
        # Female
        
        Japan_female_pop[ij,ik] = Japan_female_qx[ij,ik] * start_pop_female
        start_pop_female = start_pop_female - Japan_female_pop[ij,ik]
        
        # Male
        
        Japan_male_pop[ij,ik] = Japan_male_qx[ij,ik] * start_pop_male
        start_pop_male = start_pop_male - Japan_male_pop[ij,ik]
    }
    rm(ij)
}
rownames(Japan_female_pop) = rownames(Japan_male_pop) = years
colnames(Japan_female_pop) = colnames(Japan_male_pop) = ages

##################################
# compute CDF from PDF via cumsum
##################################

subnational_female_prefecture_dx_CDF = subnational_male_prefecture_dx_CDF = list()
for(ik in 1:47)
{
    subnational_female_pop_CDF = subnational_male_pop_CDF = matrix(NA, n_age, prefecture_n_year[ik])
    for(ij in 1:prefecture_n_year[ik])
    {
        subnational_female_pop_CDF[,ij] = cumsum((female_prefecture_dx[[ik]])[ij,])
        subnational_male_pop_CDF[,ij]   = cumsum((male_prefecture_dx[[ik]])[ij,])
        rm(ij)
    }
    subnational_female_prefecture_dx_CDF[[ik]] = subnational_female_pop_CDF
    subnational_male_prefecture_dx_CDF[[ik]]   = subnational_male_pop_CDF
    rm(ik)
}

#########################
# subnational gender gap
#########################

subnational_gender_diff_CDF = list()
for(ik in 1:47)
{
    subnational_gender_diff_CDF[[ik]] = subnational_male_prefecture_dx_CDF[[ik]] - subnational_female_prefecture_dx_CDF[[ik]]
    rm(ik)
}

##########
# Figures
##########

savefig("Fig_1a", width = 12, height = 10, toplines = 0.8, type = "png")
plot(fts(ages, t(Japan_female_pop)), xlab = "Age", ylab = "Normalized life-table death count", 
     main = "Japanese female data (1947-2023)", ylim = c(0, 8.85*10^-2))
dev.off()

savefig("Fig_1b", width = 12, height = 10, toplines = 0.8, type = "png")
plot(fts(ages, t(Japan_male_pop)), xlab = "Age", ylab = "Normalized life-table death count", 
     main = "Japanese male data (1947-2023)", ylim = c(0, 8.85*10^-2))
dev.off()

####################################################
# CDF (cumulative relative life-table death counts)
####################################################

# national

Japan_female_pop_CDF = Japan_male_pop_CDF = matrix(NA, n_age, n_year)
for(ik in 1:n_year)
{
    Japan_female_pop_CDF[,ik] = cumsum((Japan_female_pop)[ik,])
    Japan_male_pop_CDF[,ik]   = cumsum((Japan_male_pop)[ik,])
    rm(ik)
}
colnames(Japan_female_pop_CDF) = colnames(Japan_male_pop_CDF) = years
rownames(Japan_female_pop_CDF) = rownames(Japan_male_pop_CDF) = ages

# Figures

savefig("Fig_2a", width = 12, height = 10, toplines = 0.8, type = "png")
plot(fts(ages, Japan_female_pop_CDF), xlab = "Age", ylab = "CDF of normalized dx", 
     main = "")
dev.off()

savefig("Fig_2b", width = 12, height = 10, toplines = 0.8, type = "png")
plot(fts(ages, Japan_male_pop_CDF), xlab = "Age", ylab = "CDF of normalized dx", 
     main = "")
dev.off()

