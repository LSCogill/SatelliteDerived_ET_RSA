# Ensemble stats - INTERANNUAL
library(tidyverse)
library(tidyquant)
library(plotly)
library(ggplot2)
library(hydroGOF) 
library(dplyr)
library(xts)
library(zoo)

# Load the dataset
setwd("C:\\Users\\Liam\\OneDrive - Stellenbosch University\\ET_Data\\Ensemble_Data")

BF1 <- read.csv("ET_Monthly_BF1_ensemble.csv", skip=0, sep = ",")
BF2 <- read.csv("ET_Monthly_BF2_ensemble.csv", skip=0, sep = ",")
CP3 <- read.csv("ET_Monthly_CP3_ensemble.csv", skip=0, sep = ",")
CP6 <- read.csv("ET_Monthly_CP6_ensemble.csv", skip=0, sep = ",")
CP9 <- read.csv("ET_Monthly_CP9_ensemble.csv", skip=0, sep = ",")
EW1 <- read.csv("ET_Monthly_EW1_ensemble.csv", skip=0, sep = ",")
EW2 <- read.csv("ET_Monthly_EW2_ensemble.csv", skip=0, sep = ",")
EZU <- read.csv("ET_Monthly_EZU_ensemble.csv", skip=0, sep = ",")
JHK <- read.csv("ET_Monthly_JHK_ensemble.csv", skip=0, sep = ",")
MB1 <- read.csv("ET_Monthly_MB1_ensemble.csv", skip=0, sep = ",")
MB2 <- read.csv("ET_Monthly_MB2_ensemble.csv", skip=0, sep = ",")
MCP <- read.csv("ET_Monthly_MCP_ensemble.csv", skip=0, sep = ",")
MLP <- read.csv("ET_Monthly_MLP_ensemble.csv", skip=0, sep = ",")
SKU <- read.csv("ET_Monthly_SKU_ensemble.csv", skip=0, sep = ",")


######## INTERANNUAL ##############################################################

# BF1
# Split into hydrological years (if the timeseries spans over the course of two or more years)
year1 <- BF1[6:17, ]
year2 <- BF1[18:29, ]
year3 <- BF1[30:41, ]

# Mean ET for each of the yrs
mean_yr1 <- apply(year1[, 5:6], 2, mean, na.rm = TRUE)
mean_yr2 <- apply(year2[, 5:6], 2, mean, na.rm = TRUE)
mean_yr3 <- apply(year3[, 5:6], 2, mean, na.rm = TRUE)

# Bias, PBias, R2, KGE, RSR, NSE, MAE for each year
yr_df <- data.frame(
  Variable = names(mean_yr1),  
  yr1 = mean_yr1,
  yr2 = mean_yr2,
  yr3 = mean_yr3
)

yr_df_correct <- t(yr_df)
yr_df_BF1 <- as.data.frame(yr_df_correct)
yr_df_BF1 <- yr_df_BF1[-1, ]
yr_df_BF1 <- as.data.frame(lapply(yr_df_BF1, as.numeric))

# Stats 
kge_valueBF1 <- KGE(sim = yr_df_BF1$mean_ET, obs = yr_df_BF1$field_et)
rmse_valueBF1 <- rmse(actual = yr_df_BF1$field_et, predicted = yr_df_BF1$mean_ET)
nse_valueBF1 <- NSE(sim = yr_df_BF1$mean_ET, obs = yr_df_BF1$field_et)
rsr_valueBF1 <- rmse_valueBF1 / sd(yr_df_BF1$field_et)

biasBF1 <- mean(yr_df_BF1$mean_ET - yr_df_BF1$field_et)
pbiasBF1 <- 100 * sum(yr_df_BF1$mean_ET - yr_df_BF1$field_et) / sum(yr_df_BF1$field_et)
maeBF1 <- mean(abs(yr_df_BF1$mean_ET - yr_df_BF1$field_et))

spearman_corrBF1 <- cor.test(yr_df_BF1$field_et, yr_df_BF1$mean_ET, method = "spearman")
print(spearman_corrBF1)
correlation_coefficientBF1 <- spearman_corrBF1$estimate
p_valueBF1 <- spearman_corrBF1$p.value
##################################################################################

# BF2
# No complete year for interannual analyses

###################################################################################

# CP3
# Split into hydrological years (if the timeseries spans over the course of two or more years)
year1 <- CP3[14:25, ]
year2 <- CP3[26:37, ] 
year3 <- CP3[38:49, ] 

# Mean ET for each of the yrs
mean_yr1 <- apply(year1[, 5:6], 2, mean, na.rm = TRUE)
mean_yr2 <- apply(year2[, 5:6], 2, mean, na.rm = TRUE)
mean_yr3 <- apply(year3[, 5:6], 2, mean, na.rm = TRUE)

# Bias, PBias, R2, KGE, RSR, NSE, MAE for each year
yr_df <- data.frame(
  Variable = names(mean_yr1),  
  yr1 = mean_yr1,
  yr2 = mean_yr2,
  yr3 = mean_yr3
)

yr_df_correct <- t(yr_df)
yr_df_CP3 <- as.data.frame(yr_df_correct)
yr_df_CP3 <- yr_df_CP3[-1, ]
yr_df_CP3 <- as.data.frame(lapply(yr_df_CP3, as.numeric))

# Stats 
kge_valueCP3 <- KGE(sim = yr_df_CP3$mean_ET, obs = yr_df_CP3$field_et)
rmse_valueCP3 <- rmse(actual = yr_df_CP3$field_et, predicted = yr_df_CP3$mean_ET)
nse_valueCP3 <- NSE(sim = yr_df_CP3$mean_ET, obs = yr_df_CP3$field_et)
rsr_valueCP3 <- rmse_valueCP3 / sd(yr_df_CP3$field_et)

biasCP3 <- mean(yr_df_CP3$mean_ET - yr_df_CP3$field_et)
pbiasCP3 <- 100 * sum(yr_df_CP3$mean_ET - yr_df_CP3$field_et) / sum(yr_df_CP3$field_et)
maeCP3 <- mean(abs(yr_df_CP3$mean_ET - yr_df_CP3$field_et))

spearman_corrCP3 <- cor.test(yr_df_CP3$field_et, yr_df_CP3$mean_ET, method = "spearman")
print(spearman_corrCP3)
correlation_coefficientCP3 <- spearman_corrCP3$estimate
p_valueCP3 <- spearman_corrCP3$p.value
##################################################################################

# CP6
# Split into hydrological years (if the timeseries spans over the course of two or more years)
year1 <- CP6[1:12, ]
year2 <- CP6[13:24, ] # NA - incomplete
year3 <- CP6[25:36, ] 
year4 <- CP6[37:48, ]
year5 <- CP6[49:60, ]
year6 <- CP6[61:72, ]
year7 <- CP6[73:84, ]
year8 <- CP6[85:96, ]
year9 <- CP6[97:108, ]

# Mean ET for each of the yrs
mean_yr1 <- apply(year1[, 5:6], 2, mean, na.rm = TRUE)
#mean_yr2 <- apply(year2[, 5:6], 2, mean, na.rm = TRUE)
mean_yr3 <- apply(year3[, 5:6], 2, mean, na.rm = TRUE)
mean_yr4 <- apply(year4[, 5:6], 2, mean, na.rm = TRUE)
mean_yr5 <- apply(year5[, 5:6], 2, mean, na.rm = TRUE)
mean_yr6 <- apply(year6[, 5:6], 2, mean, na.rm = TRUE)
mean_yr7 <- apply(year7[, 5:6], 2, mean, na.rm = TRUE)
mean_yr8 <- apply(year8[, 5:6], 2, mean, na.rm = TRUE)
mean_yr9 <- apply(year9[, 5:6], 2, mean, na.rm = TRUE)

# Bias, PBias, R2, KGE, RSR, NSE, MAE for each year
yr_df <- data.frame(
  Variable = names(mean_yr1),  
  yr1 = mean_yr1,
  
  yr3 = mean_yr3,
  yr4 = mean_yr4,
  yr5 = mean_yr5,
  yr6 = mean_yr6,
  yr7 = mean_yr7,
  yr8 = mean_yr8,
  yr9 = mean_yr9
)

yr_df_correct <- t(yr_df)
yr_df_CP6 <- as.data.frame(yr_df_correct)
yr_df_CP6 <- yr_df_CP6[-1, ]
yr_df_CP6 <- as.data.frame(lapply(yr_df_CP6, as.numeric))

# Stats 
kge_valueCP6 <- KGE(sim = yr_df_CP6$mean_ET, obs = yr_df_CP6$field_et)
rmse_valueCP6 <- rmse(actual = yr_df_CP6$field_et, predicted = yr_df_CP6$mean_ET)
nse_valueCP6 <- NSE(sim = yr_df_CP6$mean_ET, obs = yr_df_CP6$field_et)
rsr_valueCP6 <- rmse_valueCP6 / sd(yr_df_CP6$field_et)

biasCP6 <- mean(yr_df_CP6$mean_ET - yr_df_CP6$field_et)
pbiasCP6 <- 100 * sum(yr_df_CP6$mean_ET - yr_df_CP6$field_et) / sum(yr_df_CP6$field_et)
maeCP6 <- mean(abs(yr_df_CP6$mean_ET - yr_df_CP6$field_et))

spearman_corrCP6 <- cor.test(yr_df_CP6$field_et, yr_df_CP6$mean_ET, method = "spearman")
print(spearman_corrCP6)
correlation_coefficientCP6 <- spearman_corrCP6$estimate
p_valueCP6 <- spearman_corrCP6$p.value
#################################################################################
# CP9
# Split into hydrological years (if the timeseries spans over the course of two or more years)
year1 <- CP9[2:13, ]
year2 <- CP9[19:30, ] 
year3 <- CP9[38:49, ]

# Mean ET for each of the yrs
mean_yr1 <- apply(year1[, 5:6], 2, mean, na.rm = TRUE)
mean_yr2 <- apply(year2[, 5:6], 2, mean, na.rm = TRUE)
mean_yr3 <- apply(year3[, 5:6], 2, mean, na.rm = TRUE)

# Bias, PBias, R2, KGE, RSR, NSE, MAE for each year
yr_df <- data.frame(
  Variable = names(mean_yr1),  
  yr1 = mean_yr1,
  yr2 = mean_yr2,
  yr3 = mean_yr3
)

yr_df_correct <- t(yr_df)
yr_df_CP9 <- as.data.frame(yr_df_correct)
yr_df_CP9 <- yr_df_CP9[-1, ]
yr_df_CP9 <- as.data.frame(lapply(yr_df_CP9, as.numeric))

# Stats 
kge_valueCP9 <- KGE(sim = yr_df_CP9$mean_ET, obs = yr_df_CP9$field_et)
rmse_valueCP9 <- rmse(actual = yr_df_CP9$field_et, predicted = yr_df_CP9$mean_ET)
nse_valueCP9 <- NSE(sim = yr_df_CP9$mean_ET, obs = yr_df_CP9$field_et)
rsr_valueCP9 <- rmse_valueCP9 / sd(yr_df_CP9$field_et)

biasCP9 <- mean(yr_df_CP9$mean_ET - yr_df_CP9$field_et)
pbiasCP9 <- 100 * sum(yr_df_CP9$mean_ET - yr_df_CP9$field_et) / sum(yr_df_CP9$field_et)
maeCP9 <- mean(abs(yr_df_CP9$mean_ET - yr_df_CP9$field_et))

spearman_corrCP9 <- cor.test(yr_df_CP9$field_et, yr_df_CP9$mean_ET, method = "spearman")
print(spearman_corrCP9)
correlation_coefficientCP9 <- spearman_corrCP9$estimate
p_valueCP9 <- spearman_corrCP9$p.value
################################################################################

# EW1
# Split into hydrological years (if the timeseries spans over the course of two or more years)
year1 <- EW1[1:12, ]
year2 <- EW1[13:24, ] 
year3 <- EW1[25:34, ] 

# Mean ET for each of the yrs
mean_yr1 <- apply(year1[, 5:6], 2, mean, na.rm = TRUE)
mean_yr2 <- apply(year2[, 5:6], 2, mean, na.rm = TRUE)
mean_yr3 <- apply(year3[, 5:6], 2, mean, na.rm = TRUE)

# Bias, PBias, R2, KGE, RSR, NSE, MAE for each year
yr_df <- data.frame(
  Variable = names(mean_yr1),  
  yr1 = mean_yr1,
  yr2 = mean_yr2,
  yr3 = mean_yr3
)

yr_df_correct <- t(yr_df)
yr_df_EW1 <- as.data.frame(yr_df_correct)
yr_df_EW1 <- yr_df_EW1[-1, ]
yr_df_EW1 <- as.data.frame(lapply(yr_df_EW1, as.numeric))

# Stats 
kge_valueEW1 <- KGE(sim = yr_df_EW1$mean_ET, obs = yr_df_EW1$field_et)
rmse_valueEW1 <- rmse(actual = yr_df_EW1$field_et, predicted = yr_df_EW1$mean_ET)
nse_valueEW1 <- NSE(sim = yr_df_EW1$mean_ET, obs = yr_df_EW1$field_et)
rsr_valueEW1 <- rmse_valueEW1 / sd(yr_df_EW1$field_et)

biasEW1 <- mean(yr_df_EW1$mean_ET - yr_df_EW1$field_et)
pbiasEW1 <- 100 * sum(yr_df_EW1$mean_ET - yr_df_EW1$field_et) / sum(yr_df_EW1$field_et)
maeEW1 <- mean(abs(yr_df_EW1$mean_ET - yr_df_EW1$field_et))

spearman_corrEW1 <- cor.test(yr_df_EW1$field_et, yr_df_EW1$mean_ET, method = "spearman")
print(spearman_corrEW1)
correlation_coefficientEW1 <- spearman_corrEW1$estimate
p_valueEW1 <- spearman_corrEW1$p.value
################################################################################

# EW2
# Split into hydrological years (if the timeseries spans over the course of two or more years)
year1 <- EW2[1:12, ]
year2 <- EW2[13:24, ] 
year3 <- EW2[25:36, ] 

# Mean ET for each of the yrs
mean_yr1 <- apply(year1[, 5:6], 2, mean, na.rm = TRUE)
mean_yr2 <- apply(year2[, 5:6], 2, mean, na.rm = TRUE)
mean_yr3 <- apply(year3[, 5:6], 2, mean, na.rm = TRUE)

# Bias, PBias, R2, KGE, RSR, NSE, MAE for each year
yr_df <- data.frame(
  Variable = names(mean_yr1),  
  yr1 = mean_yr1,
  yr2 = mean_yr2,
  yr3 = mean_yr3
)

yr_df_correct <- t(yr_df)
yr_df_EW2 <- as.data.frame(yr_df_correct)
yr_df_EW2 <- yr_df_EW2[-1, ]
yr_df_EW2 <- as.data.frame(lapply(yr_df_EW2, as.numeric))

# Stats 
kge_valueEW2 <- KGE(sim = yr_df_EW2$mean_ET, obs = yr_df_EW2$field_et)
rmse_valueEW2 <- rmse(actual = yr_df_EW2$field_et, predicted = yr_df_EW2$mean_ET)
nse_valueEW2 <- NSE(sim = yr_df_EW2$mean_ET, obs = yr_df_EW2$field_et)
rsr_valueEW2 <- rmse_valueEW2 / sd(yr_df_EW2$field_et)

biasEW2 <- mean(yr_df_EW2$mean_ET - yr_df_EW2$field_et)
pbiasEW2 <- 100 * sum(yr_df_EW2$mean_ET - yr_df_EW2$field_et) / sum(yr_df_EW2$field_et)
maeEW2 <- mean(abs(yr_df_EW2$mean_ET - yr_df_EW2$field_et))

spearman_corrEW2 <- cor.test(yr_df_EW2$field_et, yr_df_EW2$mean_ET, method = "spearman")
print(spearman_corrEW2)
correlation_coefficientEW2 <- spearman_corrEW2$estimate
p_valueEW2 <- spearman_corrEW2$p.value
################################################################################

# EZU
# Split into hydrological years (if the timeseries spans over the course of two or more years)
year1 <- EZU[3:14, ]
year2 <- EZU[15:26, ] 

# Mean ET for each of the yrs
mean_yr1 <- apply(year1[, 5:6], 2, mean, na.rm = TRUE)
mean_yr2 <- apply(year2[, 5:6], 2, mean, na.rm = TRUE)

# Bias, PBias, R2, KGE, RSR, NSE, MAE for each year
yr_df <- data.frame(
  Variable = names(mean_yr1),  
  yr1 = mean_yr1,
  yr2 = mean_yr2
  
)

yr_df_correct <- t(yr_df)
yr_df_EZU <- as.data.frame(yr_df_correct)
yr_df_EZU <- yr_df_EZU[-1, ]
yr_df_EZU <- as.data.frame(lapply(yr_df_EZU, as.numeric))

# Stats 
kge_valueEZU <- KGE(sim = yr_df_EZU$mean_ET, obs = yr_df_EZU$field_et)
rmse_valueEZU <- rmse(actual = yr_df_EZU$field_et, predicted = yr_df_EZU$mean_ET)
nse_valueEZU <- NSE(sim = yr_df_EZU$mean_ET, obs = yr_df_EZU$field_et)
rsr_valueEZU <- rmse_valueEZU / sd(yr_df_EZU$field_et)

biasEZU <- mean(yr_df_EZU$mean_ET - yr_df_EZU$field_et)
pbiasEZU <- 100 * sum(yr_df_EZU$mean_ET - yr_df_EZU$field_et) / sum(yr_df_EZU$field_et)
maeEZU <- mean(abs(yr_df_EZU$mean_ET - yr_df_EZU$field_et))

spearman_corrEZU <- cor.test(yr_df_EZU$field_et, yr_df_EZU$mean_ET, method = "spearman")
print(spearman_corrEZU)
correlation_coefficientEZU <- spearman_corrEZU$estimate
p_valueEZU <- spearman_corrEZU$p.value
################################################################################

# JHK
# Split into hydrological years (if the timeseries spans over the course of two or more years)
year1 <- JHK[1:12, ]
year2 <- JHK[13:24, ] 
year3 <- JHK[37:46, ] 

# Mean ET for each of the yrs
mean_yr1 <- apply(year1[, 5:6], 2, mean, na.rm = TRUE)
mean_yr2 <- apply(year2[, 5:6], 2, mean, na.rm = TRUE)
mean_yr3 <- apply(year3[, 5:6], 2, mean, na.rm = TRUE)

# Bias, PBias, R2, KGE, RSR, NSE, MAE for each year
yr_df <- data.frame(
  Variable = names(mean_yr1),  
  yr1 = mean_yr1,
  yr2 = mean_yr2,
  yr3 = mean_yr3
)

yr_df_correct <- t(yr_df)
yr_df_JHK <- as.data.frame(yr_df_correct)
yr_df_JHK <- yr_df_JHK[-1, ]
yr_df_JHK <- as.data.frame(lapply(yr_df_JHK, as.numeric))

# Stats 
kge_valueJHK <- KGE(sim = yr_df_JHK$mean_ET, obs = yr_df_JHK$field_et)
rmse_valueJHK <- rmse(actual = yr_df_JHK$field_et, predicted = yr_df_JHK$mean_ET)
nse_valueJHK <- NSE(sim = yr_df_JHK$mean_ET, obs = yr_df_JHK$field_et)
rsr_valueJHK <- rmse_valueJHK / sd(yr_df_JHK$field_et)

biasJHK <- mean(yr_df_JHK$mean_ET - yr_df_JHK$field_et)
pbiasJHK <- 100 * sum(yr_df_JHK$mean_ET - yr_df_JHK$field_et) / sum(yr_df_JHK$field_et)
maeJHK <- mean(abs(yr_df_JHK$mean_ET - yr_df_JHK$field_et))

spearman_corrJHK <- cor.test(yr_df_JHK$field_et, yr_df_JHK$mean_ET, method = "spearman")
print(spearman_corrJHK)
correlation_coefficientJHK <- spearman_corrJHK$estimate
p_valueJHK <- spearman_corrJHK$p.value

################################################################################
# MB1
# Split into hydrological years (if the timeseries spans over the course of two or more years)
year1 <- MB1[3:14, ]
year2 <- MB1[15:26, ] 
year3 <- MB1[27:38, ] 
year4 <- MB1[39:50, ]
year5 <- MB1[51:62, ]
year6 <- MB1[63:74, ]

# Mean ET for each of the yrs
mean_yr1 <- apply(year1[, 5:6], 2, mean, na.rm = TRUE)
mean_yr2 <- apply(year2[, 5:6], 2, mean, na.rm = TRUE)
mean_yr3 <- apply(year3[, 5:6], 2, mean, na.rm = TRUE)
mean_yr4 <- apply(year4[, 5:6], 2, mean, na.rm = TRUE)
mean_yr5 <- apply(year5[, 5:6], 2, mean, na.rm = TRUE)
mean_yr6 <- apply(year6[, 5:6], 2, mean, na.rm = TRUE)

# Bias, PBias, R2, KGE, RSR, NSE, MAE for each year
yr_df <- data.frame(
  Variable = names(mean_yr1),  
  yr1 = mean_yr1,
  yr2 = mean_yr2,
  yr3 = mean_yr3,
  yr4 = mean_yr4,
  yr5 = mean_yr5,
  yr6 = mean_yr6
)

yr_df_correct <- t(yr_df)
yr_df_MB1 <- as.data.frame(yr_df_correct)
yr_df_MB1 <- yr_df_MB1[-1, ]
yr_df_MB1 <- as.data.frame(lapply(yr_df_MB1, as.numeric))

# Stats 
kge_valueMB1 <- KGE(sim = yr_df_MB1$mean_ET, obs = yr_df_MB1$field_et)
rmse_valueMB1 <- rmse(actual = yr_df_MB1$field_et, predicted = yr_df_MB1$mean_ET)
nse_valueMB1 <- NSE(sim = yr_df_MB1$mean_ET, obs = yr_df_MB1$field_et)
rsr_valueMB1 <- rmse_valueMB1 / sd(yr_df_MB1$field_et)

biasMB1 <- mean(yr_df_MB1$mean_ET - yr_df_MB1$field_et)
pbiasMB1 <- 100 * sum(yr_df_MB1$mean_ET - yr_df_MB1$field_et) / sum(yr_df_MB1$field_et)
maeMB1 <- mean(abs(yr_df_MB1$mean_ET - yr_df_MB1$field_et))

spearman_corrMB1 <- cor.test(yr_df_MB1$field_et, yr_df_MB1$mean_ET, method = "spearman")
print(spearman_corrMB1)
correlation_coefficientMB1 <- spearman_corrMB1$estimate
p_valueMB1 <- spearman_corrMB1$p.value
#################################################################################

# MB2
# Split into hydrological years (if the timeseries spans over the course of two or more years)
year1 <- MB2[3:14, ]
year2 <- MB2[15:26, ] 
year3 <- MB2[27:38, ] 
year4 <- MB2[39:50, ]
year5 <- MB2[51:62, ]
year6 <- MB2[63:74, ]

# Mean ET for each of the yrs
mean_yr1 <- apply(year1[, 5:6], 2, mean, na.rm = TRUE)
mean_yr2 <- apply(year2[, 5:6], 2, mean, na.rm = TRUE)
mean_yr3 <- apply(year3[, 5:6], 2, mean, na.rm = TRUE)
mean_yr4 <- apply(year4[, 5:6], 2, mean, na.rm = TRUE)
mean_yr5 <- apply(year5[, 5:6], 2, mean, na.rm = TRUE)
mean_yr6 <- apply(year6[, 5:6], 2, mean, na.rm = TRUE)

# Bias, PBias, R2, KGE, RSR, NSE, MAE for each year
yr_df <- data.frame(
  Variable = names(mean_yr1),  
  yr1 = mean_yr1,
  yr2 = mean_yr2,
  yr3 = mean_yr3,
  yr4 = mean_yr4,
  yr5 = mean_yr5,
  yr6 = mean_yr6
)

yr_df_correct <- t(yr_df)
yr_df_MB2 <- as.data.frame(yr_df_correct)
yr_df_MB2 <- yr_df_MB2[-1, ]
yr_df_MB2 <- as.data.frame(lapply(yr_df_MB2, as.numeric))

# Stats 
kge_valueMB2 <- KGE(sim = yr_df_MB2$mean_ET, obs = yr_df_MB2$field_et)
rmse_valueMB2 <- rmse(actual = yr_df_MB2$field_et, predicted = yr_df_MB2$mean_ET)
nse_valueMB2 <- NSE(sim = yr_df_MB2$mean_ET, obs = yr_df_MB2$field_et)
rsr_valueMB2 <- rmse_valueMB2 / sd(yr_df_MB2$field_et)

biasMB2 <- mean(yr_df_MB2$mean_ET - yr_df_MB2$field_et)
pbiasMB2 <- 100 * sum(yr_df_MB2$mean_ET - yr_df_MB2$field_et) / sum(yr_df_MB2$field_et)
maeMB2 <- mean(abs(yr_df_MB2$mean_ET - yr_df_MB2$field_et))

spearman_corrMB2 <- cor.test(yr_df_MB2$field_et, yr_df_MB2$mean_ET, method = "spearman")
print(spearman_corrMB2)
correlation_coefficientMB2 <- spearman_corrMB2$estimate
p_valueMB2 <- spearman_corrMB2$p.value
################################################################################

# MCP
# No complete years for interannual analyses

################################################################################

# MLP
# No complete years for interannual analyses

################################################################################

# SKU
# Split into hydrological years (if the timeseries spans over the course of two or more years)
year1 <- SKU[19:30, ]
year2 <- SKU[31:42, ] 

# Mean ET for each of the yrs
mean_yr1 <- apply(year1[, 5:6], 2, mean, na.rm = TRUE)
mean_yr2 <- apply(year2[, 5:6], 2, mean, na.rm = TRUE)

# Bias, PBias, R2, KGE, RSR, NSE, MAE for each year
yr_df <- data.frame(
  Variable = names(mean_yr1),  
  yr1 = mean_yr1,
  yr2 = mean_yr2
  
)

yr_df_correct <- t(yr_df)
yr_df_SKU <- as.data.frame(yr_df_correct)
yr_df_SKU <- yr_df_SKU[-1, ]
yr_df_SKU <- as.data.frame(lapply(yr_df_SKU, as.numeric))

# Stats 
kge_valueSKU <- KGE(sim = yr_df_SKU$mean_ET, obs = yr_df_SKU$field_et)
rmse_valueSKU <- rmse(actual = yr_df_SKU$field_et, predicted = yr_df_SKU$mean_ET)
nse_valueSKU <- NSE(sim = yr_df_SKU$mean_ET, obs = yr_df_SKU$field_et)
rsr_valueSKU <- rmse_valueSKU / sd(yr_df_SKU$field_et)

biasSKU <- mean(yr_df_SKU$mean_ET - yr_df_SKU$field_et)
pbiasSKU <- 100 * sum(yr_df_SKU$mean_ET - yr_df_SKU$field_et) / sum(yr_df_SKU$field_et)
maeSKU <- mean(abs(yr_df_SKU$mean_ET - yr_df_SKU$field_et))

spearman_corrSKU <- cor.test(yr_df_SKU$field_et, yr_df_SKU$mean_ET, method = "spearman")
print(spearman_corrSKU)
correlation_coefficientSKU <- spearman_corrSKU$estimate
p_valueSKU <- spearman_corrSKU$p.value
################################################################################

# Coefficient of determination (R2):
# BF1
fit1 <- lm(yr_df_BF1$mean_ET ~ yr_df_BF1$field_et)
abline(fit1, col="red")

# CP3
fit3 <- lm(yr_df_CP3$mean_ET ~ yr_df_CP3$field_et)
abline(fit3, col="red")


# CP6
fit4 <- lm(yr_df_CP6$mean_ET ~ yr_df_CP6$field_et)
abline(fit4, col="red")


# CP9
fit5 <- lm(yr_df_CP9$mean_ET ~ yr_df_CP9$field_et)
abline(fit5, col="red")

# EW1
fit6 <- lm(yr_df_EW1$mean_ET ~ yr_df_EW1$field_et)
abline(fit6, col="red")


# EW2
fit7 <- lm(yr_df_EW2$mean_ET ~ yr_df_EW2$field_et)
abline(fit7, col="red")


# EZU
fit8 <- lm(yr_df_EZU$mean_ET ~ yr_df_EZU$field_et)
abline(fit8, col="red")

# JHK
fit9 <- lm(yr_df_JHK$mean_ET ~ yr_df_JHK$field_et)
abline(fit9, col="red")

# MB1
fit10 <- lm(yr_df_MB1$mean_ET ~ yr_df_MB1$field_et)
abline(fit10, col="red")


# MB2
fit11 <- lm(yr_df_MB2$mean_ET ~ yr_df_MB2$field_et)
abline(fit11, col="red")

# SKU
fit14 <- lm(yr_df_SKU$mean_ET ~ yr_df_SKU$field_et)
abline(fit14, col="red")

#########################

R2_BF1 <- summary(fit1)

R2_CP3 <- summary(fit3)
R2_CP6 <- summary(fit4)
R2_CP9 <- summary(fit5)
R2_EW1 <- summary(fit6)
R2_EW2 <- summary(fit7)
R2_EZU <- summary(fit8)
R2_JHK <- summary(fit9)
R2_MB1 <- summary(fit10)
R2_MB2 <- summary(fit11)

R2_SKU <- summary(fit14)

r_squared_BF1 <- R2_BF1$r.squared

r_squared_CP3 <- R2_CP3$r.squared
r_squared_CP6 <- R2_CP6$r.squared
r_squared_CP9 <- R2_CP9$r.squared
r_squared_EW1 <- R2_EW1$r.squared
r_squared_EW2 <- R2_EW2$r.squared
r_squared_EZU <- R2_EZU$r.squared
r_squared_JHK <- R2_JHK$r.squared
r_squared_MB1 <- R2_MB1$r.squared
r_squared_MB2 <- R2_MB2$r.squared

r_squared_SKU <- R2_SKU$r.squared


df_stats <- data.frame(rsr_valueBF1, rsr_valueCP3, rsr_valueCP6, rsr_valueCP9, rsr_valueEW1, rsr_valueEW2,
                       rsr_valueEZU, rsr_valueJHK, rsr_valueMB1, rsr_valueMB2, rsr_valueSKU,
                       rmse_valueBF1, rmse_valueCP3, rmse_valueCP6, rmse_valueCP9, rmse_valueEW1, rmse_valueEW2,
                       rmse_valueEZU, rmse_valueJHK, rmse_valueMB1, rmse_valueMB2, rmse_valueSKU,
                       pbiasBF1, pbiasCP3, pbiasCP6, pbiasCP9, pbiasEW1, pbiasEW2,
                       pbiasEZU, pbiasJHK, pbiasMB1, pbiasMB2, pbiasSKU,
                       biasBF1, biasCP3, biasCP6, biasCP9, biasEW1, biasEW2,
                       biasEZU, biasJHK, biasMB1, biasMB2, biasSKU,
                       maeBF1, maeCP3, maeCP6, maeCP9, maeEW1, maeEW2,
                       maeEZU, maeJHK, maeMB1, maeMB2, maeSKU,
                       nse_valueBF1, nse_valueCP3, nse_valueCP6, nse_valueCP9, nse_valueEW1, nse_valueEW2,
                       nse_valueEZU, nse_valueJHK, nse_valueMB1, nse_valueMB2, nse_valueSKU,
                       kge_valueBF1, kge_valueCP3, kge_valueCP6, kge_valueCP9, kge_valueEW1, kge_valueEW2,
                       kge_valueEZU, kge_valueJHK, kge_valueMB1, kge_valueMB2, kge_valueSKU,
                       p_valueBF1, p_valueCP3, p_valueCP6, p_valueCP9, p_valueEW1, p_valueEW2,
                       p_valueEZU, p_valueJHK, p_valueMB1, p_valueMB2, p_valueSKU,
                       correlation_coefficientBF1, correlation_coefficientCP3, correlation_coefficientCP6, correlation_coefficientCP9, correlation_coefficientEW1, correlation_coefficientEW2,
                       correlation_coefficientEZU, correlation_coefficientJHK, correlation_coefficientMB1, correlation_coefficientMB2, correlation_coefficientSKU,
                       r_squared_BF1, r_squared_CP3, r_squared_CP6, r_squared_CP9, r_squared_EW1, r_squared_EW2,
                       r_squared_EZU, r_squared_JHK, r_squared_MB1, r_squared_MB2, r_squared_SKU
)

# Reshape the data frame from wide to long format
df_long_Interannual <- df_stats %>%
  pivot_longer(
    cols = everything(),
    names_to = c("metric", "site"),  # Separate columns for metric names and sites
    names_pattern = "^(rsr_value|rmse_value|pbias|bias|mae|nse_value|kge_value|p_value|correlation_coefficient|r_squared_)(.*)" # Specify explicit metric names
  ) %>%
  pivot_wider(
    names_from = metric,             # Separate each metric into its own column
    values_from = value
  )

# View the reshaped data
head(df_long_Interannual)

setwd("C:\\Users\\Liam\\OneDrive - Stellenbosch University\\ET_Data\\Stats")
write.csv(df_long_Interannual, file = "Ensemble_InterAnnual_Stats.csv", row.names = FALSE)