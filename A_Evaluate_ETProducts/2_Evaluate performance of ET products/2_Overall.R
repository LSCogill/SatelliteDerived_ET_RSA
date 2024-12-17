# Overall: for each of the 14 sites

# Indv_Seasonal
library(xts)
library(zoo)
library(tidyverse)
library(plotly)
library(lubridate)
library(ggplot2)
library(hydroGOF) 

# Load the dataset
setwd("C:\\Users\\Liam\\Desktop\\Seasonal_Indv")

BF1 <- read.csv("monthly_means_BF1.csv", skip=0, sep = ",")
BF2 <- read.csv("monthly_means_BF2.csv", skip=0, sep = ",")
CP3 <- read.csv("monthly_means_CP3.csv", skip=0, sep = ",")
CP6 <- read.csv("monthly_means_CP6.csv", skip=0, sep = ",")
CP9 <- read.csv("monthly_means_CP9.csv", skip=0, sep = ",")
EW1 <- read.csv("monthly_means_EW1.csv", skip=0, sep = ",")
EW2 <- read.csv("monthly_means_EW2.csv", skip=0, sep = ",")
EZU <- read.csv("monthly_means_EZU.csv", skip=0, sep = ",")
JHK <- read.csv("monthly_means_JHK.csv", skip=0, sep = ",")
MB1 <- read.csv("monthly_means_MB1.csv", skip=0, sep = ",")
MB2 <- read.csv("monthly_means_MB2.csv", skip=0, sep = ",")
MCP <- read.csv("monthly_means_MCP.csv", skip=0, sep = ",")
MLP <- read.csv("monthly_means_MLP.csv", skip=0, sep = ",")
SKU <- read.csv("monthly_means_SKU.csv", skip=0, sep = ",")

# Test for normality
BF1_shapiro <- c(BF1$Field_ET_monthly, BF1$TC_monthly, BF1$SMAP_monthly, BF1$SSEBop_monthly, BF1$WaPOR_monthly, BF1$GLDAS_monthly,BF1$MOD16_monthly, BF1$FLDAS_monthly, BF1$PTjpl_monthly)
shapiro <- shapiro.test(BF1_shapiro)
shapiro
BF2_shapiro <- c(BF2$Field_ET_monthly, BF2$TC_monthly, BF2$SMAP_monthly, BF2$SSEBop_monthly, BF2$WaPOR_monthly, BF2$GLDAS_monthly,BF2$MOD16_monthly, BF2$FLDAS_monthly, BF2$PTjpl_monthly)
shapiro <- shapiro.test(BF2_shapiro)
shapiro
CP3_shapiro <- c(CP3$Field_ET_monthly, CP3$TC_monthly, CP3$SMAP_monthly, CP3$SSEBop_monthly, CP3$WaPOR_monthly, CP3$GLDAS_monthly,CP3$MOD16_monthly, CP3$FLDAS_monthly, CP3$PTjpl_monthly)
shapiro <- shapiro.test(CP3_shapiro)
shapiro
CP6_shapiro <- c(CP6$Field_ET_monthly, CP6$TC_monthly, CP6$SMAP_monthly, CP6$SSEBop_monthly, CP6$WaPOR_monthly, CP6$GLDAS_monthly,CP6$MOD16_monthly, CP6$FLDAS_monthly, CP6$PTjpl_monthly)
shapiro <- shapiro.test(CP6_shapiro)
shapiro
CP9_shapiro <- c(CP9$Field_ET_monthly, CP9$TC_monthly, CP9$SMAP_monthly, CP9$SSEBop_monthly, CP9$WaPOR_monthly, CP9$GLDAS_monthly,CP9$MOD16_monthly, CP9$FLDAS_monthly, CP9$PTjpl_monthly)
shapiro <- shapiro.test(CP9_shapiro)
shapiro
EW1_shapiro <- c(EW1$Field_ET_monthly, EW1$TC_monthly, EW1$SMAP_monthly, EW1$SSEBop_monthly, EW1$WaPOR_monthly, EW1$GLDAS_monthly,EW1$MOD16_monthly, EW1$FLDAS_monthly, EW1$PTjpl_monthly)
shapiro <- shapiro.test(EW1_shapiro)
shapiro
EW2_shapiro <- c(EW2$Field_ET_monthly, EW2$TC_monthly, EW2$SMAP_monthly, EW2$SSEBop_monthly, EW2$WaPOR_monthly, EW2$GLDAS_monthly,EW2$MOD16_monthly, EW2$FLDAS_monthly, EW2$PTjpl_monthly)
shapiro <- shapiro.test(EW2_shapiro)
shapiro
EZU_shapiro <- c(EZU$Field_ET_monthly, EZU$TC_monthly, EZU$SMAP_monthly, EZU$SSEBop_monthly, EZU$WaPOR_monthly, EZU$GLDAS_monthly,EZU$MOD16_monthly, EZU$FLDAS_monthly, EZU$PTjpl_monthly)
shapiro <- shapiro.test(EZU_shapiro)
shapiro
JHK_shapiro <- c(JHK$Field_ET_monthly, JHK$TC_monthly, JHK$SMAP_monthly, JHK$SSEBop_monthly, JHK$WaPOR_monthly, JHK$GLDAS_monthly,JHK$MOD16_monthly, JHK$FLDAS_monthly, JHK$PTjpl_monthly)
shapiro <- shapiro.test(JHK_shapiro)
shapiro
MB1_shapiro <- c(MB1$Field_ET_monthly, MB1$TC_monthly, MB1$SMAP_monthly, MB1$SSEBop_monthly, MB1$WaPOR_monthly, MB1$GLDAS_monthly,MB1$MOD16_monthly, MB1$FLDAS_monthly, MB1$PTjpl_monthly)
shapiro <- shapiro.test(MB1_shapiro)
shapiro
MB2_shapiro <- c(MB2$Field_ET_monthly, MB2$TC_monthly, MB2$SMAP_monthly, MB2$SSEBop_monthly, MB2$WaPOR_monthly, MB2$GLDAS_monthly,MB2$MOD16_monthly, MB2$FLDAS_monthly, MB2$PTjpl_monthly)
shapiro <- shapiro.test(MB2_shapiro)
shapiro
MCP_shapiro <- c(MCP$Field_ET_monthly, MCP$TC_monthly, MCP$SMAP_monthly, MCP$SSEBop_monthly, MCP$WaPOR_monthly, MCP$GLDAS_monthly,MCP$MOD16_monthly, MCP$FLDAS_monthly, MCP$PTjpl_monthly)
shapiro <- shapiro.test(MCP_shapiro)
shapiro
MLP_shapiro <- c(MLP$Field_ET_monthly, MLP$TC_monthly, MLP$SMAP_monthly, MLP$SSEBop_monthly, MLP$WaPOR_monthly, MLP$GLDAS_monthly,MLP$MOD16_monthly, MLP$FLDAS_monthly, MLP$PTjpl_monthly)
shapiro <- shapiro.test(MLP_shapiro)
shapiro
SKU_shapiro <- c(SKU$Field_ET_monthly, SKU$TC_monthly, SKU$SMAP_monthly, SKU$SSEBop_monthly, SKU$WaPOR_monthly, SKU$GLDAS_monthly,SKU$MOD16_monthly, SKU$FLDAS_monthly, SKU$PTjpl_monthly)
shapiro <- shapiro.test(SKU_shapiro)
shapiro

Shapiro_All <- c(SKU_shapiro, MLP_shapiro, MCP_shapiro, MB2_shapiro, MB1_shapiro, CP3_shapiro, CP6_shapiro, CP9_shapiro, BF1_shapiro, BF2_shapiro, EW1_shapiro, EW2_shapiro, EZU_shapiro, JHK_shapiro)
shapiro <- shapiro.test(Shapiro_All)
shapiro
###################################################################################################
####################################################################################################
# BF1
# TC 
kge_valueTC <- KGE(sim = BF1$TC_monthly, obs = BF1$Field_ET_monthly)
rmse_valueTC <- rmse(actual = BF1$Field_ET_monthly, predicted = BF1$TC_monthly)
nse_valueTC <- NSE(sim = BF1$TC_monthly, obs = BF1$Field_ET_monthly)
rsr_valueTC <- rmse_valueTC / sd(BF1$Field_ET_monthly)

biasTC <- mean(BF1$TC_monthly - BF1$Field_ET_monthly)
pbiasTC <- 100 * sum(BF1$TC_monthly - BF1$Field_ET_monthly) / sum(BF1$Field_ET_monthly)
maeTC <- mean(abs(BF1$TC_monthly - BF1$Field_ET_monthly))

spearman_corrTC <- cor.test(BF1$Field_ET_monthly, BF1$TC_monthly, method = "spearman")
print(spearman_corrTC)
correlation_coefficientTC <- spearman_corrTC$estimate

# SMAP
kge_valueSMAP <- KGE(sim = BF1$SMAP_monthly, obs = BF1$Field_ET_monthly)
rmse_valueSMAP <- rmse(actual = BF1$Field_ET_monthly, predicted = BF1$SMAP_monthly)
nse_valueSMAP <- NSE(sim = BF1$SMAP_monthly, obs = BF1$Field_ET_monthly)
rsr_valueSMAP <- rmse_valueSMAP / sd(BF1$Field_ET_monthly)

biasSMAP <- mean(BF1$SMAP_monthly - BF1$Field_ET_monthly)
pbiasSMAP <- 100 * sum(BF1$SMAP_monthly - BF1$Field_ET_monthly) / sum(BF1$Field_ET_monthly)
maeSMAP <- mean(abs(BF1$SMAP_monthly - BF1$Field_ET_monthly))

spearman_corrSMAP <- cor.test(BF1$Field_ET_monthly, BF1$SMAP_monthly, method = "spearman")
print(spearman_corrSMAP)
correlation_coefficientSMAP <- spearman_corrSMAP$estimate

# SSEBop
kge_valueSSEBop <- KGE(sim = BF1$SSEBop_monthly, obs = BF1$Field_ET_monthly)
rmse_valueSSEBop <- rmse(actual = BF1$Field_ET_monthly[!is.na(BF1$Field_ET_monthly) & !is.na(BF1$SSEBop_monthly)], 
                         predicted = BF1$SSEBop_monthly[!is.na(BF1$Field_ET_monthly) & !is.na(BF1$SSEBop_monthly)])
nse_valueSSEBop <- NSE(sim = BF1$SSEBop_monthly, obs = BF1$Field_ET_monthly)
rsr_valueSSEBop <- rmse_valueSSEBop / sd(BF1$Field_ET_monthly, na.rm = TRUE)

biasSSEBop <- mean(BF1$SSEBop_monthly - BF1$Field_ET_monthly, na.rm = TRUE)
pbiasSSEBop <- 100 * sum(BF1$SSEBop_monthly - BF1$Field_ET_monthly, na.rm = TRUE) / sum(BF1$Field_ET_monthly, na.rm = TRUE)
maeSSEBop <- mean(abs(BF1$SSEBop_monthly - BF1$Field_ET_monthly), na.rm = TRUE)

spearman_corrSSEBop <- cor.test(BF1$Field_ET_monthly, BF1$SSEBop_monthly, method = "spearman")
print(spearman_corrSSEBop)
correlation_coefficientSSEBop <- spearman_corrSSEBop$estimate

# WAPOR
kge_valueWaPOR <- KGE(sim = BF1$WaPOR_monthly, obs = BF1$Field_ET_monthly)
rmse_valueWaPOR <- rmse(actual = BF1$Field_ET_monthly[!is.na(BF1$Field_ET_monthly) & !is.na(BF1$WaPOR_monthly)],
                        predicted = BF1$WaPOR_monthly[!is.na(BF1$Field_ET_monthly) & !is.na(BF1$WaPOR_monthly)])
nse_valueWaPOR <- NSE(sim = BF1$WaPOR_monthly, obs = BF1$Field_ET_monthly)
rsr_valueWaPOR <- rmse_valueWaPOR / sd(BF1$Field_ET_monthly, na.rm = TRUE)

biasWaPOR <- mean(BF1$WaPOR_monthly - BF1$Field_ET_monthly, na.rm = TRUE)
pbiasWaPOR <- 100 * sum(BF1$WaPOR_monthly - BF1$Field_ET_monthly, na.rm = TRUE) / sum(BF1$Field_ET_monthly, na.rm = TRUE)
maeWaPOR <- mean(abs(BF1$WaPOR_monthly - BF1$Field_ET_monthly), na.rm = TRUE)

spearman_corrWaPOR <- cor.test(BF1$Field_ET_monthly, BF1$WaPOR_monthly, method = "spearman")
print(spearman_corrWaPOR)
correlation_coefficientWaPOR <- spearman_corrWaPOR$estimate

# MOD16
kge_valueMOD16 <- KGE(sim = BF1$MOD16_monthly, obs = BF1$Field_ET_monthly)
rmse_valueMOD16 <- rmse(actual = BF1$Field_ET_monthly, predicted = BF1$MOD16_monthly)
nse_valueMOD16 <- NSE(sim = BF1$MOD16_monthly, obs = BF1$Field_ET_monthly)
rsr_valueMOD16 <- rmse_valueMOD16 / sd(BF1$Field_ET_monthly)

biasMOD16 <- mean(BF1$MOD16_monthly - BF1$Field_ET_monthly)
pbiasMOD16 <- 100 * sum(BF1$MOD16_monthly - BF1$Field_ET_monthly) / sum(BF1$Field_ET_monthly)
maeMOD16 <- mean(abs(BF1$MOD16_monthly - BF1$Field_ET_monthly))

spearman_corrMOD16 <- cor.test(BF1$Field_ET_monthly, BF1$MOD16_monthly, method = "spearman")
print(spearman_corrMOD16)
correlation_coefficientMOD16 <- spearman_corrMOD16$estimate

# GLDAS
kge_valueGLDAS <- KGE(sim = BF1$GLDAS_monthly, obs = BF1$Field_ET_monthly)
rmse_valueGLDAS <- rmse(actual = BF1$Field_ET_monthly, predicted = BF1$GLDAS_monthly)
nse_valueGLDAS <- NSE(sim = BF1$GLDAS_monthly, obs = BF1$Field_ET_monthly)
rsr_valueGLDAS <- rmse_valueGLDAS / sd(BF1$Field_ET_monthly)

biasGLDAS <- mean(BF1$GLDAS_monthly - BF1$Field_ET_monthly)
pbiasGLDAS <- 100 * sum(BF1$GLDAS_monthly - BF1$Field_ET_monthly) / sum(BF1$Field_ET_monthly)
maeGLDAS <- mean(abs(BF1$GLDAS_monthly - BF1$Field_ET_monthly))

spearman_corrGLDAS <- cor.test(BF1$Field_ET_monthly, BF1$GLDAS_monthly, method = "spearman")
print(spearman_corrGLDAS)
correlation_coefficientGLDAS <- spearman_corrGLDAS$estimate

# FLDAS
kge_valueFLDAS <- KGE(sim = BF1$FLDAS_monthly, obs = BF1$Field_ET_monthly)
rmse_valueFLDAS <- rmse(actual = BF1$Field_ET_monthly, predicted = BF1$FLDAS_monthly)
nse_valueFLDAS <- NSE(sim = BF1$FLDAS_monthly, obs = BF1$Field_ET_monthly)
rsr_valueFLDAS <- rmse_valueFLDAS / sd(BF1$Field_ET_monthly)

biasFLDAS <- mean(BF1$FLDAS_monthly - BF1$Field_ET_monthly)
pbiasFLDAS <- 100 * sum(BF1$FLDAS_monthly - BF1$Field_ET_monthly) / sum(BF1$Field_ET_monthly)
maeFLDAS <- mean(abs(BF1$FLDAS_monthly - BF1$Field_ET_monthly))

spearman_corrFLDAS <- cor.test(BF1$Field_ET_monthly, BF1$FLDAS_monthly, method = "spearman")
print(spearman_corrFLDAS)
correlation_coefficientFLDAS <- spearman_corrFLDAS$estimate

# PTJPL
kge_valuePTjpl <- KGE(sim = BF1$PTjpl_monthly, obs = BF1$Field_ET_monthly)
rmse_valuePTjpl <- rmse(actual = BF1$Field_ET_monthly[!is.na(BF1$Field_ET_monthly) & !is.na(BF1$PTjpl_monthly)], 
                        predicted = BF1$PTjpl_monthly[!is.na(BF1$Field_ET_monthly) & !is.na(BF1$PTjpl_monthly)])
nse_valuePTjpl <- NSE(sim = BF1$PTjpl_monthly, obs = BF1$Field_ET_monthly)
rsr_valuePTjpl <- rmse_valuePTjpl / sd(BF1$Field_ET_monthly, na.rm = TRUE)

biasPTjpl <- mean(BF1$PTjpl_monthly - BF1$Field_ET_monthly, na.rm = TRUE)
pbiasPTjpl <- 100 * sum(BF1$PTjpl_monthly - BF1$Field_ET_monthly,na.rm = TRUE) / sum(BF1$Field_ET_monthly, na.rm = TRUE)
maePTjpl <- mean(abs(BF1$PTjpl_monthly - BF1$Field_ET_monthly), na.rm = TRUE)

spearman_corrPTjpl <- cor.test(BF1$Field_ET_monthly, BF1$PTjpl_monthly, method = "spearman")
print(spearman_corrPTjpl)
correlation_coefficientPTjpl <- spearman_corrPTjpl$estimate

df_stats <- data.frame(rsr_valueTC, rsr_valueWaPOR, rsr_valueSMAP, rsr_valueFLDAS, rsr_valueGLDAS, rsr_valueMOD16, rsr_valueSSEBop, rsr_valuePTjpl,
                       rmse_valueTC, rmse_valueWaPOR, rmse_valueSMAP, rmse_valueFLDAS, rmse_valueGLDAS, rmse_valueMOD16, rmse_valueSSEBop, rmse_valuePTjpl,
                       pbiasTC, pbiasWaPOR, pbiasSMAP, pbiasFLDAS, pbiasGLDAS, pbiasMOD16, pbiasSSEBop, pbiasPTjpl,
                       biasTC, biasWaPOR, biasSMAP, biasFLDAS, biasGLDAS, biasMOD16, biasSSEBop, biasPTjpl,
                       maeTC, maeWaPOR, maeSMAP, maeFLDAS, maeGLDAS, maeMOD16, maeSSEBop, maePTjpl,
                       nse_valueTC, nse_valueWaPOR, nse_valueSMAP, nse_valueFLDAS, nse_valueGLDAS, nse_valueMOD16, nse_valueSSEBop, nse_valuePTjpl,
                       kge_valueTC, kge_valueWaPOR, kge_valueSMAP, kge_valueFLDAS, kge_valueGLDAS, kge_valueMOD16, kge_valueSSEBop, kge_valuePTjpl,
                       correlation_coefficientTC, correlation_coefficientWaPOR, correlation_coefficientSMAP, correlation_coefficientFLDAS, correlation_coefficientGLDAS, correlation_coefficientMOD16, correlation_coefficientSSEBop, correlation_coefficientPTjpl)

# Reshape the data frame from wide to long format
df_long <- df_stats %>%
  pivot_longer(
    cols = everything(),
    names_to = c("metric", "site"),  # Separate columns for metric names and sites
    names_pattern = "^(pbias|nse_value|kge_value|correlation_coefficient)(.*)" # Specify explicit metric names
  ) %>%
  pivot_wider(
    names_from = metric,             # Separate each metric into its own column
    values_from = value
  )

# View the reshaped data
head(df_long)
setwd("C:\\Users\\Liam\\OneDrive - Stellenbosch University\\ET_Data\\Stats")
write.csv(df_stats, file = "Indv_Seasonal_BF1.csv", row.names = FALSE)

###################################################################################################
####################################################################################################
# BF2
# TC 
kge_valueTC <- KGE(sim = BF2$TC_monthly, obs = BF2$Field_ET_monthly)
rmse_valueTC <- rmse(actual = BF2$Field_ET_monthly, predicted = BF2$TC_monthly)
nse_valueTC <- NSE(sim = BF2$TC_monthly, obs = BF2$Field_ET_monthly)
rsr_valueTC <- rmse_valueTC / sd(BF2$Field_ET_monthly)

biasTC <- mean(BF2$TC_monthly - BF2$Field_ET_monthly)
pbiasTC <- 100 * sum(BF2$TC_monthly - BF2$Field_ET_monthly) / sum(BF2$Field_ET_monthly)
maeTC <- mean(abs(BF2$TC_monthly - BF2$Field_ET_monthly))

spearman_corrTC <- cor.test(BF2$Field_ET_monthly, BF2$TC_monthly, method = "spearman")
print(spearman_corrTC)
correlation_coefficientTC <- spearman_corrTC$estimate

# SMAP
kge_valueSMAP <- KGE(sim = BF2$SMAP_monthly, obs = BF2$Field_ET_monthly)
rmse_valueSMAP <- rmse(actual = BF2$Field_ET_monthly, predicted = BF2$SMAP_monthly)
nse_valueSMAP <- NSE(sim = BF2$SMAP_monthly, obs = BF2$Field_ET_monthly)
rsr_valueSMAP <- rmse_valueSMAP / sd(BF2$Field_ET_monthly)

biasSMAP <- mean(BF2$SMAP_monthly - BF2$Field_ET_monthly)
pbiasSMAP <- 100 * sum(BF2$SMAP_monthly - BF2$Field_ET_monthly) / sum(BF2$Field_ET_monthly)
maeSMAP <- mean(abs(BF2$SMAP_monthly - BF2$Field_ET_monthly))

spearman_corrSMAP <- cor.test(BF2$Field_ET_monthly, BF2$SMAP_monthly, method = "spearman")
print(spearman_corrSMAP)
correlation_coefficientSMAP <- spearman_corrSMAP$estimate

# WAPOR
kge_valueWaPOR <- KGE(sim = BF2$WaPOR_monthly, obs = BF2$Field_ET_monthly)
rmse_valueWaPOR <- rmse(actual = BF2$Field_ET_monthly[!is.na(BF2$Field_ET_monthly) & !is.na(BF2$WaPOR_monthly)],
                        predicted = BF2$WaPOR_monthly[!is.na(BF2$Field_ET_monthly) & !is.na(BF2$WaPOR_monthly)])
nse_valueWaPOR <- NSE(sim = BF2$WaPOR_monthly, obs = BF2$Field_ET_monthly)
rsr_valueWaPOR <- rmse_valueWaPOR / sd(BF2$Field_ET_monthly, na.rm = TRUE)

biasWaPOR <- mean(BF2$WaPOR_monthly - BF2$Field_ET_monthly, na.rm = TRUE)
pbiasWaPOR <- 100 * sum(BF2$WaPOR_monthly - BF2$Field_ET_monthly, na.rm = TRUE) / sum(BF2$Field_ET_monthly, na.rm = TRUE)
maeWaPOR <- mean(abs(BF2$WaPOR_monthly - BF2$Field_ET_monthly), na.rm = TRUE)

spearman_corrWaPOR <- cor.test(BF2$Field_ET_monthly, BF2$WaPOR_monthly, method = "spearman")
print(spearman_corrWaPOR)
correlation_coefficientWaPOR <- spearman_corrWaPOR$estimate

# MOD16
kge_valueMOD16 <- KGE(sim = BF2$MOD16_monthly, obs = BF2$Field_ET_monthly)
rmse_valueMOD16 <- rmse(actual = BF2$Field_ET_monthly, predicted = BF2$MOD16_monthly)
nse_valueMOD16 <- NSE(sim = BF2$MOD16_monthly, obs = BF2$Field_ET_monthly)
rsr_valueMOD16 <- rmse_valueMOD16 / sd(BF2$Field_ET_monthly)

biasMOD16 <- mean(BF2$MOD16_monthly - BF2$Field_ET_monthly)
pbiasMOD16 <- 100 * sum(BF2$MOD16_monthly - BF2$Field_ET_monthly) / sum(BF2$Field_ET_monthly)
maeMOD16 <- mean(abs(BF2$MOD16_monthly - BF2$Field_ET_monthly))

spearman_corrMOD16 <- cor.test(BF2$Field_ET_monthly, BF2$MOD16_monthly, method = "spearman")
print(spearman_corrMOD16)
correlation_coefficientMOD16 <- spearman_corrMOD16$estimate

# GLDAS
kge_valueGLDAS <- KGE(sim = BF2$GLDAS_monthly, obs = BF2$Field_ET_monthly)
rmse_valueGLDAS <- rmse(actual = BF2$Field_ET_monthly, predicted = BF2$GLDAS_monthly)
nse_valueGLDAS <- NSE(sim = BF2$GLDAS_monthly, obs = BF2$Field_ET_monthly)
rsr_valueGLDAS <- rmse_valueGLDAS / sd(BF2$Field_ET_monthly)

biasGLDAS <- mean(BF2$GLDAS_monthly - BF2$Field_ET_monthly)
pbiasGLDAS <- 100 * sum(BF2$GLDAS_monthly - BF2$Field_ET_monthly) / sum(BF2$Field_ET_monthly)
maeGLDAS <- mean(abs(BF2$GLDAS_monthly - BF2$Field_ET_monthly))

spearman_corrGLDAS <- cor.test(BF2$Field_ET_monthly, BF2$GLDAS_monthly, method = "spearman")
print(spearman_corrGLDAS)
correlation_coefficientGLDAS <- spearman_corrGLDAS$estimate

# FLDAS
kge_valueFLDAS <- KGE(sim = BF2$FLDAS_monthly, obs = BF2$Field_ET_monthly)
rmse_valueFLDAS <- rmse(actual = BF2$Field_ET_monthly, predicted = BF2$FLDAS_monthly)
nse_valueFLDAS <- NSE(sim = BF2$FLDAS_monthly, obs = BF2$Field_ET_monthly)
rsr_valueFLDAS <- rmse_valueFLDAS / sd(BF2$Field_ET_monthly)

biasFLDAS <- mean(BF2$FLDAS_monthly - BF2$Field_ET_monthly)
pbiasFLDAS <- 100 * sum(BF2$FLDAS_monthly - BF2$Field_ET_monthly) / sum(BF2$Field_ET_monthly)
maeFLDAS <- mean(abs(BF2$FLDAS_monthly - BF2$Field_ET_monthly))

spearman_corrFLDAS <- cor.test(BF2$Field_ET_monthly, BF2$FLDAS_monthly, method = "spearman")
print(spearman_corrFLDAS)
correlation_coefficientFLDAS <- spearman_corrFLDAS$estimate

# PTJPL
kge_valuePTjpl <- KGE(sim = BF2$PTjpl_monthly, obs = BF2$Field_ET_monthly)
rmse_valuePTjpl <- rmse(actual = BF2$Field_ET_monthly[!is.na(BF2$Field_ET_monthly) & !is.na(BF2$PTjpl_monthly)], 
                        predicted = BF2$PTjpl_monthly[!is.na(BF2$Field_ET_monthly) & !is.na(BF2$PTjpl_monthly)])
nse_valuePTjpl <- NSE(sim = BF2$PTjpl_monthly, obs = BF2$Field_ET_monthly)
rsr_valuePTjpl <- rmse_valuePTjpl / sd(BF2$Field_ET_monthly, na.rm = TRUE)

biasPTjpl <- mean(BF2$PTjpl_monthly - BF2$Field_ET_monthly, na.rm = TRUE)
pbiasPTjpl <- 100 * sum(BF2$PTjpl_monthly - BF2$Field_ET_monthly,na.rm = TRUE) / sum(BF2$Field_ET_monthly, na.rm = TRUE)
maePTjpl <- mean(abs(BF2$PTjpl_monthly - BF2$Field_ET_monthly), na.rm = TRUE)

spearman_corrPTjpl <- cor.test(BF2$Field_ET_monthly, BF2$PTjpl_monthly, method = "spearman")
print(spearman_corrPTjpl)
correlation_coefficientPTjpl <- spearman_corrPTjpl$estimate

df_stats <- data.frame(rsr_valueTC, rsr_valueWaPOR, rsr_valueSMAP, rsr_valueFLDAS, rsr_valueGLDAS, rsr_valueMOD16, rsr_valuePTjpl,
                       rmse_valueTC, rmse_valueWaPOR, rmse_valueSMAP, rmse_valueFLDAS, rmse_valueGLDAS, rmse_valueMOD16, rmse_valuePTjpl,
                       pbiasTC, pbiasWaPOR, pbiasSMAP, pbiasFLDAS, pbiasGLDAS, pbiasMOD16, pbiasPTjpl,
                       biasTC, biasWaPOR, biasSMAP, biasFLDAS, biasGLDAS, biasMOD16, biasPTjpl,
                       maeTC, maeWaPOR, maeSMAP, maeFLDAS, maeGLDAS, maeMOD16, maePTjpl,
                       nse_valueTC, nse_valueWaPOR, nse_valueSMAP, nse_valueFLDAS, nse_valueGLDAS, nse_valueMOD16, nse_valuePTjpl,
                       kge_valueTC, kge_valueWaPOR, kge_valueSMAP, kge_valueFLDAS, kge_valueGLDAS, kge_valueMOD16, kge_valuePTjpl,
                       correlation_coefficientTC, correlation_coefficientWaPOR, correlation_coefficientSMAP, correlation_coefficientFLDAS, correlation_coefficientGLDAS, correlation_coefficientMOD16, correlation_coefficientPTjpl)

# Reshape the data frame from wide to long format
df_long <- df_stats %>%
  pivot_longer(
    cols = everything(),
    names_to = c("metric", "site"),  # Separate columns for metric names and sites
    names_pattern = "^(pbias|nse_value|kge_value|correlation_coefficient)(.*)" # Specify explicit metric names
  ) %>%
  pivot_wider(
    names_from = metric,             # Separate each metric into its own column
    values_from = value
  )

# View the reshaped data
head(df_long)
setwd("C:\\Users\\Liam\\OneDrive - Stellenbosch University\\ET_Data\\Stats")
write.csv(df_stats, file = "Indv_Seasonal_BF2.csv", row.names = FALSE)




###################################################################################################
####################################################################################################
# CP3
# TC 
kge_valueTC <- KGE(sim = CP3$TC_monthly, obs = CP3$Field_ET_monthly)
rmse_valueTC <- rmse(actual = CP3$Field_ET_monthly, predicted = CP3$TC_monthly)
nse_valueTC <- NSE(sim = CP3$TC_monthly, obs = CP3$Field_ET_monthly)
rsr_valueTC <- rmse_valueTC / sd(CP3$Field_ET_monthly)

biasTC <- mean(CP3$TC_monthly - CP3$Field_ET_monthly)
pbiasTC <- 100 * sum(CP3$TC_monthly - CP3$Field_ET_monthly) / sum(CP3$Field_ET_monthly)
maeTC <- mean(abs(CP3$TC_monthly - CP3$Field_ET_monthly))

spearman_corrTC <- cor.test(CP3$Field_ET_monthly, CP3$TC_monthly, method = "spearman")
print(spearman_corrTC)
correlation_coefficientTC <- spearman_corrTC$estimate

# SMAP
kge_valueSMAP <- KGE(sim = CP3$SMAP_monthly, obs = CP3$Field_ET_monthly)
rmse_valueSMAP <- rmse(actual = CP3$Field_ET_monthly, predicted = CP3$SMAP_monthly)
nse_valueSMAP <- NSE(sim = CP3$SMAP_monthly, obs = CP3$Field_ET_monthly)
rsr_valueSMAP <- rmse_valueSMAP / sd(CP3$Field_ET_monthly)

biasSMAP <- mean(CP3$SMAP_monthly - CP3$Field_ET_monthly)
pbiasSMAP <- 100 * sum(CP3$SMAP_monthly - CP3$Field_ET_monthly) / sum(CP3$Field_ET_monthly)
maeSMAP <- mean(abs(CP3$SMAP_monthly - CP3$Field_ET_monthly))

spearman_corrSMAP <- cor.test(CP3$Field_ET_monthly, CP3$SMAP_monthly, method = "spearman")
print(spearman_corrSMAP)
correlation_coefficientSMAP <- spearman_corrSMAP$estimate

# SSEBop
kge_valueSSEBop <- KGE(sim = CP3$SSEBop_monthly, obs = CP3$Field_ET_monthly)
rmse_valueSSEBop <- rmse(actual = CP3$Field_ET_monthly[!is.na(CP3$Field_ET_monthly) & !is.na(CP3$SSEBop_monthly)], 
                         predicted = CP3$SSEBop_monthly[!is.na(CP3$Field_ET_monthly) & !is.na(CP3$SSEBop_monthly)])
nse_valueSSEBop <- NSE(sim = CP3$SSEBop_monthly, obs = CP3$Field_ET_monthly)
rsr_valueSSEBop <- rmse_valueSSEBop / sd(CP3$Field_ET_monthly, na.rm = TRUE)

biasSSEBop <- mean(CP3$SSEBop_monthly - CP3$Field_ET_monthly, na.rm = TRUE)
pbiasSSEBop <- 100 * sum(CP3$SSEBop_monthly - CP3$Field_ET_monthly, na.rm = TRUE) / sum(CP3$Field_ET_monthly, na.rm = TRUE)
maeSSEBop <- mean(abs(CP3$SSEBop_monthly - CP3$Field_ET_monthly), na.rm = TRUE)

spearman_corrSSEBop <- cor.test(CP3$Field_ET_monthly, CP3$SSEBop_monthly, method = "spearman")
print(spearman_corrSSEBop)
correlation_coefficientSSEBop <- spearman_corrSSEBop$estimate

# WAPOR
kge_valueWaPOR <- KGE(sim = CP3$WaPOR_monthly, obs = CP3$Field_ET_monthly)
rmse_valueWaPOR <- rmse(actual = CP3$Field_ET_monthly[!is.na(CP3$Field_ET_monthly) & !is.na(CP3$WaPOR_monthly)],
                        predicted = CP3$WaPOR_monthly[!is.na(CP3$Field_ET_monthly) & !is.na(CP3$WaPOR_monthly)])
nse_valueWaPOR <- NSE(sim = CP3$WaPOR_monthly, obs = CP3$Field_ET_monthly)
rsr_valueWaPOR <- rmse_valueWaPOR / sd(CP3$Field_ET_monthly, na.rm = TRUE)

biasWaPOR <- mean(CP3$WaPOR_monthly - CP3$Field_ET_monthly, na.rm = TRUE)
pbiasWaPOR <- 100 * sum(CP3$WaPOR_monthly - CP3$Field_ET_monthly, na.rm = TRUE) / sum(CP3$Field_ET_monthly, na.rm = TRUE)
maeWaPOR <- mean(abs(CP3$WaPOR_monthly - CP3$Field_ET_monthly), na.rm = TRUE)

spearman_corrWaPOR <- cor.test(CP3$Field_ET_monthly, CP3$WaPOR_monthly, method = "spearman")
print(spearman_corrWaPOR)
correlation_coefficientWaPOR <- spearman_corrWaPOR$estimate

# MOD16
kge_valueMOD16 <- KGE(sim = CP3$MOD16_monthly, obs = CP3$Field_ET_monthly)
rmse_valueMOD16 <- rmse(actual = CP3$Field_ET_monthly, predicted = CP3$MOD16_monthly)
nse_valueMOD16 <- NSE(sim = CP3$MOD16_monthly, obs = CP3$Field_ET_monthly)
rsr_valueMOD16 <- rmse_valueMOD16 / sd(CP3$Field_ET_monthly)

biasMOD16 <- mean(CP3$MOD16_monthly - CP3$Field_ET_monthly)
pbiasMOD16 <- 100 * sum(CP3$MOD16_monthly - CP3$Field_ET_monthly) / sum(CP3$Field_ET_monthly)
maeMOD16 <- mean(abs(CP3$MOD16_monthly - CP3$Field_ET_monthly))

spearman_corrMOD16 <- cor.test(CP3$Field_ET_monthly, CP3$MOD16_monthly, method = "spearman")
print(spearman_corrMOD16)
correlation_coefficientMOD16 <- spearman_corrMOD16$estimate

# GLDAS
kge_valueGLDAS <- KGE(sim = CP3$GLDAS_monthly, obs = CP3$Field_ET_monthly)
rmse_valueGLDAS <- rmse(actual = CP3$Field_ET_monthly, predicted = CP3$GLDAS_monthly)
nse_valueGLDAS <- NSE(sim = CP3$GLDAS_monthly, obs = CP3$Field_ET_monthly)
rsr_valueGLDAS <- rmse_valueGLDAS / sd(CP3$Field_ET_monthly)

biasGLDAS <- mean(CP3$GLDAS_monthly - CP3$Field_ET_monthly)
pbiasGLDAS <- 100 * sum(CP3$GLDAS_monthly - CP3$Field_ET_monthly) / sum(CP3$Field_ET_monthly)
maeGLDAS <- mean(abs(CP3$GLDAS_monthly - CP3$Field_ET_monthly))

spearman_corrGLDAS <- cor.test(CP3$Field_ET_monthly, CP3$GLDAS_monthly, method = "spearman")
print(spearman_corrGLDAS)
correlation_coefficientGLDAS <- spearman_corrGLDAS$estimate

# FLDAS
kge_valueFLDAS <- KGE(sim = CP3$FLDAS_monthly, obs = CP3$Field_ET_monthly)
rmse_valueFLDAS <- rmse(actual = CP3$Field_ET_monthly, predicted = CP3$FLDAS_monthly)
nse_valueFLDAS <- NSE(sim = CP3$FLDAS_monthly, obs = CP3$Field_ET_monthly)
rsr_valueFLDAS <- rmse_valueFLDAS / sd(CP3$Field_ET_monthly)

biasFLDAS <- mean(CP3$FLDAS_monthly - CP3$Field_ET_monthly)
pbiasFLDAS <- 100 * sum(CP3$FLDAS_monthly - CP3$Field_ET_monthly) / sum(CP3$Field_ET_monthly)
maeFLDAS <- mean(abs(CP3$FLDAS_monthly - CP3$Field_ET_monthly))

spearman_corrFLDAS <- cor.test(CP3$Field_ET_monthly, CP3$FLDAS_monthly, method = "spearman")
print(spearman_corrFLDAS)
correlation_coefficientFLDAS <- spearman_corrFLDAS$estimate

# PTJPL
kge_valuePTjpl <- KGE(sim = CP3$PTjpl_monthly, obs = CP3$Field_ET_monthly)
rmse_valuePTjpl <- rmse(actual = CP3$Field_ET_monthly[!is.na(CP3$Field_ET_monthly) & !is.na(CP3$PTjpl_monthly)], 
                        predicted = CP3$PTjpl_monthly[!is.na(CP3$Field_ET_monthly) & !is.na(CP3$PTjpl_monthly)])
nse_valuePTjpl <- NSE(sim = CP3$PTjpl_monthly, obs = CP3$Field_ET_monthly)
rsr_valuePTjpl <- rmse_valuePTjpl / sd(CP3$Field_ET_monthly, na.rm = TRUE)

biasPTjpl <- mean(CP3$PTjpl_monthly - CP3$Field_ET_monthly, na.rm = TRUE)
pbiasPTjpl <- 100 * sum(CP3$PTjpl_monthly - CP3$Field_ET_monthly,na.rm = TRUE) / sum(CP3$Field_ET_monthly, na.rm = TRUE)
maePTjpl <- mean(abs(CP3$PTjpl_monthly - CP3$Field_ET_monthly), na.rm = TRUE)

spearman_corrPTjpl <- cor.test(CP3$Field_ET_monthly, CP3$PTjpl_monthly, method = "spearman")
print(spearman_corrPTjpl)
correlation_coefficientPTjpl <- spearman_corrPTjpl$estimate

df_stats <- data.frame(rsr_valueTC, rsr_valueWaPOR, rsr_valueSMAP, rsr_valueFLDAS, rsr_valueGLDAS, rsr_valueMOD16, rsr_valueSSEBop, rsr_valuePTjpl,
                       rmse_valueTC, rmse_valueWaPOR, rmse_valueSMAP, rmse_valueFLDAS, rmse_valueGLDAS, rmse_valueMOD16, rmse_valueSSEBop, rmse_valuePTjpl,
                       pbiasTC, pbiasWaPOR, pbiasSMAP, pbiasFLDAS, pbiasGLDAS, pbiasMOD16, pbiasSSEBop, pbiasPTjpl,
                       biasTC, biasWaPOR, biasSMAP, biasFLDAS, biasGLDAS, biasMOD16, biasSSEBop, biasPTjpl,
                       maeTC, maeWaPOR, maeSMAP, maeFLDAS, maeGLDAS, maeMOD16, maeSSEBop, maePTjpl,
                       nse_valueTC, nse_valueWaPOR, nse_valueSMAP, nse_valueFLDAS, nse_valueGLDAS, nse_valueMOD16, nse_valueSSEBop, nse_valuePTjpl,
                       kge_valueTC, kge_valueWaPOR, kge_valueSMAP, kge_valueFLDAS, kge_valueGLDAS, kge_valueMOD16, kge_valueSSEBop, kge_valuePTjpl,
                       correlation_coefficientTC, correlation_coefficientWaPOR, correlation_coefficientSMAP, correlation_coefficientFLDAS, correlation_coefficientGLDAS, correlation_coefficientMOD16, correlation_coefficientSSEBop, correlation_coefficientPTjpl)

# Reshape the data frame from wide to long format
df_long <- df_stats %>%
  pivot_longer(
    cols = everything(),
    names_to = c("metric", "site"),  # Separate columns for metric names and sites
    names_pattern = "^(pbias|nse_value|kge_value|correlation_coefficient)(.*)" # Specify explicit metric names
  ) %>%
  pivot_wider(
    names_from = metric,             # Separate each metric into its own column
    values_from = value
  )

# View the reshaped data
head(df_long)
setwd("C:\\Users\\Liam\\OneDrive - Stellenbosch University\\ET_Data\\Stats")
write.csv(df_stats, file = "Indv_Seasonal_CP3.csv", row.names = FALSE)

###################################################################################################
####################################################################################################
# CP6
# TC 
kge_valueTC <- KGE(sim = CP6$TC_monthly, obs = CP6$Field_ET_monthly)
rmse_valueTC <- rmse(actual = CP6$Field_ET_monthly, predicted = CP6$TC_monthly)
nse_valueTC <- NSE(sim = CP6$TC_monthly, obs = CP6$Field_ET_monthly)
rsr_valueTC <- rmse_valueTC / sd(CP6$Field_ET_monthly)

biasTC <- mean(CP6$TC_monthly - CP6$Field_ET_monthly)
pbiasTC <- 100 * sum(CP6$TC_monthly - CP6$Field_ET_monthly) / sum(CP6$Field_ET_monthly)
maeTC <- mean(abs(CP6$TC_monthly - CP6$Field_ET_monthly))

spearman_corrTC <- cor.test(CP6$Field_ET_monthly, CP6$TC_monthly, method = "spearman")
print(spearman_corrTC)
correlation_coefficientTC <- spearman_corrTC$estimate

# SMAP
kge_valueSMAP <- KGE(sim = CP6$SMAP_monthly, obs = CP6$Field_ET_monthly)
rmse_valueSMAP <- rmse(actual = CP6$Field_ET_monthly, predicted = CP6$SMAP_monthly)
nse_valueSMAP <- NSE(sim = CP6$SMAP_monthly, obs = CP6$Field_ET_monthly)
rsr_valueSMAP <- rmse_valueSMAP / sd(CP6$Field_ET_monthly)

biasSMAP <- mean(CP6$SMAP_monthly - CP6$Field_ET_monthly)
pbiasSMAP <- 100 * sum(CP6$SMAP_monthly - CP6$Field_ET_monthly) / sum(CP6$Field_ET_monthly)
maeSMAP <- mean(abs(CP6$SMAP_monthly - CP6$Field_ET_monthly))

spearman_corrSMAP <- cor.test(CP6$Field_ET_monthly, CP6$SMAP_monthly, method = "spearman")
print(spearman_corrSMAP)
correlation_coefficientSMAP <- spearman_corrSMAP$estimate

# SSEBop
kge_valueSSEBop <- KGE(sim = CP6$SSEBop_monthly, obs = CP6$Field_ET_monthly)
rmse_valueSSEBop <- rmse(actual = CP6$Field_ET_monthly[!is.na(CP6$Field_ET_monthly) & !is.na(CP6$SSEBop_monthly)], 
                         predicted = CP6$SSEBop_monthly[!is.na(CP6$Field_ET_monthly) & !is.na(CP6$SSEBop_monthly)])
nse_valueSSEBop <- NSE(sim = CP6$SSEBop_monthly, obs = CP6$Field_ET_monthly)
rsr_valueSSEBop <- rmse_valueSSEBop / sd(CP6$Field_ET_monthly, na.rm = TRUE)

biasSSEBop <- mean(CP6$SSEBop_monthly - CP6$Field_ET_monthly, na.rm = TRUE)
pbiasSSEBop <- 100 * sum(CP6$SSEBop_monthly - CP6$Field_ET_monthly, na.rm = TRUE) / sum(CP6$Field_ET_monthly, na.rm = TRUE)
maeSSEBop <- mean(abs(CP6$SSEBop_monthly - CP6$Field_ET_monthly), na.rm = TRUE)

spearman_corrSSEBop <- cor.test(CP6$Field_ET_monthly, CP6$SSEBop_monthly, method = "spearman")
print(spearman_corrSSEBop)
correlation_coefficientSSEBop <- spearman_corrSSEBop$estimate

# WAPOR
kge_valueWaPOR <- KGE(sim = CP6$WaPOR_monthly, obs = CP6$Field_ET_monthly)
rmse_valueWaPOR <- rmse(actual = CP6$Field_ET_monthly[!is.na(CP6$Field_ET_monthly) & !is.na(CP6$WaPOR_monthly)],
                        predicted = CP6$WaPOR_monthly[!is.na(CP6$Field_ET_monthly) & !is.na(CP6$WaPOR_monthly)])
nse_valueWaPOR <- NSE(sim = CP6$WaPOR_monthly, obs = CP6$Field_ET_monthly)
rsr_valueWaPOR <- rmse_valueWaPOR / sd(CP6$Field_ET_monthly, na.rm = TRUE)

biasWaPOR <- mean(CP6$WaPOR_monthly - CP6$Field_ET_monthly, na.rm = TRUE)
pbiasWaPOR <- 100 * sum(CP6$WaPOR_monthly - CP6$Field_ET_monthly, na.rm = TRUE) / sum(CP6$Field_ET_monthly, na.rm = TRUE)
maeWaPOR <- mean(abs(CP6$WaPOR_monthly - CP6$Field_ET_monthly), na.rm = TRUE)

spearman_corrWaPOR <- cor.test(CP6$Field_ET_monthly, CP6$WaPOR_monthly, method = "spearman")
print(spearman_corrWaPOR)
correlation_coefficientWaPOR <- spearman_corrWaPOR$estimate

# MOD16
kge_valueMOD16 <- KGE(sim = CP6$MOD16_monthly, obs = CP6$Field_ET_monthly)
rmse_valueMOD16 <- rmse(actual = CP6$Field_ET_monthly, predicted = CP6$MOD16_monthly)
nse_valueMOD16 <- NSE(sim = CP6$MOD16_monthly, obs = CP6$Field_ET_monthly)
rsr_valueMOD16 <- rmse_valueMOD16 / sd(CP6$Field_ET_monthly)

biasMOD16 <- mean(CP6$MOD16_monthly - CP6$Field_ET_monthly)
pbiasMOD16 <- 100 * sum(CP6$MOD16_monthly - CP6$Field_ET_monthly) / sum(CP6$Field_ET_monthly)
maeMOD16 <- mean(abs(CP6$MOD16_monthly - CP6$Field_ET_monthly))

spearman_corrMOD16 <- cor.test(CP6$Field_ET_monthly, CP6$MOD16_monthly, method = "spearman")
print(spearman_corrMOD16)
correlation_coefficientMOD16 <- spearman_corrMOD16$estimate

# GLDAS
kge_valueGLDAS <- KGE(sim = CP6$GLDAS_monthly, obs = CP6$Field_ET_monthly)
rmse_valueGLDAS <- rmse(actual = CP6$Field_ET_monthly, predicted = CP6$GLDAS_monthly)
nse_valueGLDAS <- NSE(sim = CP6$GLDAS_monthly, obs = CP6$Field_ET_monthly)
rsr_valueGLDAS <- rmse_valueGLDAS / sd(CP6$Field_ET_monthly)

biasGLDAS <- mean(CP6$GLDAS_monthly - CP6$Field_ET_monthly)
pbiasGLDAS <- 100 * sum(CP6$GLDAS_monthly - CP6$Field_ET_monthly) / sum(CP6$Field_ET_monthly)
maeGLDAS <- mean(abs(CP6$GLDAS_monthly - CP6$Field_ET_monthly))

spearman_corrGLDAS <- cor.test(CP6$Field_ET_monthly, CP6$GLDAS_monthly, method = "spearman")
print(spearman_corrGLDAS)
correlation_coefficientGLDAS <- spearman_corrGLDAS$estimate

# FLDAS
kge_valueFLDAS <- KGE(sim = CP6$FLDAS_monthly, obs = CP6$Field_ET_monthly)
rmse_valueFLDAS <- rmse(actual = CP6$Field_ET_monthly, predicted = CP6$FLDAS_monthly)
nse_valueFLDAS <- NSE(sim = CP6$FLDAS_monthly, obs = CP6$Field_ET_monthly)
rsr_valueFLDAS <- rmse_valueFLDAS / sd(CP6$Field_ET_monthly)

biasFLDAS <- mean(CP6$FLDAS_monthly - CP6$Field_ET_monthly)
pbiasFLDAS <- 100 * sum(CP6$FLDAS_monthly - CP6$Field_ET_monthly) / sum(CP6$Field_ET_monthly)
maeFLDAS <- mean(abs(CP6$FLDAS_monthly - CP6$Field_ET_monthly))

spearman_corrFLDAS <- cor.test(CP6$Field_ET_monthly, CP6$FLDAS_monthly, method = "spearman")
print(spearman_corrFLDAS)
correlation_coefficientFLDAS <- spearman_corrFLDAS$estimate

# PTJPL
kge_valuePTjpl <- KGE(sim = CP6$PTjpl_monthly, obs = CP6$Field_ET_monthly)
rmse_valuePTjpl <- rmse(actual = CP6$Field_ET_monthly[!is.na(CP6$Field_ET_monthly) & !is.na(CP6$PTjpl_monthly)], 
                        predicted = CP6$PTjpl_monthly[!is.na(CP6$Field_ET_monthly) & !is.na(CP6$PTjpl_monthly)])
nse_valuePTjpl <- NSE(sim = CP6$PTjpl_monthly, obs = CP6$Field_ET_monthly)
rsr_valuePTjpl <- rmse_valuePTjpl / sd(CP6$Field_ET_monthly, na.rm = TRUE)

biasPTjpl <- mean(CP6$PTjpl_monthly - CP6$Field_ET_monthly, na.rm = TRUE)
pbiasPTjpl <- 100 * sum(CP6$PTjpl_monthly - CP6$Field_ET_monthly,na.rm = TRUE) / sum(CP6$Field_ET_monthly, na.rm = TRUE)
maePTjpl <- mean(abs(CP6$PTjpl_monthly - CP6$Field_ET_monthly), na.rm = TRUE)

spearman_corrPTjpl <- cor.test(CP6$Field_ET_monthly, CP6$PTjpl_monthly, method = "spearman")
print(spearman_corrPTjpl)
correlation_coefficientPTjpl <- spearman_corrPTjpl$estimate

df_stats <- data.frame(rsr_valueTC, rsr_valueWaPOR, rsr_valueSMAP, rsr_valueFLDAS, rsr_valueGLDAS, rsr_valueMOD16, rsr_valueSSEBop, rsr_valuePTjpl,
                       rmse_valueTC, rmse_valueWaPOR, rmse_valueSMAP, rmse_valueFLDAS, rmse_valueGLDAS, rmse_valueMOD16, rmse_valueSSEBop, rmse_valuePTjpl,
                       pbiasTC, pbiasWaPOR, pbiasSMAP, pbiasFLDAS, pbiasGLDAS, pbiasMOD16, pbiasSSEBop, pbiasPTjpl,
                       biasTC, biasWaPOR, biasSMAP, biasFLDAS, biasGLDAS, biasMOD16, biasSSEBop, biasPTjpl,
                       maeTC, maeWaPOR, maeSMAP, maeFLDAS, maeGLDAS, maeMOD16, maeSSEBop, maePTjpl,
                       nse_valueTC, nse_valueWaPOR, nse_valueSMAP, nse_valueFLDAS, nse_valueGLDAS, nse_valueMOD16, nse_valueSSEBop, nse_valuePTjpl,
                       kge_valueTC, kge_valueWaPOR, kge_valueSMAP, kge_valueFLDAS, kge_valueGLDAS, kge_valueMOD16, kge_valueSSEBop, kge_valuePTjpl,
                       correlation_coefficientTC, correlation_coefficientWaPOR, correlation_coefficientSMAP, correlation_coefficientFLDAS, correlation_coefficientGLDAS, correlation_coefficientMOD16, correlation_coefficientSSEBop, correlation_coefficientPTjpl)

# Reshape the data frame from wide to long format
df_long <- df_stats %>%
  pivot_longer(
    cols = everything(),
    names_to = c("metric", "site"),  # Separate columns for metric names and sites
    names_pattern = "^(pbias|nse_value|kge_value|correlation_coefficient)(.*)" # Specify explicit metric names
  ) %>%
  pivot_wider(
    names_from = metric,             # Separate each metric into its own column
    values_from = value
  )

# View the reshaped data
head(df_long)
setwd("C:\\Users\\Liam\\OneDrive - Stellenbosch University\\ET_Data\\Stats")
write.csv(df_stats, file = "Indv_Seasonal_CP6.csv", row.names = FALSE)

###################################################################################################
####################################################################################################
# CP9
# TC 
kge_valueTC <- KGE(sim = CP9$TC_monthly, obs = CP9$Field_ET_monthly)
rmse_valueTC <- rmse(actual = CP9$Field_ET_monthly, predicted = CP9$TC_monthly)
nse_valueTC <- NSE(sim = CP9$TC_monthly, obs = CP9$Field_ET_monthly)
rsr_valueTC <- rmse_valueTC / sd(CP9$Field_ET_monthly)

biasTC <- mean(CP9$TC_monthly - CP9$Field_ET_monthly)
pbiasTC <- 100 * sum(CP9$TC_monthly - CP9$Field_ET_monthly) / sum(CP9$Field_ET_monthly)
maeTC <- mean(abs(CP9$TC_monthly - CP9$Field_ET_monthly))

spearman_corrTC <- cor.test(CP9$Field_ET_monthly, CP9$TC_monthly, method = "spearman")
print(spearman_corrTC)
correlation_coefficientTC <- spearman_corrTC$estimate

# SMAP
kge_valueSMAP <- KGE(sim = CP9$SMAP_monthly, obs = CP9$Field_ET_monthly)
rmse_valueSMAP <- rmse(actual = CP9$Field_ET_monthly, predicted = CP9$SMAP_monthly)
nse_valueSMAP <- NSE(sim = CP9$SMAP_monthly, obs = CP9$Field_ET_monthly)
rsr_valueSMAP <- rmse_valueSMAP / sd(CP9$Field_ET_monthly)

biasSMAP <- mean(CP9$SMAP_monthly - CP9$Field_ET_monthly)
pbiasSMAP <- 100 * sum(CP9$SMAP_monthly - CP9$Field_ET_monthly) / sum(CP9$Field_ET_monthly)
maeSMAP <- mean(abs(CP9$SMAP_monthly - CP9$Field_ET_monthly))

spearman_corrSMAP <- cor.test(CP9$Field_ET_monthly, CP9$SMAP_monthly, method = "spearman")
print(spearman_corrSMAP)
correlation_coefficientSMAP <- spearman_corrSMAP$estimate

# SSEBop
kge_valueSSEBop <- KGE(sim = CP9$SSEBop_monthly, obs = CP9$Field_ET_monthly)
rmse_valueSSEBop <- rmse(actual = CP9$Field_ET_monthly[!is.na(CP9$Field_ET_monthly) & !is.na(CP9$SSEBop_monthly)], 
                         predicted = CP9$SSEBop_monthly[!is.na(CP9$Field_ET_monthly) & !is.na(CP9$SSEBop_monthly)])
nse_valueSSEBop <- NSE(sim = CP9$SSEBop_monthly, obs = CP9$Field_ET_monthly)
rsr_valueSSEBop <- rmse_valueSSEBop / sd(CP9$Field_ET_monthly, na.rm = TRUE)

biasSSEBop <- mean(CP9$SSEBop_monthly - CP9$Field_ET_monthly, na.rm = TRUE)
pbiasSSEBop <- 100 * sum(CP9$SSEBop_monthly - CP9$Field_ET_monthly, na.rm = TRUE) / sum(CP9$Field_ET_monthly, na.rm = TRUE)
maeSSEBop <- mean(abs(CP9$SSEBop_monthly - CP9$Field_ET_monthly), na.rm = TRUE)

spearman_corrSSEBop <- cor.test(CP9$Field_ET_monthly, CP9$SSEBop_monthly, method = "spearman")
print(spearman_corrSSEBop)
correlation_coefficientSSEBop <- spearman_corrSSEBop$estimate

# WAPOR
kge_valueWaPOR <- KGE(sim = CP9$WaPOR_monthly, obs = CP9$Field_ET_monthly)
rmse_valueWaPOR <- rmse(actual = CP9$Field_ET_monthly[!is.na(CP9$Field_ET_monthly) & !is.na(CP9$WaPOR_monthly)],
                        predicted = CP9$WaPOR_monthly[!is.na(CP9$Field_ET_monthly) & !is.na(CP9$WaPOR_monthly)])
nse_valueWaPOR <- NSE(sim = CP9$WaPOR_monthly, obs = CP9$Field_ET_monthly)
rsr_valueWaPOR <- rmse_valueWaPOR / sd(CP9$Field_ET_monthly, na.rm = TRUE)

biasWaPOR <- mean(CP9$WaPOR_monthly - CP9$Field_ET_monthly, na.rm = TRUE)
pbiasWaPOR <- 100 * sum(CP9$WaPOR_monthly - CP9$Field_ET_monthly, na.rm = TRUE) / sum(CP9$Field_ET_monthly, na.rm = TRUE)
maeWaPOR <- mean(abs(CP9$WaPOR_monthly - CP9$Field_ET_monthly), na.rm = TRUE)

spearman_corrWaPOR <- cor.test(CP9$Field_ET_monthly, CP9$WaPOR_monthly, method = "spearman")
print(spearman_corrWaPOR)
correlation_coefficientWaPOR <- spearman_corrWaPOR$estimate

# MOD16
kge_valueMOD16 <- KGE(sim = CP9$MOD16_monthly, obs = CP9$Field_ET_monthly)
rmse_valueMOD16 <- rmse(actual = CP9$Field_ET_monthly, predicted = CP9$MOD16_monthly)
nse_valueMOD16 <- NSE(sim = CP9$MOD16_monthly, obs = CP9$Field_ET_monthly)
rsr_valueMOD16 <- rmse_valueMOD16 / sd(CP9$Field_ET_monthly)

biasMOD16 <- mean(CP9$MOD16_monthly - CP9$Field_ET_monthly)
pbiasMOD16 <- 100 * sum(CP9$MOD16_monthly - CP9$Field_ET_monthly) / sum(CP9$Field_ET_monthly)
maeMOD16 <- mean(abs(CP9$MOD16_monthly - CP9$Field_ET_monthly))

spearman_corrMOD16 <- cor.test(CP9$Field_ET_monthly, CP9$MOD16_monthly, method = "spearman")
print(spearman_corrMOD16)
correlation_coefficientMOD16 <- spearman_corrMOD16$estimate

# GLDAS
kge_valueGLDAS <- KGE(sim = CP9$GLDAS_monthly, obs = CP9$Field_ET_monthly)
rmse_valueGLDAS <- rmse(actual = CP9$Field_ET_monthly, predicted = CP9$GLDAS_monthly)
nse_valueGLDAS <- NSE(sim = CP9$GLDAS_monthly, obs = CP9$Field_ET_monthly)
rsr_valueGLDAS <- rmse_valueGLDAS / sd(CP9$Field_ET_monthly)

biasGLDAS <- mean(CP9$GLDAS_monthly - CP9$Field_ET_monthly)
pbiasGLDAS <- 100 * sum(CP9$GLDAS_monthly - CP9$Field_ET_monthly) / sum(CP9$Field_ET_monthly)
maeGLDAS <- mean(abs(CP9$GLDAS_monthly - CP9$Field_ET_monthly))

spearman_corrGLDAS <- cor.test(CP9$Field_ET_monthly, CP9$GLDAS_monthly, method = "spearman")
print(spearman_corrGLDAS)
correlation_coefficientGLDAS <- spearman_corrGLDAS$estimate

# FLDAS
kge_valueFLDAS <- KGE(sim = CP9$FLDAS_monthly, obs = CP9$Field_ET_monthly)
rmse_valueFLDAS <- rmse(actual = CP9$Field_ET_monthly, predicted = CP9$FLDAS_monthly)
nse_valueFLDAS <- NSE(sim = CP9$FLDAS_monthly, obs = CP9$Field_ET_monthly)
rsr_valueFLDAS <- rmse_valueFLDAS / sd(CP9$Field_ET_monthly)

biasFLDAS <- mean(CP9$FLDAS_monthly - CP9$Field_ET_monthly)
pbiasFLDAS <- 100 * sum(CP9$FLDAS_monthly - CP9$Field_ET_monthly) / sum(CP9$Field_ET_monthly)
maeFLDAS <- mean(abs(CP9$FLDAS_monthly - CP9$Field_ET_monthly))

spearman_corrFLDAS <- cor.test(CP9$Field_ET_monthly, CP9$FLDAS_monthly, method = "spearman")
print(spearman_corrFLDAS)
correlation_coefficientFLDAS <- spearman_corrFLDAS$estimate

# PTJPL
kge_valuePTjpl <- KGE(sim = CP9$PTjpl_monthly, obs = CP9$Field_ET_monthly)
rmse_valuePTjpl <- rmse(actual = CP9$Field_ET_monthly[!is.na(CP9$Field_ET_monthly) & !is.na(CP9$PTjpl_monthly)], 
                        predicted = CP9$PTjpl_monthly[!is.na(CP9$Field_ET_monthly) & !is.na(CP9$PTjpl_monthly)])
nse_valuePTjpl <- NSE(sim = CP9$PTjpl_monthly, obs = CP9$Field_ET_monthly)
rsr_valuePTjpl <- rmse_valuePTjpl / sd(CP9$Field_ET_monthly, na.rm = TRUE)

biasPTjpl <- mean(CP9$PTjpl_monthly - CP9$Field_ET_monthly, na.rm = TRUE)
pbiasPTjpl <- 100 * sum(CP9$PTjpl_monthly - CP9$Field_ET_monthly,na.rm = TRUE) / sum(CP9$Field_ET_monthly, na.rm = TRUE)
maePTjpl <- mean(abs(CP9$PTjpl_monthly - CP9$Field_ET_monthly), na.rm = TRUE)

spearman_corrPTjpl <- cor.test(CP9$Field_ET_monthly, CP9$PTjpl_monthly, method = "spearman")
print(spearman_corrPTjpl)
correlation_coefficientPTjpl <- spearman_corrPTjpl$estimate

df_stats <- data.frame(rsr_valueTC, rsr_valueWaPOR, rsr_valueSMAP, rsr_valueFLDAS, rsr_valueGLDAS, rsr_valueMOD16, rsr_valueSSEBop, rsr_valuePTjpl,
                       rmse_valueTC, rmse_valueWaPOR, rmse_valueSMAP, rmse_valueFLDAS, rmse_valueGLDAS, rmse_valueMOD16, rmse_valueSSEBop, rmse_valuePTjpl,
                       pbiasTC, pbiasWaPOR, pbiasSMAP, pbiasFLDAS, pbiasGLDAS, pbiasMOD16, pbiasSSEBop, pbiasPTjpl,
                       biasTC, biasWaPOR, biasSMAP, biasFLDAS, biasGLDAS, biasMOD16, biasSSEBop, biasPTjpl,
                       maeTC, maeWaPOR, maeSMAP, maeFLDAS, maeGLDAS, maeMOD16, maeSSEBop, maePTjpl,
                       nse_valueTC, nse_valueWaPOR, nse_valueSMAP, nse_valueFLDAS, nse_valueGLDAS, nse_valueMOD16, nse_valueSSEBop, nse_valuePTjpl,
                       kge_valueTC, kge_valueWaPOR, kge_valueSMAP, kge_valueFLDAS, kge_valueGLDAS, kge_valueMOD16, kge_valueSSEBop, kge_valuePTjpl,
                       correlation_coefficientTC, correlation_coefficientWaPOR, correlation_coefficientSMAP, correlation_coefficientFLDAS, correlation_coefficientGLDAS, correlation_coefficientMOD16, correlation_coefficientSSEBop, correlation_coefficientPTjpl)

# Reshape the data frame from wide to long format
df_long <- df_stats %>%
  pivot_longer(
    cols = everything(),
    names_to = c("metric", "site"),  # Separate columns for metric names and sites
    names_pattern = "^(pbias|nse_value|kge_value|correlation_coefficient)(.*)" # Specify explicit metric names
  ) %>%
  pivot_wider(
    names_from = metric,             # Separate each metric into its own column
    values_from = value
  )

# View the reshaped data
head(df_long)
setwd("C:\\Users\\Liam\\OneDrive - Stellenbosch University\\ET_Data\\Stats")
write.csv(df_stats, file = "Indv_Seasonal_CP9.csv", row.names = FALSE)

###################################################################################################
####################################################################################################
# EZU
# TC 
kge_valueTC <- KGE(sim = EZU$TC_monthly, obs = EZU$Field_ET_monthly)
rmse_valueTC <- rmse(actual = EZU$Field_ET_monthly, predicted = EZU$TC_monthly)
nse_valueTC <- NSE(sim = EZU$TC_monthly, obs = EZU$Field_ET_monthly)
rsr_valueTC <- rmse_valueTC / sd(EZU$Field_ET_monthly)

biasTC <- mean(EZU$TC_monthly - EZU$Field_ET_monthly)
pbiasTC <- 100 * sum(EZU$TC_monthly - EZU$Field_ET_monthly) / sum(EZU$Field_ET_monthly)
maeTC <- mean(abs(EZU$TC_monthly - EZU$Field_ET_monthly))

spearman_corrTC <- cor.test(EZU$Field_ET_monthly, EZU$TC_monthly, method = "spearman")
print(spearman_corrTC)
correlation_coefficientTC <- spearman_corrTC$estimate

# SMAP
kge_valueSMAP <- KGE(sim = EZU$SMAP_monthly, obs = EZU$Field_ET_monthly)
rmse_valueSMAP <- rmse(actual = EZU$Field_ET_monthly, predicted = EZU$SMAP_monthly)
nse_valueSMAP <- NSE(sim = EZU$SMAP_monthly, obs = EZU$Field_ET_monthly)
rsr_valueSMAP <- rmse_valueSMAP / sd(EZU$Field_ET_monthly)

biasSMAP <- mean(EZU$SMAP_monthly - EZU$Field_ET_monthly)
pbiasSMAP <- 100 * sum(EZU$SMAP_monthly - EZU$Field_ET_monthly) / sum(EZU$Field_ET_monthly)
maeSMAP <- mean(abs(EZU$SMAP_monthly - EZU$Field_ET_monthly))

spearman_corrSMAP <- cor.test(EZU$Field_ET_monthly, EZU$SMAP_monthly, method = "spearman")
print(spearman_corrSMAP)
correlation_coefficientSMAP <- spearman_corrSMAP$estimate

# SSEBop
kge_valueSSEBop <- KGE(sim = EZU$SSEBop_monthly, obs = EZU$Field_ET_monthly)
rmse_valueSSEBop <- rmse(actual = EZU$Field_ET_monthly[!is.na(EZU$Field_ET_monthly) & !is.na(EZU$SSEBop_monthly)], 
                         predicted = EZU$SSEBop_monthly[!is.na(EZU$Field_ET_monthly) & !is.na(EZU$SSEBop_monthly)])
nse_valueSSEBop <- NSE(sim = EZU$SSEBop_monthly, obs = EZU$Field_ET_monthly)
rsr_valueSSEBop <- rmse_valueSSEBop / sd(EZU$Field_ET_monthly, na.rm = TRUE)

biasSSEBop <- mean(EZU$SSEBop_monthly - EZU$Field_ET_monthly, na.rm = TRUE)
pbiasSSEBop <- 100 * sum(EZU$SSEBop_monthly - EZU$Field_ET_monthly, na.rm = TRUE) / sum(EZU$Field_ET_monthly, na.rm = TRUE)
maeSSEBop <- mean(abs(EZU$SSEBop_monthly - EZU$Field_ET_monthly), na.rm = TRUE)

spearman_corrSSEBop <- cor.test(EZU$Field_ET_monthly, EZU$SSEBop_monthly, method = "spearman")
print(spearman_corrSSEBop)
correlation_coefficientSSEBop <- spearman_corrSSEBop$estimate

# WAPOR
kge_valueWaPOR <- KGE(sim = EZU$WaPOR_monthly, obs = EZU$Field_ET_monthly)
rmse_valueWaPOR <- rmse(actual = EZU$Field_ET_monthly[!is.na(EZU$Field_ET_monthly) & !is.na(EZU$WaPOR_monthly)],
                        predicted = EZU$WaPOR_monthly[!is.na(EZU$Field_ET_monthly) & !is.na(EZU$WaPOR_monthly)])
nse_valueWaPOR <- NSE(sim = EZU$WaPOR_monthly, obs = EZU$Field_ET_monthly)
rsr_valueWaPOR <- rmse_valueWaPOR / sd(EZU$Field_ET_monthly, na.rm = TRUE)

biasWaPOR <- mean(EZU$WaPOR_monthly - EZU$Field_ET_monthly, na.rm = TRUE)
pbiasWaPOR <- 100 * sum(EZU$WaPOR_monthly - EZU$Field_ET_monthly, na.rm = TRUE) / sum(EZU$Field_ET_monthly, na.rm = TRUE)
maeWaPOR <- mean(abs(EZU$WaPOR_monthly - EZU$Field_ET_monthly), na.rm = TRUE)

spearman_corrWaPOR <- cor.test(EZU$Field_ET_monthly, EZU$WaPOR_monthly, method = "spearman")
print(spearman_corrWaPOR)
correlation_coefficientWaPOR <- spearman_corrWaPOR$estimate

# MOD16
kge_valueMOD16 <- KGE(sim = EZU$MOD16_monthly, obs = EZU$Field_ET_monthly)
rmse_valueMOD16 <- rmse(actual = EZU$Field_ET_monthly, predicted = EZU$MOD16_monthly)
nse_valueMOD16 <- NSE(sim = EZU$MOD16_monthly, obs = EZU$Field_ET_monthly)
rsr_valueMOD16 <- rmse_valueMOD16 / sd(EZU$Field_ET_monthly)

biasMOD16 <- mean(EZU$MOD16_monthly - EZU$Field_ET_monthly)
pbiasMOD16 <- 100 * sum(EZU$MOD16_monthly - EZU$Field_ET_monthly) / sum(EZU$Field_ET_monthly)
maeMOD16 <- mean(abs(EZU$MOD16_monthly - EZU$Field_ET_monthly))

spearman_corrMOD16 <- cor.test(EZU$Field_ET_monthly, EZU$MOD16_monthly, method = "spearman")
print(spearman_corrMOD16)
correlation_coefficientMOD16 <- spearman_corrMOD16$estimate

# GLDAS
kge_valueGLDAS <- KGE(sim = EZU$GLDAS_monthly, obs = EZU$Field_ET_monthly)
rmse_valueGLDAS <- rmse(actual = EZU$Field_ET_monthly, predicted = EZU$GLDAS_monthly)
nse_valueGLDAS <- NSE(sim = EZU$GLDAS_monthly, obs = EZU$Field_ET_monthly)
rsr_valueGLDAS <- rmse_valueGLDAS / sd(EZU$Field_ET_monthly)

biasGLDAS <- mean(EZU$GLDAS_monthly - EZU$Field_ET_monthly)
pbiasGLDAS <- 100 * sum(EZU$GLDAS_monthly - EZU$Field_ET_monthly) / sum(EZU$Field_ET_monthly)
maeGLDAS <- mean(abs(EZU$GLDAS_monthly - EZU$Field_ET_monthly))

spearman_corrGLDAS <- cor.test(EZU$Field_ET_monthly, EZU$GLDAS_monthly, method = "spearman")
print(spearman_corrGLDAS)
correlation_coefficientGLDAS <- spearman_corrGLDAS$estimate

# FLDAS
kge_valueFLDAS <- KGE(sim = EZU$FLDAS_monthly, obs = EZU$Field_ET_monthly)
rmse_valueFLDAS <- rmse(actual = EZU$Field_ET_monthly, predicted = EZU$FLDAS_monthly)
nse_valueFLDAS <- NSE(sim = EZU$FLDAS_monthly, obs = EZU$Field_ET_monthly)
rsr_valueFLDAS <- rmse_valueFLDAS / sd(EZU$Field_ET_monthly)

biasFLDAS <- mean(EZU$FLDAS_monthly - EZU$Field_ET_monthly)
pbiasFLDAS <- 100 * sum(EZU$FLDAS_monthly - EZU$Field_ET_monthly) / sum(EZU$Field_ET_monthly)
maeFLDAS <- mean(abs(EZU$FLDAS_monthly - EZU$Field_ET_monthly))

spearman_corrFLDAS <- cor.test(EZU$Field_ET_monthly, EZU$FLDAS_monthly, method = "spearman")
print(spearman_corrFLDAS)
correlation_coefficientFLDAS <- spearman_corrFLDAS$estimate

df_stats <- data.frame(rsr_valueTC, rsr_valueWaPOR, rsr_valueSMAP, rsr_valueFLDAS, rsr_valueGLDAS, rsr_valueMOD16, rsr_valueSSEBop, 
                       rmse_valueTC, rmse_valueWaPOR, rmse_valueSMAP, rmse_valueFLDAS, rmse_valueGLDAS, rmse_valueMOD16, rmse_valueSSEBop, 
                       pbiasTC, pbiasWaPOR, pbiasSMAP, pbiasFLDAS, pbiasGLDAS, pbiasMOD16, pbiasSSEBop, 
                       biasTC, biasWaPOR, biasSMAP, biasFLDAS, biasGLDAS, biasMOD16, biasSSEBop, 
                       maeTC, maeWaPOR, maeSMAP, maeFLDAS, maeGLDAS, maeMOD16, maeSSEBop, 
                       nse_valueTC, nse_valueWaPOR, nse_valueSMAP, nse_valueFLDAS, nse_valueGLDAS, nse_valueMOD16, nse_valueSSEBop, 
                       kge_valueTC, kge_valueWaPOR, kge_valueSMAP, kge_valueFLDAS, kge_valueGLDAS, kge_valueMOD16, kge_valueSSEBop, 
                       correlation_coefficientTC, correlation_coefficientWaPOR, correlation_coefficientSMAP, correlation_coefficientFLDAS, correlation_coefficientGLDAS, correlation_coefficientMOD16, correlation_coefficientSSEBop)

# Reshape the data frame from wide to long format
df_long <- df_stats %>%
  pivot_longer(
    cols = everything(),
    names_to = c("metric", "site"),  # Separate columns for metric names and sites
    names_pattern = "^(pbias|nse_value|kge_value|correlation_coefficient)(.*)" # Specify explicit metric names
  ) %>%
  pivot_wider(
    names_from = metric,             # Separate each metric into its own column
    values_from = value
  )

# View the reshaped data
head(df_long)
setwd("C:\\Users\\Liam\\OneDrive - Stellenbosch University\\ET_Data\\Stats")
write.csv(df_stats, file = "Indv_Seasonal_EZU.csv", row.names = FALSE)

###################################################################################################
####################################################################################################
# EW1
# TC 
kge_valueTC <- KGE(sim = EW1$TC_monthly, obs = EW1$Field_ET_monthly)
rmse_valueTC <- rmse(actual = EW1$Field_ET_monthly, predicted = EW1$TC_monthly)
nse_valueTC <- NSE(sim = EW1$TC_monthly, obs = EW1$Field_ET_monthly)
rsr_valueTC <- rmse_valueTC / sd(EW1$Field_ET_monthly)

biasTC <- mean(EW1$TC_monthly - EW1$Field_ET_monthly)
pbiasTC <- 100 * sum(EW1$TC_monthly - EW1$Field_ET_monthly) / sum(EW1$Field_ET_monthly)
maeTC <- mean(abs(EW1$TC_monthly - EW1$Field_ET_monthly))

spearman_corrTC <- cor.test(EW1$Field_ET_monthly, EW1$TC_monthly, method = "spearman")
print(spearman_corrTC)
correlation_coefficientTC <- spearman_corrTC$estimate

# SMAP
kge_valueSMAP <- KGE(sim = EW1$SMAP_monthly, obs = EW1$Field_ET_monthly)
rmse_valueSMAP <- rmse(actual = EW1$Field_ET_monthly, predicted = EW1$SMAP_monthly)
nse_valueSMAP <- NSE(sim = EW1$SMAP_monthly, obs = EW1$Field_ET_monthly)
rsr_valueSMAP <- rmse_valueSMAP / sd(EW1$Field_ET_monthly)

biasSMAP <- mean(EW1$SMAP_monthly - EW1$Field_ET_monthly)
pbiasSMAP <- 100 * sum(EW1$SMAP_monthly - EW1$Field_ET_monthly) / sum(EW1$Field_ET_monthly)
maeSMAP <- mean(abs(EW1$SMAP_monthly - EW1$Field_ET_monthly))

spearman_corrSMAP <- cor.test(EW1$Field_ET_monthly, EW1$SMAP_monthly, method = "spearman")
print(spearman_corrSMAP)
correlation_coefficientSMAP <- spearman_corrSMAP$estimate

# SSEBop
kge_valueSSEBop <- KGE(sim = EW1$SSEBop_monthly, obs = EW1$Field_ET_monthly)
rmse_valueSSEBop <- rmse(actual = EW1$Field_ET_monthly[!is.na(EW1$Field_ET_monthly) & !is.na(EW1$SSEBop_monthly)], 
                         predicted = EW1$SSEBop_monthly[!is.na(EW1$Field_ET_monthly) & !is.na(EW1$SSEBop_monthly)])
nse_valueSSEBop <- NSE(sim = EW1$SSEBop_monthly, obs = EW1$Field_ET_monthly)
rsr_valueSSEBop <- rmse_valueSSEBop / sd(EW1$Field_ET_monthly, na.rm = TRUE)

biasSSEBop <- mean(EW1$SSEBop_monthly - EW1$Field_ET_monthly, na.rm = TRUE)
pbiasSSEBop <- 100 * sum(EW1$SSEBop_monthly - EW1$Field_ET_monthly, na.rm = TRUE) / sum(EW1$Field_ET_monthly, na.rm = TRUE)
maeSSEBop <- mean(abs(EW1$SSEBop_monthly - EW1$Field_ET_monthly), na.rm = TRUE)

spearman_corrSSEBop <- cor.test(EW1$Field_ET_monthly, EW1$SSEBop_monthly, method = "spearman")
print(spearman_corrSSEBop)
correlation_coefficientSSEBop <- spearman_corrSSEBop$estimate

# WAPOR
kge_valueWaPOR <- KGE(sim = EW1$WaPOR_monthly, obs = EW1$Field_ET_monthly)
rmse_valueWaPOR <- rmse(actual = EW1$Field_ET_monthly[!is.na(EW1$Field_ET_monthly) & !is.na(EW1$WaPOR_monthly)],
                        predicted = EW1$WaPOR_monthly[!is.na(EW1$Field_ET_monthly) & !is.na(EW1$WaPOR_monthly)])
nse_valueWaPOR <- NSE(sim = EW1$WaPOR_monthly, obs = EW1$Field_ET_monthly)
rsr_valueWaPOR <- rmse_valueWaPOR / sd(EW1$Field_ET_monthly, na.rm = TRUE)

biasWaPOR <- mean(EW1$WaPOR_monthly - EW1$Field_ET_monthly, na.rm = TRUE)
pbiasWaPOR <- 100 * sum(EW1$WaPOR_monthly - EW1$Field_ET_monthly, na.rm = TRUE) / sum(EW1$Field_ET_monthly, na.rm = TRUE)
maeWaPOR <- mean(abs(EW1$WaPOR_monthly - EW1$Field_ET_monthly), na.rm = TRUE)

spearman_corrWaPOR <- cor.test(EW1$Field_ET_monthly, EW1$WaPOR_monthly, method = "spearman")
print(spearman_corrWaPOR)
correlation_coefficientWaPOR <- spearman_corrWaPOR$estimate

# MOD16
kge_valueMOD16 <- KGE(sim = EW1$MOD16_monthly, obs = EW1$Field_ET_monthly)
rmse_valueMOD16 <- rmse(actual = EW1$Field_ET_monthly, predicted = EW1$MOD16_monthly)
nse_valueMOD16 <- NSE(sim = EW1$MOD16_monthly, obs = EW1$Field_ET_monthly)
rsr_valueMOD16 <- rmse_valueMOD16 / sd(EW1$Field_ET_monthly)

biasMOD16 <- mean(EW1$MOD16_monthly - EW1$Field_ET_monthly)
pbiasMOD16 <- 100 * sum(EW1$MOD16_monthly - EW1$Field_ET_monthly) / sum(EW1$Field_ET_monthly)
maeMOD16 <- mean(abs(EW1$MOD16_monthly - EW1$Field_ET_monthly))

spearman_corrMOD16 <- cor.test(EW1$Field_ET_monthly, EW1$MOD16_monthly, method = "spearman")
print(spearman_corrMOD16)
correlation_coefficientMOD16 <- spearman_corrMOD16$estimate

# GLDAS
kge_valueGLDAS <- KGE(sim = EW1$GLDAS_monthly, obs = EW1$Field_ET_monthly)
rmse_valueGLDAS <- rmse(actual = EW1$Field_ET_monthly, predicted = EW1$GLDAS_monthly)
nse_valueGLDAS <- NSE(sim = EW1$GLDAS_monthly, obs = EW1$Field_ET_monthly)
rsr_valueGLDAS <- rmse_valueGLDAS / sd(EW1$Field_ET_monthly)

biasGLDAS <- mean(EW1$GLDAS_monthly - EW1$Field_ET_monthly)
pbiasGLDAS <- 100 * sum(EW1$GLDAS_monthly - EW1$Field_ET_monthly) / sum(EW1$Field_ET_monthly)
maeGLDAS <- mean(abs(EW1$GLDAS_monthly - EW1$Field_ET_monthly))

spearman_corrGLDAS <- cor.test(EW1$Field_ET_monthly, EW1$GLDAS_monthly, method = "spearman")
print(spearman_corrGLDAS)
correlation_coefficientGLDAS <- spearman_corrGLDAS$estimate

# FLDAS
kge_valueFLDAS <- KGE(sim = EW1$FLDAS_monthly, obs = EW1$Field_ET_monthly)
rmse_valueFLDAS <- rmse(actual = EW1$Field_ET_monthly, predicted = EW1$FLDAS_monthly)
nse_valueFLDAS <- NSE(sim = EW1$FLDAS_monthly, obs = EW1$Field_ET_monthly)
rsr_valueFLDAS <- rmse_valueFLDAS / sd(EW1$Field_ET_monthly)

biasFLDAS <- mean(EW1$FLDAS_monthly - EW1$Field_ET_monthly)
pbiasFLDAS <- 100 * sum(EW1$FLDAS_monthly - EW1$Field_ET_monthly) / sum(EW1$Field_ET_monthly)
maeFLDAS <- mean(abs(EW1$FLDAS_monthly - EW1$Field_ET_monthly))

spearman_corrFLDAS <- cor.test(EW1$Field_ET_monthly, EW1$FLDAS_monthly, method = "spearman")
print(spearman_corrFLDAS)
correlation_coefficientFLDAS <- spearman_corrFLDAS$estimate

# PTJPL
kge_valuePTjpl <- KGE(sim = EW1$PTjpl_monthly, obs = EW1$Field_ET_monthly)
rmse_valuePTjpl <- rmse(actual = EW1$Field_ET_monthly[!is.na(EW1$Field_ET_monthly) & !is.na(EW1$PTjpl_monthly)], 
                        predicted = EW1$PTjpl_monthly[!is.na(EW1$Field_ET_monthly) & !is.na(EW1$PTjpl_monthly)])
nse_valuePTjpl <- NSE(sim = EW1$PTjpl_monthly, obs = EW1$Field_ET_monthly)
rsr_valuePTjpl <- rmse_valuePTjpl / sd(EW1$Field_ET_monthly, na.rm = TRUE)

biasPTjpl <- mean(EW1$PTjpl_monthly - EW1$Field_ET_monthly, na.rm = TRUE)
pbiasPTjpl <- 100 * sum(EW1$PTjpl_monthly - EW1$Field_ET_monthly,na.rm = TRUE) / sum(EW1$Field_ET_monthly, na.rm = TRUE)
maePTjpl <- mean(abs(EW1$PTjpl_monthly - EW1$Field_ET_monthly), na.rm = TRUE)

spearman_corrPTjpl <- cor.test(EW1$Field_ET_monthly, EW1$PTjpl_monthly, method = "spearman")
print(spearman_corrPTjpl)
correlation_coefficientPTjpl <- spearman_corrPTjpl$estimate

df_stats <- data.frame(rsr_valueTC, rsr_valueWaPOR, rsr_valueSMAP, rsr_valueFLDAS, rsr_valueGLDAS, rsr_valueMOD16, rsr_valueSSEBop, rsr_valuePTjpl,
                       rmse_valueTC, rmse_valueWaPOR, rmse_valueSMAP, rmse_valueFLDAS, rmse_valueGLDAS, rmse_valueMOD16, rmse_valueSSEBop, rmse_valuePTjpl,
                       pbiasTC, pbiasWaPOR, pbiasSMAP, pbiasFLDAS, pbiasGLDAS, pbiasMOD16, pbiasSSEBop, pbiasPTjpl,
                       biasTC, biasWaPOR, biasSMAP, biasFLDAS, biasGLDAS, biasMOD16, biasSSEBop, biasPTjpl,
                       maeTC, maeWaPOR, maeSMAP, maeFLDAS, maeGLDAS, maeMOD16, maeSSEBop, maePTjpl,
                       nse_valueTC, nse_valueWaPOR, nse_valueSMAP, nse_valueFLDAS, nse_valueGLDAS, nse_valueMOD16, nse_valueSSEBop, nse_valuePTjpl,
                       kge_valueTC, kge_valueWaPOR, kge_valueSMAP, kge_valueFLDAS, kge_valueGLDAS, kge_valueMOD16, kge_valueSSEBop, kge_valuePTjpl,
                       correlation_coefficientTC, correlation_coefficientWaPOR, correlation_coefficientSMAP, correlation_coefficientFLDAS, correlation_coefficientGLDAS, correlation_coefficientMOD16, correlation_coefficientSSEBop, correlation_coefficientPTjpl)

# Reshape the data frame from wide to long format
df_long <- df_stats %>%
  pivot_longer(
    cols = everything(),
    names_to = c("metric", "site"),  # Separate columns for metric names and sites
    names_pattern = "^(pbias|nse_value|kge_value|correlation_coefficient)(.*)" # Specify explicit metric names
  ) %>%
  pivot_wider(
    names_from = metric,             # Separate each metric into its own column
    values_from = value
  )

# View the reshaped data
head(df_long)
setwd("C:\\Users\\Liam\\OneDrive - Stellenbosch University\\ET_Data\\Stats")
write.csv(df_stats, file = "Indv_Seasonal_EW1.csv", row.names = FALSE)

###################################################################################################
####################################################################################################
# EW2
# TC 
kge_valueTC <- KGE(sim = EW2$TC_monthly, obs = EW2$Field_ET_monthly)
rmse_valueTC <- rmse(actual = EW2$Field_ET_monthly, predicted = EW2$TC_monthly)
nse_valueTC <- NSE(sim = EW2$TC_monthly, obs = EW2$Field_ET_monthly)
rsr_valueTC <- rmse_valueTC / sd(EW2$Field_ET_monthly)

biasTC <- mean(EW2$TC_monthly - EW2$Field_ET_monthly)
pbiasTC <- 100 * sum(EW2$TC_monthly - EW2$Field_ET_monthly) / sum(EW2$Field_ET_monthly)
maeTC <- mean(abs(EW2$TC_monthly - EW2$Field_ET_monthly))

spearman_corrTC <- cor.test(EW2$Field_ET_monthly, EW2$TC_monthly, method = "spearman")
print(spearman_corrTC)
correlation_coefficientTC <- spearman_corrTC$estimate

# SMAP
kge_valueSMAP <- KGE(sim = EW2$SMAP_monthly, obs = EW2$Field_ET_monthly)
rmse_valueSMAP <- rmse(actual = EW2$Field_ET_monthly, predicted = EW2$SMAP_monthly)
nse_valueSMAP <- NSE(sim = EW2$SMAP_monthly, obs = EW2$Field_ET_monthly)
rsr_valueSMAP <- rmse_valueSMAP / sd(EW2$Field_ET_monthly)

biasSMAP <- mean(EW2$SMAP_monthly - EW2$Field_ET_monthly)
pbiasSMAP <- 100 * sum(EW2$SMAP_monthly - EW2$Field_ET_monthly) / sum(EW2$Field_ET_monthly)
maeSMAP <- mean(abs(EW2$SMAP_monthly - EW2$Field_ET_monthly))

spearman_corrSMAP <- cor.test(EW2$Field_ET_monthly, EW2$SMAP_monthly, method = "spearman")
print(spearman_corrSMAP)
correlation_coefficientSMAP <- spearman_corrSMAP$estimate

# SSEBop
kge_valueSSEBop <- KGE(sim = EW2$SSEBop_monthly, obs = EW2$Field_ET_monthly)
rmse_valueSSEBop <- rmse(actual = EW2$Field_ET_monthly[!is.na(EW2$Field_ET_monthly) & !is.na(EW2$SSEBop_monthly)], 
                         predicted = EW2$SSEBop_monthly[!is.na(EW2$Field_ET_monthly) & !is.na(EW2$SSEBop_monthly)])
nse_valueSSEBop <- NSE(sim = EW2$SSEBop_monthly, obs = EW2$Field_ET_monthly)
rsr_valueSSEBop <- rmse_valueSSEBop / sd(EW2$Field_ET_monthly, na.rm = TRUE)

biasSSEBop <- mean(EW2$SSEBop_monthly - EW2$Field_ET_monthly, na.rm = TRUE)
pbiasSSEBop <- 100 * sum(EW2$SSEBop_monthly - EW2$Field_ET_monthly, na.rm = TRUE) / sum(EW2$Field_ET_monthly, na.rm = TRUE)
maeSSEBop <- mean(abs(EW2$SSEBop_monthly - EW2$Field_ET_monthly), na.rm = TRUE)

spearman_corrSSEBop <- cor.test(EW2$Field_ET_monthly, EW2$SSEBop_monthly, method = "spearman")
print(spearman_corrSSEBop)
correlation_coefficientSSEBop <- spearman_corrSSEBop$estimate

# WAPOR
kge_valueWaPOR <- KGE(sim = EW2$WaPOR_monthly, obs = EW2$Field_ET_monthly)
rmse_valueWaPOR <- rmse(actual = EW2$Field_ET_monthly[!is.na(EW2$Field_ET_monthly) & !is.na(EW2$WaPOR_monthly)],
                        predicted = EW2$WaPOR_monthly[!is.na(EW2$Field_ET_monthly) & !is.na(EW2$WaPOR_monthly)])
nse_valueWaPOR <- NSE(sim = EW2$WaPOR_monthly, obs = EW2$Field_ET_monthly)
rsr_valueWaPOR <- rmse_valueWaPOR / sd(EW2$Field_ET_monthly, na.rm = TRUE)

biasWaPOR <- mean(EW2$WaPOR_monthly - EW2$Field_ET_monthly, na.rm = TRUE)
pbiasWaPOR <- 100 * sum(EW2$WaPOR_monthly - EW2$Field_ET_monthly, na.rm = TRUE) / sum(EW2$Field_ET_monthly, na.rm = TRUE)
maeWaPOR <- mean(abs(EW2$WaPOR_monthly - EW2$Field_ET_monthly), na.rm = TRUE)

spearman_corrWaPOR <- cor.test(EW2$Field_ET_monthly, EW2$WaPOR_monthly, method = "spearman")
print(spearman_corrWaPOR)
correlation_coefficientWaPOR <- spearman_corrWaPOR$estimate

# MOD16
kge_valueMOD16 <- KGE(sim = EW2$MOD16_monthly, obs = EW2$Field_ET_monthly)
rmse_valueMOD16 <- rmse(actual = EW2$Field_ET_monthly, predicted = EW2$MOD16_monthly)
nse_valueMOD16 <- NSE(sim = EW2$MOD16_monthly, obs = EW2$Field_ET_monthly)
rsr_valueMOD16 <- rmse_valueMOD16 / sd(EW2$Field_ET_monthly)

biasMOD16 <- mean(EW2$MOD16_monthly - EW2$Field_ET_monthly)
pbiasMOD16 <- 100 * sum(EW2$MOD16_monthly - EW2$Field_ET_monthly) / sum(EW2$Field_ET_monthly)
maeMOD16 <- mean(abs(EW2$MOD16_monthly - EW2$Field_ET_monthly))

spearman_corrMOD16 <- cor.test(EW2$Field_ET_monthly, EW2$MOD16_monthly, method = "spearman")
print(spearman_corrMOD16)
correlation_coefficientMOD16 <- spearman_corrMOD16$estimate

# GLDAS
kge_valueGLDAS <- KGE(sim = EW2$GLDAS_monthly, obs = EW2$Field_ET_monthly)
rmse_valueGLDAS <- rmse(actual = EW2$Field_ET_monthly, predicted = EW2$GLDAS_monthly)
nse_valueGLDAS <- NSE(sim = EW2$GLDAS_monthly, obs = EW2$Field_ET_monthly)
rsr_valueGLDAS <- rmse_valueGLDAS / sd(EW2$Field_ET_monthly)

biasGLDAS <- mean(EW2$GLDAS_monthly - EW2$Field_ET_monthly)
pbiasGLDAS <- 100 * sum(EW2$GLDAS_monthly - EW2$Field_ET_monthly) / sum(EW2$Field_ET_monthly)
maeGLDAS <- mean(abs(EW2$GLDAS_monthly - EW2$Field_ET_monthly))

spearman_corrGLDAS <- cor.test(EW2$Field_ET_monthly, EW2$GLDAS_monthly, method = "spearman")
print(spearman_corrGLDAS)
correlation_coefficientGLDAS <- spearman_corrGLDAS$estimate

# FLDAS
kge_valueFLDAS <- KGE(sim = EW2$FLDAS_monthly, obs = EW2$Field_ET_monthly)
rmse_valueFLDAS <- rmse(actual = EW2$Field_ET_monthly, predicted = EW2$FLDAS_monthly)
nse_valueFLDAS <- NSE(sim = EW2$FLDAS_monthly, obs = EW2$Field_ET_monthly)
rsr_valueFLDAS <- rmse_valueFLDAS / sd(EW2$Field_ET_monthly)

biasFLDAS <- mean(EW2$FLDAS_monthly - EW2$Field_ET_monthly)
pbiasFLDAS <- 100 * sum(EW2$FLDAS_monthly - EW2$Field_ET_monthly) / sum(EW2$Field_ET_monthly)
maeFLDAS <- mean(abs(EW2$FLDAS_monthly - EW2$Field_ET_monthly))

spearman_corrFLDAS <- cor.test(EW2$Field_ET_monthly, EW2$FLDAS_monthly, method = "spearman")
print(spearman_corrFLDAS)
correlation_coefficientFLDAS <- spearman_corrFLDAS$estimate

# PTJPL
kge_valuePTjpl <- KGE(sim = EW2$PTjpl_monthly, obs = EW2$Field_ET_monthly)
rmse_valuePTjpl <- rmse(actual = EW2$Field_ET_monthly[!is.na(EW2$Field_ET_monthly) & !is.na(EW2$PTjpl_monthly)], 
                        predicted = EW2$PTjpl_monthly[!is.na(EW2$Field_ET_monthly) & !is.na(EW2$PTjpl_monthly)])
nse_valuePTjpl <- NSE(sim = EW2$PTjpl_monthly, obs = EW2$Field_ET_monthly)
rsr_valuePTjpl <- rmse_valuePTjpl / sd(EW2$Field_ET_monthly, na.rm = TRUE)

biasPTjpl <- mean(EW2$PTjpl_monthly - EW2$Field_ET_monthly, na.rm = TRUE)
pbiasPTjpl <- 100 * sum(EW2$PTjpl_monthly - EW2$Field_ET_monthly,na.rm = TRUE) / sum(EW2$Field_ET_monthly, na.rm = TRUE)
maePTjpl <- mean(abs(EW2$PTjpl_monthly - EW2$Field_ET_monthly), na.rm = TRUE)

spearman_corrPTjpl <- cor.test(EW2$Field_ET_monthly, EW2$PTjpl_monthly, method = "spearman")
print(spearman_corrPTjpl)
correlation_coefficientPTjpl <- spearman_corrPTjpl$estimate

df_stats <- data.frame(rsr_valueTC, rsr_valueWaPOR, rsr_valueSMAP, rsr_valueFLDAS, rsr_valueGLDAS, rsr_valueMOD16, rsr_valueSSEBop, rsr_valuePTjpl,
                       rmse_valueTC, rmse_valueWaPOR, rmse_valueSMAP, rmse_valueFLDAS, rmse_valueGLDAS, rmse_valueMOD16, rmse_valueSSEBop, rmse_valuePTjpl,
                       pbiasTC, pbiasWaPOR, pbiasSMAP, pbiasFLDAS, pbiasGLDAS, pbiasMOD16, pbiasSSEBop, pbiasPTjpl,
                       biasTC, biasWaPOR, biasSMAP, biasFLDAS, biasGLDAS, biasMOD16, biasSSEBop, biasPTjpl,
                       maeTC, maeWaPOR, maeSMAP, maeFLDAS, maeGLDAS, maeMOD16, maeSSEBop, maePTjpl,
                       nse_valueTC, nse_valueWaPOR, nse_valueSMAP, nse_valueFLDAS, nse_valueGLDAS, nse_valueMOD16, nse_valueSSEBop, nse_valuePTjpl,
                       kge_valueTC, kge_valueWaPOR, kge_valueSMAP, kge_valueFLDAS, kge_valueGLDAS, kge_valueMOD16, kge_valueSSEBop, kge_valuePTjpl,
                       correlation_coefficientTC, correlation_coefficientWaPOR, correlation_coefficientSMAP, correlation_coefficientFLDAS, correlation_coefficientGLDAS, correlation_coefficientMOD16, correlation_coefficientSSEBop, correlation_coefficientPTjpl)

# Reshape the data frame from wide to long format
df_long <- df_stats %>%
  pivot_longer(
    cols = everything(),
    names_to = c("metric", "site"),  # Separate columns for metric names and sites
    names_pattern = "^(pbias|nse_value|kge_value|correlation_coefficient)(.*)" # Specify explicit metric names
  ) %>%
  pivot_wider(
    names_from = metric,             # Separate each metric into its own column
    values_from = value
  )

# View the reshaped data
head(df_long)
setwd("C:\\Users\\Liam\\OneDrive - Stellenbosch University\\ET_Data\\Stats")
write.csv(df_stats, file = "Indv_Seasonal_EW2.csv", row.names = FALSE)





###################################################################################################
####################################################################################################
# JHK
# TC 
kge_valueTC <- KGE(sim = JHK$TC_monthly, obs = JHK$Field_ET_monthly)
rmse_valueTC <- rmse(actual = JHK$Field_ET_monthly, predicted = JHK$TC_monthly)
nse_valueTC <- NSE(sim = JHK$TC_monthly, obs = JHK$Field_ET_monthly)
rsr_valueTC <- rmse_valueTC / sd(JHK$Field_ET_monthly)

biasTC <- mean(JHK$TC_monthly - JHK$Field_ET_monthly)
pbiasTC <- 100 * sum(JHK$TC_monthly - JHK$Field_ET_monthly) / sum(JHK$Field_ET_monthly)
maeTC <- mean(abs(JHK$TC_monthly - JHK$Field_ET_monthly))

spearman_corrTC <- cor.test(JHK$Field_ET_monthly, JHK$TC_monthly, method = "spearman")
print(spearman_corrTC)
correlation_coefficientTC <- spearman_corrTC$estimate

# SMAP
kge_valueSMAP <- KGE(sim = JHK$SMAP_monthly, obs = JHK$Field_ET_monthly)
rmse_valueSMAP <- rmse(actual = JHK$Field_ET_monthly, predicted = JHK$SMAP_monthly)
nse_valueSMAP <- NSE(sim = JHK$SMAP_monthly, obs = JHK$Field_ET_monthly)
rsr_valueSMAP <- rmse_valueSMAP / sd(JHK$Field_ET_monthly)

biasSMAP <- mean(JHK$SMAP_monthly - JHK$Field_ET_monthly)
pbiasSMAP <- 100 * sum(JHK$SMAP_monthly - JHK$Field_ET_monthly) / sum(JHK$Field_ET_monthly)
maeSMAP <- mean(abs(JHK$SMAP_monthly - JHK$Field_ET_monthly))

spearman_corrSMAP <- cor.test(JHK$Field_ET_monthly, JHK$SMAP_monthly, method = "spearman")
print(spearman_corrSMAP)
correlation_coefficientSMAP <- spearman_corrSMAP$estimate

# SSEBop
kge_valueSSEBop <- KGE(sim = JHK$SSEBop_monthly, obs = JHK$Field_ET_monthly)
rmse_valueSSEBop <- rmse(actual = JHK$Field_ET_monthly[!is.na(JHK$Field_ET_monthly) & !is.na(JHK$SSEBop_monthly)], 
                         predicted = JHK$SSEBop_monthly[!is.na(JHK$Field_ET_monthly) & !is.na(JHK$SSEBop_monthly)])
nse_valueSSEBop <- NSE(sim = JHK$SSEBop_monthly, obs = JHK$Field_ET_monthly)
rsr_valueSSEBop <- rmse_valueSSEBop / sd(JHK$Field_ET_monthly, na.rm = TRUE)

biasSSEBop <- mean(JHK$SSEBop_monthly - JHK$Field_ET_monthly, na.rm = TRUE)
pbiasSSEBop <- 100 * sum(JHK$SSEBop_monthly - JHK$Field_ET_monthly, na.rm = TRUE) / sum(JHK$Field_ET_monthly, na.rm = TRUE)
maeSSEBop <- mean(abs(JHK$SSEBop_monthly - JHK$Field_ET_monthly), na.rm = TRUE)

spearman_corrSSEBop <- cor.test(JHK$Field_ET_monthly, JHK$SSEBop_monthly, method = "spearman")
print(spearman_corrSSEBop)
correlation_coefficientSSEBop <- spearman_corrSSEBop$estimate

# WAPOR
kge_valueWaPOR <- KGE(sim = JHK$WaPOR_monthly, obs = JHK$Field_ET_monthly)
rmse_valueWaPOR <- rmse(actual = JHK$Field_ET_monthly[!is.na(JHK$Field_ET_monthly) & !is.na(JHK$WaPOR_monthly)],
                        predicted = JHK$WaPOR_monthly[!is.na(JHK$Field_ET_monthly) & !is.na(JHK$WaPOR_monthly)])
nse_valueWaPOR <- NSE(sim = JHK$WaPOR_monthly, obs = JHK$Field_ET_monthly)
rsr_valueWaPOR <- rmse_valueWaPOR / sd(JHK$Field_ET_monthly, na.rm = TRUE)

biasWaPOR <- mean(JHK$WaPOR_monthly - JHK$Field_ET_monthly, na.rm = TRUE)
pbiasWaPOR <- 100 * sum(JHK$WaPOR_monthly - JHK$Field_ET_monthly, na.rm = TRUE) / sum(JHK$Field_ET_monthly, na.rm = TRUE)
maeWaPOR <- mean(abs(JHK$WaPOR_monthly - JHK$Field_ET_monthly), na.rm = TRUE)

spearman_corrWaPOR <- cor.test(JHK$Field_ET_monthly, JHK$WaPOR_monthly, method = "spearman")
print(spearman_corrWaPOR)
correlation_coefficientWaPOR <- spearman_corrWaPOR$estimate

# MOD16
kge_valueMOD16 <- KGE(sim = JHK$MOD16_monthly, obs = JHK$Field_ET_monthly)
rmse_valueMOD16 <- rmse(actual = JHK$Field_ET_monthly, predicted = JHK$MOD16_monthly)
nse_valueMOD16 <- NSE(sim = JHK$MOD16_monthly, obs = JHK$Field_ET_monthly)
rsr_valueMOD16 <- rmse_valueMOD16 / sd(JHK$Field_ET_monthly)

biasMOD16 <- mean(JHK$MOD16_monthly - JHK$Field_ET_monthly)
pbiasMOD16 <- 100 * sum(JHK$MOD16_monthly - JHK$Field_ET_monthly) / sum(JHK$Field_ET_monthly)
maeMOD16 <- mean(abs(JHK$MOD16_monthly - JHK$Field_ET_monthly))

spearman_corrMOD16 <- cor.test(JHK$Field_ET_monthly, JHK$MOD16_monthly, method = "spearman")
print(spearman_corrMOD16)
correlation_coefficientMOD16 <- spearman_corrMOD16$estimate

# GLDAS
kge_valueGLDAS <- KGE(sim = JHK$GLDAS_monthly, obs = JHK$Field_ET_monthly)
rmse_valueGLDAS <- rmse(actual = JHK$Field_ET_monthly, predicted = JHK$GLDAS_monthly)
nse_valueGLDAS <- NSE(sim = JHK$GLDAS_monthly, obs = JHK$Field_ET_monthly)
rsr_valueGLDAS <- rmse_valueGLDAS / sd(JHK$Field_ET_monthly)

biasGLDAS <- mean(JHK$GLDAS_monthly - JHK$Field_ET_monthly)
pbiasGLDAS <- 100 * sum(JHK$GLDAS_monthly - JHK$Field_ET_monthly) / sum(JHK$Field_ET_monthly)
maeGLDAS <- mean(abs(JHK$GLDAS_monthly - JHK$Field_ET_monthly))

spearman_corrGLDAS <- cor.test(JHK$Field_ET_monthly, JHK$GLDAS_monthly, method = "spearman")
print(spearman_corrGLDAS)
correlation_coefficientGLDAS <- spearman_corrGLDAS$estimate

# FLDAS
kge_valueFLDAS <- KGE(sim = JHK$FLDAS_monthly, obs = JHK$Field_ET_monthly)
rmse_valueFLDAS <- rmse(actual = JHK$Field_ET_monthly, predicted = JHK$FLDAS_monthly)
nse_valueFLDAS <- NSE(sim = JHK$FLDAS_monthly, obs = JHK$Field_ET_monthly)
rsr_valueFLDAS <- rmse_valueFLDAS / sd(JHK$Field_ET_monthly)

biasFLDAS <- mean(JHK$FLDAS_monthly - JHK$Field_ET_monthly)
pbiasFLDAS <- 100 * sum(JHK$FLDAS_monthly - JHK$Field_ET_monthly) / sum(JHK$Field_ET_monthly)
maeFLDAS <- mean(abs(JHK$FLDAS_monthly - JHK$Field_ET_monthly))

spearman_corrFLDAS <- cor.test(JHK$Field_ET_monthly, JHK$FLDAS_monthly, method = "spearman")
print(spearman_corrFLDAS)
correlation_coefficientFLDAS <- spearman_corrFLDAS$estimate

# PTJPL
kge_valuePTjpl <- KGE(sim = JHK$PTjpl_monthly, obs = JHK$Field_ET_monthly)
rmse_valuePTjpl <- rmse(actual = JHK$Field_ET_monthly[!is.na(JHK$Field_ET_monthly) & !is.na(JHK$PTjpl_monthly)], 
                        predicted = JHK$PTjpl_monthly[!is.na(JHK$Field_ET_monthly) & !is.na(JHK$PTjpl_monthly)])
nse_valuePTjpl <- NSE(sim = JHK$PTjpl_monthly, obs = JHK$Field_ET_monthly)
rsr_valuePTjpl <- rmse_valuePTjpl / sd(JHK$Field_ET_monthly, na.rm = TRUE)

biasPTjpl <- mean(JHK$PTjpl_monthly - JHK$Field_ET_monthly, na.rm = TRUE)
pbiasPTjpl <- 100 * sum(JHK$PTjpl_monthly - JHK$Field_ET_monthly,na.rm = TRUE) / sum(JHK$Field_ET_monthly, na.rm = TRUE)
maePTjpl <- mean(abs(JHK$PTjpl_monthly - JHK$Field_ET_monthly), na.rm = TRUE)

spearman_corrPTjpl <- cor.test(JHK$Field_ET_monthly, JHK$PTjpl_monthly, method = "spearman")
print(spearman_corrPTjpl)
correlation_coefficientPTjpl <- spearman_corrPTjpl$estimate

df_stats <- data.frame(rsr_valueTC, rsr_valueWaPOR, rsr_valueSMAP, rsr_valueFLDAS, rsr_valueGLDAS, rsr_valueMOD16, rsr_valueSSEBop, rsr_valuePTjpl,
                       rmse_valueTC, rmse_valueWaPOR, rmse_valueSMAP, rmse_valueFLDAS, rmse_valueGLDAS, rmse_valueMOD16, rmse_valueSSEBop, rmse_valuePTjpl,
                       pbiasTC, pbiasWaPOR, pbiasSMAP, pbiasFLDAS, pbiasGLDAS, pbiasMOD16, pbiasSSEBop, pbiasPTjpl,
                       biasTC, biasWaPOR, biasSMAP, biasFLDAS, biasGLDAS, biasMOD16, biasSSEBop, biasPTjpl,
                       maeTC, maeWaPOR, maeSMAP, maeFLDAS, maeGLDAS, maeMOD16, maeSSEBop, maePTjpl,
                       nse_valueTC, nse_valueWaPOR, nse_valueSMAP, nse_valueFLDAS, nse_valueGLDAS, nse_valueMOD16, nse_valueSSEBop, nse_valuePTjpl,
                       kge_valueTC, kge_valueWaPOR, kge_valueSMAP, kge_valueFLDAS, kge_valueGLDAS, kge_valueMOD16, kge_valueSSEBop, kge_valuePTjpl,
                       correlation_coefficientTC, correlation_coefficientWaPOR, correlation_coefficientSMAP, correlation_coefficientFLDAS, correlation_coefficientGLDAS, correlation_coefficientMOD16, correlation_coefficientSSEBop, correlation_coefficientPTjpl)

# Reshape the data frame from wide to long format
df_long <- df_stats %>%
  pivot_longer(
    cols = everything(),
    names_to = c("metric", "site"),  # Separate columns for metric names and sites
    names_pattern = "^(pbias|nse_value|kge_value|correlation_coefficient)(.*)" # Specify explicit metric names
  ) %>%
  pivot_wider(
    names_from = metric,             # Separate each metric into its own column
    values_from = value
  )

# View the reshaped data
head(df_long)
setwd("C:\\Users\\Liam\\OneDrive - Stellenbosch University\\ET_Data\\Stats")
write.csv(df_stats, file = "Indv_Seasonal_JHK.csv", row.names = FALSE)

###################################################################################################
####################################################################################################
# MB1
# TC 
kge_valueTC <- KGE(sim = MB1$TC_monthly, obs = MB1$Field_ET_monthly)
rmse_valueTC <- rmse(actual = MB1$Field_ET_monthly, predicted = MB1$TC_monthly)
nse_valueTC <- NSE(sim = MB1$TC_monthly, obs = MB1$Field_ET_monthly)
rsr_valueTC <- rmse_valueTC / sd(MB1$Field_ET_monthly)

biasTC <- mean(MB1$TC_monthly - MB1$Field_ET_monthly)
pbiasTC <- 100 * sum(MB1$TC_monthly - MB1$Field_ET_monthly) / sum(MB1$Field_ET_monthly)
maeTC <- mean(abs(MB1$TC_monthly - MB1$Field_ET_monthly))

spearman_corrTC <- cor.test(MB1$Field_ET_monthly, MB1$TC_monthly, method = "spearman")
print(spearman_corrTC)
correlation_coefficientTC <- spearman_corrTC$estimate

# SMAP
kge_valueSMAP <- KGE(sim = MB1$SMAP_monthly, obs = MB1$Field_ET_monthly)
rmse_valueSMAP <- rmse(actual = MB1$Field_ET_monthly, predicted = MB1$SMAP_monthly)
nse_valueSMAP <- NSE(sim = MB1$SMAP_monthly, obs = MB1$Field_ET_monthly)
rsr_valueSMAP <- rmse_valueSMAP / sd(MB1$Field_ET_monthly)

biasSMAP <- mean(MB1$SMAP_monthly - MB1$Field_ET_monthly)
pbiasSMAP <- 100 * sum(MB1$SMAP_monthly - MB1$Field_ET_monthly) / sum(MB1$Field_ET_monthly)
maeSMAP <- mean(abs(MB1$SMAP_monthly - MB1$Field_ET_monthly))

spearman_corrSMAP <- cor.test(MB1$Field_ET_monthly, MB1$SMAP_monthly, method = "spearman")
print(spearman_corrSMAP)
correlation_coefficientSMAP <- spearman_corrSMAP$estimate

# SSEBop
kge_valueSSEBop <- KGE(sim = MB1$SSEBop_monthly, obs = MB1$Field_ET_monthly)
rmse_valueSSEBop <- rmse(actual = MB1$Field_ET_monthly[!is.na(MB1$Field_ET_monthly) & !is.na(MB1$SSEBop_monthly)], 
                         predicted = MB1$SSEBop_monthly[!is.na(MB1$Field_ET_monthly) & !is.na(MB1$SSEBop_monthly)])
nse_valueSSEBop <- NSE(sim = MB1$SSEBop_monthly, obs = MB1$Field_ET_monthly)
rsr_valueSSEBop <- rmse_valueSSEBop / sd(MB1$Field_ET_monthly, na.rm = TRUE)

biasSSEBop <- mean(MB1$SSEBop_monthly - MB1$Field_ET_monthly, na.rm = TRUE)
pbiasSSEBop <- 100 * sum(MB1$SSEBop_monthly - MB1$Field_ET_monthly, na.rm = TRUE) / sum(MB1$Field_ET_monthly, na.rm = TRUE)
maeSSEBop <- mean(abs(MB1$SSEBop_monthly - MB1$Field_ET_monthly), na.rm = TRUE)

spearman_corrSSEBop <- cor.test(MB1$Field_ET_monthly, MB1$SSEBop_monthly, method = "spearman")
print(spearman_corrSSEBop)
correlation_coefficientSSEBop <- spearman_corrSSEBop$estimate

# WAPOR
kge_valueWaPOR <- KGE(sim = MB1$WaPOR_monthly, obs = MB1$Field_ET_monthly)
rmse_valueWaPOR <- rmse(actual = MB1$Field_ET_monthly[!is.na(MB1$Field_ET_monthly) & !is.na(MB1$WaPOR_monthly)],
                        predicted = MB1$WaPOR_monthly[!is.na(MB1$Field_ET_monthly) & !is.na(MB1$WaPOR_monthly)])
nse_valueWaPOR <- NSE(sim = MB1$WaPOR_monthly, obs = MB1$Field_ET_monthly)
rsr_valueWaPOR <- rmse_valueWaPOR / sd(MB1$Field_ET_monthly, na.rm = TRUE)

biasWaPOR <- mean(MB1$WaPOR_monthly - MB1$Field_ET_monthly, na.rm = TRUE)
pbiasWaPOR <- 100 * sum(MB1$WaPOR_monthly - MB1$Field_ET_monthly, na.rm = TRUE) / sum(MB1$Field_ET_monthly, na.rm = TRUE)
maeWaPOR <- mean(abs(MB1$WaPOR_monthly - MB1$Field_ET_monthly), na.rm = TRUE)

spearman_corrWaPOR <- cor.test(MB1$Field_ET_monthly, MB1$WaPOR_monthly, method = "spearman")
print(spearman_corrWaPOR)
correlation_coefficientWaPOR <- spearman_corrWaPOR$estimate

# MOD16
kge_valueMOD16 <- KGE(sim = MB1$MOD16_monthly, obs = MB1$Field_ET_monthly)
rmse_valueMOD16 <- rmse(actual = MB1$Field_ET_monthly, predicted = MB1$MOD16_monthly)
nse_valueMOD16 <- NSE(sim = MB1$MOD16_monthly, obs = MB1$Field_ET_monthly)
rsr_valueMOD16 <- rmse_valueMOD16 / sd(MB1$Field_ET_monthly)

biasMOD16 <- mean(MB1$MOD16_monthly - MB1$Field_ET_monthly)
pbiasMOD16 <- 100 * sum(MB1$MOD16_monthly - MB1$Field_ET_monthly) / sum(MB1$Field_ET_monthly)
maeMOD16 <- mean(abs(MB1$MOD16_monthly - MB1$Field_ET_monthly))

spearman_corrMOD16 <- cor.test(MB1$Field_ET_monthly, MB1$MOD16_monthly, method = "spearman")
print(spearman_corrMOD16)
correlation_coefficientMOD16 <- spearman_corrMOD16$estimate

# GLDAS
kge_valueGLDAS <- KGE(sim = MB1$GLDAS_monthly, obs = MB1$Field_ET_monthly)
rmse_valueGLDAS <- rmse(actual = MB1$Field_ET_monthly, predicted = MB1$GLDAS_monthly)
nse_valueGLDAS <- NSE(sim = MB1$GLDAS_monthly, obs = MB1$Field_ET_monthly)
rsr_valueGLDAS <- rmse_valueGLDAS / sd(MB1$Field_ET_monthly)

biasGLDAS <- mean(MB1$GLDAS_monthly - MB1$Field_ET_monthly)
pbiasGLDAS <- 100 * sum(MB1$GLDAS_monthly - MB1$Field_ET_monthly) / sum(MB1$Field_ET_monthly)
maeGLDAS <- mean(abs(MB1$GLDAS_monthly - MB1$Field_ET_monthly))

spearman_corrGLDAS <- cor.test(MB1$Field_ET_monthly, MB1$GLDAS_monthly, method = "spearman")
print(spearman_corrGLDAS)
correlation_coefficientGLDAS <- spearman_corrGLDAS$estimate

# FLDAS
kge_valueFLDAS <- KGE(sim = MB1$FLDAS_monthly, obs = MB1$Field_ET_monthly)
rmse_valueFLDAS <- rmse(actual = MB1$Field_ET_monthly, predicted = MB1$FLDAS_monthly)
nse_valueFLDAS <- NSE(sim = MB1$FLDAS_monthly, obs = MB1$Field_ET_monthly)
rsr_valueFLDAS <- rmse_valueFLDAS / sd(MB1$Field_ET_monthly)

biasFLDAS <- mean(MB1$FLDAS_monthly - MB1$Field_ET_monthly)
pbiasFLDAS <- 100 * sum(MB1$FLDAS_monthly - MB1$Field_ET_monthly) / sum(MB1$Field_ET_monthly)
maeFLDAS <- mean(abs(MB1$FLDAS_monthly - MB1$Field_ET_monthly))

spearman_corrFLDAS <- cor.test(MB1$Field_ET_monthly, MB1$FLDAS_monthly, method = "spearman")
print(spearman_corrFLDAS)
correlation_coefficientFLDAS <- spearman_corrFLDAS$estimate

# PTJPL
kge_valuePTjpl <- KGE(sim = MB1$PTjpl_monthly, obs = MB1$Field_ET_monthly)
rmse_valuePTjpl <- rmse(actual = MB1$Field_ET_monthly[!is.na(MB1$Field_ET_monthly) & !is.na(MB1$PTjpl_monthly)], 
                        predicted = MB1$PTjpl_monthly[!is.na(MB1$Field_ET_monthly) & !is.na(MB1$PTjpl_monthly)])
nse_valuePTjpl <- NSE(sim = MB1$PTjpl_monthly, obs = MB1$Field_ET_monthly)
rsr_valuePTjpl <- rmse_valuePTjpl / sd(MB1$Field_ET_monthly, na.rm = TRUE)

biasPTjpl <- mean(MB1$PTjpl_monthly - MB1$Field_ET_monthly, na.rm = TRUE)
pbiasPTjpl <- 100 * sum(MB1$PTjpl_monthly - MB1$Field_ET_monthly,na.rm = TRUE) / sum(MB1$Field_ET_monthly, na.rm = TRUE)
maePTjpl <- mean(abs(MB1$PTjpl_monthly - MB1$Field_ET_monthly), na.rm = TRUE)

spearman_corrPTjpl <- cor.test(MB1$Field_ET_monthly, MB1$PTjpl_monthly, method = "spearman")
print(spearman_corrPTjpl)
correlation_coefficientPTjpl <- spearman_corrPTjpl$estimate

df_stats <- data.frame(rsr_valueTC, rsr_valueWaPOR, rsr_valueSMAP, rsr_valueFLDAS, rsr_valueGLDAS, rsr_valueMOD16, rsr_valueSSEBop, rsr_valuePTjpl,
                       rmse_valueTC, rmse_valueWaPOR, rmse_valueSMAP, rmse_valueFLDAS, rmse_valueGLDAS, rmse_valueMOD16, rmse_valueSSEBop, rmse_valuePTjpl,
                       pbiasTC, pbiasWaPOR, pbiasSMAP, pbiasFLDAS, pbiasGLDAS, pbiasMOD16, pbiasSSEBop, pbiasPTjpl,
                       biasTC, biasWaPOR, biasSMAP, biasFLDAS, biasGLDAS, biasMOD16, biasSSEBop, biasPTjpl,
                       maeTC, maeWaPOR, maeSMAP, maeFLDAS, maeGLDAS, maeMOD16, maeSSEBop, maePTjpl,
                       nse_valueTC, nse_valueWaPOR, nse_valueSMAP, nse_valueFLDAS, nse_valueGLDAS, nse_valueMOD16, nse_valueSSEBop, nse_valuePTjpl,
                       kge_valueTC, kge_valueWaPOR, kge_valueSMAP, kge_valueFLDAS, kge_valueGLDAS, kge_valueMOD16, kge_valueSSEBop, kge_valuePTjpl,
                       correlation_coefficientTC, correlation_coefficientWaPOR, correlation_coefficientSMAP, correlation_coefficientFLDAS, correlation_coefficientGLDAS, correlation_coefficientMOD16, correlation_coefficientSSEBop, correlation_coefficientPTjpl)

# Reshape the data frame from wide to long format
df_long <- df_stats %>%
  pivot_longer(
    cols = everything(),
    names_to = c("metric", "site"),  # Separate columns for metric names and sites
    names_pattern = "^(pbias|nse_value|kge_value|correlation_coefficient)(.*)" # Specify explicit metric names
  ) %>%
  pivot_wider(
    names_from = metric,             # Separate each metric into its own column
    values_from = value
  )

# View the reshaped data
head(df_long)
setwd("C:\\Users\\Liam\\OneDrive - Stellenbosch University\\ET_Data\\Stats")
write.csv(df_stats, file = "Indv_Seasonal_MB1.csv", row.names = FALSE)

###################################################################################################
####################################################################################################
# MB2
# TC 
kge_valueTC <- KGE(sim = MB2$TC_monthly, obs = MB2$Field_ET_monthly)
rmse_valueTC <- rmse(actual = MB2$Field_ET_monthly, predicted = MB2$TC_monthly)
nse_valueTC <- NSE(sim = MB2$TC_monthly, obs = MB2$Field_ET_monthly)
rsr_valueTC <- rmse_valueTC / sd(MB2$Field_ET_monthly)

biasTC <- mean(MB2$TC_monthly - MB2$Field_ET_monthly)
pbiasTC <- 100 * sum(MB2$TC_monthly - MB2$Field_ET_monthly) / sum(MB2$Field_ET_monthly)
maeTC <- mean(abs(MB2$TC_monthly - MB2$Field_ET_monthly))

spearman_corrTC <- cor.test(MB2$Field_ET_monthly, MB2$TC_monthly, method = "spearman")
print(spearman_corrTC)
correlation_coefficientTC <- spearman_corrTC$estimate

# SMAP
kge_valueSMAP <- KGE(sim = MB2$SMAP_monthly, obs = MB2$Field_ET_monthly)
rmse_valueSMAP <- rmse(actual = MB2$Field_ET_monthly, predicted = MB2$SMAP_monthly)
nse_valueSMAP <- NSE(sim = MB2$SMAP_monthly, obs = MB2$Field_ET_monthly)
rsr_valueSMAP <- rmse_valueSMAP / sd(MB2$Field_ET_monthly)

biasSMAP <- mean(MB2$SMAP_monthly - MB2$Field_ET_monthly)
pbiasSMAP <- 100 * sum(MB2$SMAP_monthly - MB2$Field_ET_monthly) / sum(MB2$Field_ET_monthly)
maeSMAP <- mean(abs(MB2$SMAP_monthly - MB2$Field_ET_monthly))

spearman_corrSMAP <- cor.test(MB2$Field_ET_monthly, MB2$SMAP_monthly, method = "spearman")
print(spearman_corrSMAP)
correlation_coefficientSMAP <- spearman_corrSMAP$estimate

# SSEBop
kge_valueSSEBop <- KGE(sim = MB2$SSEBop_monthly, obs = MB2$Field_ET_monthly)
rmse_valueSSEBop <- rmse(actual = MB2$Field_ET_monthly[!is.na(MB2$Field_ET_monthly) & !is.na(MB2$SSEBop_monthly)], 
                         predicted = MB2$SSEBop_monthly[!is.na(MB2$Field_ET_monthly) & !is.na(MB2$SSEBop_monthly)])
nse_valueSSEBop <- NSE(sim = MB2$SSEBop_monthly, obs = MB2$Field_ET_monthly)
rsr_valueSSEBop <- rmse_valueSSEBop / sd(MB2$Field_ET_monthly, na.rm = TRUE)

biasSSEBop <- mean(MB2$SSEBop_monthly - MB2$Field_ET_monthly, na.rm = TRUE)
pbiasSSEBop <- 100 * sum(MB2$SSEBop_monthly - MB2$Field_ET_monthly, na.rm = TRUE) / sum(MB2$Field_ET_monthly, na.rm = TRUE)
maeSSEBop <- mean(abs(MB2$SSEBop_monthly - MB2$Field_ET_monthly), na.rm = TRUE)

spearman_corrSSEBop <- cor.test(MB2$Field_ET_monthly, MB2$SSEBop_monthly, method = "spearman")
print(spearman_corrSSEBop)
correlation_coefficientSSEBop <- spearman_corrSSEBop$estimate

# WAPOR
kge_valueWaPOR <- KGE(sim = MB2$WaPOR_monthly, obs = MB2$Field_ET_monthly)
rmse_valueWaPOR <- rmse(actual = MB2$Field_ET_monthly[!is.na(MB2$Field_ET_monthly) & !is.na(MB2$WaPOR_monthly)],
                        predicted = MB2$WaPOR_monthly[!is.na(MB2$Field_ET_monthly) & !is.na(MB2$WaPOR_monthly)])
nse_valueWaPOR <- NSE(sim = MB2$WaPOR_monthly, obs = MB2$Field_ET_monthly)
rsr_valueWaPOR <- rmse_valueWaPOR / sd(MB2$Field_ET_monthly, na.rm = TRUE)

biasWaPOR <- mean(MB2$WaPOR_monthly - MB2$Field_ET_monthly, na.rm = TRUE)
pbiasWaPOR <- 100 * sum(MB2$WaPOR_monthly - MB2$Field_ET_monthly, na.rm = TRUE) / sum(MB2$Field_ET_monthly, na.rm = TRUE)
maeWaPOR <- mean(abs(MB2$WaPOR_monthly - MB2$Field_ET_monthly), na.rm = TRUE)

spearman_corrWaPOR <- cor.test(MB2$Field_ET_monthly, MB2$WaPOR_monthly, method = "spearman")
print(spearman_corrWaPOR)
correlation_coefficientWaPOR <- spearman_corrWaPOR$estimate

# MOD16
kge_valueMOD16 <- KGE(sim = MB2$MOD16_monthly, obs = MB2$Field_ET_monthly)
rmse_valueMOD16 <- rmse(actual = MB2$Field_ET_monthly, predicted = MB2$MOD16_monthly)
nse_valueMOD16 <- NSE(sim = MB2$MOD16_monthly, obs = MB2$Field_ET_monthly)
rsr_valueMOD16 <- rmse_valueMOD16 / sd(MB2$Field_ET_monthly)

biasMOD16 <- mean(MB2$MOD16_monthly - MB2$Field_ET_monthly)
pbiasMOD16 <- 100 * sum(MB2$MOD16_monthly - MB2$Field_ET_monthly) / sum(MB2$Field_ET_monthly)
maeMOD16 <- mean(abs(MB2$MOD16_monthly - MB2$Field_ET_monthly))

spearman_corrMOD16 <- cor.test(MB2$Field_ET_monthly, MB2$MOD16_monthly, method = "spearman")
print(spearman_corrMOD16)
correlation_coefficientMOD16 <- spearman_corrMOD16$estimate

# GLDAS
kge_valueGLDAS <- KGE(sim = MB2$GLDAS_monthly, obs = MB2$Field_ET_monthly)
rmse_valueGLDAS <- rmse(actual = MB2$Field_ET_monthly, predicted = MB2$GLDAS_monthly)
nse_valueGLDAS <- NSE(sim = MB2$GLDAS_monthly, obs = MB2$Field_ET_monthly)
rsr_valueGLDAS <- rmse_valueGLDAS / sd(MB2$Field_ET_monthly)

biasGLDAS <- mean(MB2$GLDAS_monthly - MB2$Field_ET_monthly)
pbiasGLDAS <- 100 * sum(MB2$GLDAS_monthly - MB2$Field_ET_monthly) / sum(MB2$Field_ET_monthly)
maeGLDAS <- mean(abs(MB2$GLDAS_monthly - MB2$Field_ET_monthly))

spearman_corrGLDAS <- cor.test(MB2$Field_ET_monthly, MB2$GLDAS_monthly, method = "spearman")
print(spearman_corrGLDAS)
correlation_coefficientGLDAS <- spearman_corrGLDAS$estimate

# FLDAS
kge_valueFLDAS <- KGE(sim = MB2$FLDAS_monthly, obs = MB2$Field_ET_monthly)
rmse_valueFLDAS <- rmse(actual = MB2$Field_ET_monthly, predicted = MB2$FLDAS_monthly)
nse_valueFLDAS <- NSE(sim = MB2$FLDAS_monthly, obs = MB2$Field_ET_monthly)
rsr_valueFLDAS <- rmse_valueFLDAS / sd(MB2$Field_ET_monthly)

biasFLDAS <- mean(MB2$FLDAS_monthly - MB2$Field_ET_monthly)
pbiasFLDAS <- 100 * sum(MB2$FLDAS_monthly - MB2$Field_ET_monthly) / sum(MB2$Field_ET_monthly)
maeFLDAS <- mean(abs(MB2$FLDAS_monthly - MB2$Field_ET_monthly))

spearman_corrFLDAS <- cor.test(MB2$Field_ET_monthly, MB2$FLDAS_monthly, method = "spearman")
print(spearman_corrFLDAS)
correlation_coefficientFLDAS <- spearman_corrFLDAS$estimate

# PTJPL
kge_valuePTjpl <- KGE(sim = MB2$PTjpl_monthly, obs = MB2$Field_ET_monthly)
rmse_valuePTjpl <- rmse(actual = MB2$Field_ET_monthly[!is.na(MB2$Field_ET_monthly) & !is.na(MB2$PTjpl_monthly)], 
                        predicted = MB2$PTjpl_monthly[!is.na(MB2$Field_ET_monthly) & !is.na(MB2$PTjpl_monthly)])
nse_valuePTjpl <- NSE(sim = MB2$PTjpl_monthly, obs = MB2$Field_ET_monthly)
rsr_valuePTjpl <- rmse_valuePTjpl / sd(MB2$Field_ET_monthly, na.rm = TRUE)

biasPTjpl <- mean(MB2$PTjpl_monthly - MB2$Field_ET_monthly, na.rm = TRUE)
pbiasPTjpl <- 100 * sum(MB2$PTjpl_monthly - MB2$Field_ET_monthly,na.rm = TRUE) / sum(MB2$Field_ET_monthly, na.rm = TRUE)
maePTjpl <- mean(abs(MB2$PTjpl_monthly - MB2$Field_ET_monthly), na.rm = TRUE)

spearman_corrPTjpl <- cor.test(MB2$Field_ET_monthly, MB2$PTjpl_monthly, method = "spearman")
print(spearman_corrPTjpl)
correlation_coefficientPTjpl <- spearman_corrPTjpl$estimate

df_stats <- data.frame(rsr_valueTC, rsr_valueWaPOR, rsr_valueSMAP, rsr_valueFLDAS, rsr_valueGLDAS, rsr_valueMOD16, rsr_valueSSEBop, rsr_valuePTjpl,
                       rmse_valueTC, rmse_valueWaPOR, rmse_valueSMAP, rmse_valueFLDAS, rmse_valueGLDAS, rmse_valueMOD16, rmse_valueSSEBop, rmse_valuePTjpl,
                       pbiasTC, pbiasWaPOR, pbiasSMAP, pbiasFLDAS, pbiasGLDAS, pbiasMOD16, pbiasSSEBop, pbiasPTjpl,
                       biasTC, biasWaPOR, biasSMAP, biasFLDAS, biasGLDAS, biasMOD16, biasSSEBop, biasPTjpl,
                       maeTC, maeWaPOR, maeSMAP, maeFLDAS, maeGLDAS, maeMOD16, maeSSEBop, maePTjpl,
                       nse_valueTC, nse_valueWaPOR, nse_valueSMAP, nse_valueFLDAS, nse_valueGLDAS, nse_valueMOD16, nse_valueSSEBop, nse_valuePTjpl,
                       kge_valueTC, kge_valueWaPOR, kge_valueSMAP, kge_valueFLDAS, kge_valueGLDAS, kge_valueMOD16, kge_valueSSEBop, kge_valuePTjpl,
                       correlation_coefficientTC, correlation_coefficientWaPOR, correlation_coefficientSMAP, correlation_coefficientFLDAS, correlation_coefficientGLDAS, correlation_coefficientMOD16, correlation_coefficientSSEBop, correlation_coefficientPTjpl)

# Reshape the data frame from wide to long format
df_long <- df_stats %>%
  pivot_longer(
    cols = everything(),
    names_to = c("metric", "site"),  # Separate columns for metric names and sites
    names_pattern = "^(pbias|nse_value|kge_value|correlation_coefficient)(.*)" # Specify explicit metric names
  ) %>%
  pivot_wider(
    names_from = metric,             # Separate each metric into its own column
    values_from = value
  )

# View the reshaped data
head(df_long)
setwd("C:\\Users\\Liam\\OneDrive - Stellenbosch University\\ET_Data\\Stats")
write.csv(df_stats, file = "Indv_Seasonal_MB2.csv", row.names = FALSE)

###################################################################################################
####################################################################################################
# MCP
# TC 
kge_valueTC <- KGE(sim = MCP$TC_monthly, obs = MCP$Field_ET_monthly)
rmse_valueTC <- rmse(actual = MCP$Field_ET_monthly, predicted = MCP$TC_monthly)
nse_valueTC <- NSE(sim = MCP$TC_monthly, obs = MCP$Field_ET_monthly)
rsr_valueTC <- rmse_valueTC / sd(MCP$Field_ET_monthly)

biasTC <- mean(MCP$TC_monthly - MCP$Field_ET_monthly)
pbiasTC <- 100 * sum(MCP$TC_monthly - MCP$Field_ET_monthly) / sum(MCP$Field_ET_monthly)
maeTC <- mean(abs(MCP$TC_monthly - MCP$Field_ET_monthly))

spearman_corrTC <- cor.test(MCP$Field_ET_monthly, MCP$TC_monthly, method = "spearman")
print(spearman_corrTC)
correlation_coefficientTC <- spearman_corrTC$estimate

# SMAP
kge_valueSMAP <- KGE(sim = MCP$SMAP_monthly, obs = MCP$Field_ET_monthly)
rmse_valueSMAP <- rmse(actual = MCP$Field_ET_monthly, predicted = MCP$SMAP_monthly)
nse_valueSMAP <- NSE(sim = MCP$SMAP_monthly, obs = MCP$Field_ET_monthly)
rsr_valueSMAP <- rmse_valueSMAP / sd(MCP$Field_ET_monthly)

biasSMAP <- mean(MCP$SMAP_monthly - MCP$Field_ET_monthly)
pbiasSMAP <- 100 * sum(MCP$SMAP_monthly - MCP$Field_ET_monthly) / sum(MCP$Field_ET_monthly)
maeSMAP <- mean(abs(MCP$SMAP_monthly - MCP$Field_ET_monthly))

spearman_corrSMAP <- cor.test(MCP$Field_ET_monthly, MCP$SMAP_monthly, method = "spearman")
print(spearman_corrSMAP)
correlation_coefficientSMAP <- spearman_corrSMAP$estimate

# SSEBop
kge_valueSSEBop <- KGE(sim = MCP$SSEBop_monthly, obs = MCP$Field_ET_monthly)
rmse_valueSSEBop <- rmse(actual = MCP$Field_ET_monthly[!is.na(MCP$Field_ET_monthly) & !is.na(MCP$SSEBop_monthly)], 
                         predicted = MCP$SSEBop_monthly[!is.na(MCP$Field_ET_monthly) & !is.na(MCP$SSEBop_monthly)])
nse_valueSSEBop <- NSE(sim = MCP$SSEBop_monthly, obs = MCP$Field_ET_monthly)
rsr_valueSSEBop <- rmse_valueSSEBop / sd(MCP$Field_ET_monthly, na.rm = TRUE)

biasSSEBop <- mean(MCP$SSEBop_monthly - MCP$Field_ET_monthly, na.rm = TRUE)
pbiasSSEBop <- 100 * sum(MCP$SSEBop_monthly - MCP$Field_ET_monthly, na.rm = TRUE) / sum(MCP$Field_ET_monthly, na.rm = TRUE)
maeSSEBop <- mean(abs(MCP$SSEBop_monthly - MCP$Field_ET_monthly), na.rm = TRUE)

spearman_corrSSEBop <- cor.test(MCP$Field_ET_monthly, MCP$SSEBop_monthly, method = "spearman")
print(spearman_corrSSEBop)
correlation_coefficientSSEBop <- spearman_corrSSEBop$estimate

# WAPOR
kge_valueWaPOR <- KGE(sim = MCP$WaPOR_monthly, obs = MCP$Field_ET_monthly)
rmse_valueWaPOR <- rmse(actual = MCP$Field_ET_monthly[!is.na(MCP$Field_ET_monthly) & !is.na(MCP$WaPOR_monthly)],
                        predicted = MCP$WaPOR_monthly[!is.na(MCP$Field_ET_monthly) & !is.na(MCP$WaPOR_monthly)])
nse_valueWaPOR <- NSE(sim = MCP$WaPOR_monthly, obs = MCP$Field_ET_monthly)
rsr_valueWaPOR <- rmse_valueWaPOR / sd(MCP$Field_ET_monthly, na.rm = TRUE)

biasWaPOR <- mean(MCP$WaPOR_monthly - MCP$Field_ET_monthly, na.rm = TRUE)
pbiasWaPOR <- 100 * sum(MCP$WaPOR_monthly - MCP$Field_ET_monthly, na.rm = TRUE) / sum(MCP$Field_ET_monthly, na.rm = TRUE)
maeWaPOR <- mean(abs(MCP$WaPOR_monthly - MCP$Field_ET_monthly), na.rm = TRUE)

spearman_corrWaPOR <- cor.test(MCP$Field_ET_monthly, MCP$WaPOR_monthly, method = "spearman")
print(spearman_corrWaPOR)
correlation_coefficientWaPOR <- spearman_corrWaPOR$estimate

# MOD16
kge_valueMOD16 <- KGE(sim = MCP$MOD16_monthly, obs = MCP$Field_ET_monthly)
rmse_valueMOD16 <- rmse(actual = MCP$Field_ET_monthly, predicted = MCP$MOD16_monthly)
nse_valueMOD16 <- NSE(sim = MCP$MOD16_monthly, obs = MCP$Field_ET_monthly)
rsr_valueMOD16 <- rmse_valueMOD16 / sd(MCP$Field_ET_monthly)

biasMOD16 <- mean(MCP$MOD16_monthly - MCP$Field_ET_monthly)
pbiasMOD16 <- 100 * sum(MCP$MOD16_monthly - MCP$Field_ET_monthly) / sum(MCP$Field_ET_monthly)
maeMOD16 <- mean(abs(MCP$MOD16_monthly - MCP$Field_ET_monthly))

spearman_corrMOD16 <- cor.test(MCP$Field_ET_monthly, MCP$MOD16_monthly, method = "spearman")
print(spearman_corrMOD16)
correlation_coefficientMOD16 <- spearman_corrMOD16$estimate

# GLDAS
kge_valueGLDAS <- KGE(sim = MCP$GLDAS_monthly, obs = MCP$Field_ET_monthly)
rmse_valueGLDAS <- rmse(actual = MCP$Field_ET_monthly, predicted = MCP$GLDAS_monthly)
nse_valueGLDAS <- NSE(sim = MCP$GLDAS_monthly, obs = MCP$Field_ET_monthly)
rsr_valueGLDAS <- rmse_valueGLDAS / sd(MCP$Field_ET_monthly)

biasGLDAS <- mean(MCP$GLDAS_monthly - MCP$Field_ET_monthly)
pbiasGLDAS <- 100 * sum(MCP$GLDAS_monthly - MCP$Field_ET_monthly) / sum(MCP$Field_ET_monthly)
maeGLDAS <- mean(abs(MCP$GLDAS_monthly - MCP$Field_ET_monthly))

spearman_corrGLDAS <- cor.test(MCP$Field_ET_monthly, MCP$GLDAS_monthly, method = "spearman")
print(spearman_corrGLDAS)
correlation_coefficientGLDAS <- spearman_corrGLDAS$estimate

# FLDAS
kge_valueFLDAS <- KGE(sim = MCP$FLDAS_monthly, obs = MCP$Field_ET_monthly)
rmse_valueFLDAS <- rmse(actual = MCP$Field_ET_monthly, predicted = MCP$FLDAS_monthly)
nse_valueFLDAS <- NSE(sim = MCP$FLDAS_monthly, obs = MCP$Field_ET_monthly)
rsr_valueFLDAS <- rmse_valueFLDAS / sd(MCP$Field_ET_monthly)

biasFLDAS <- mean(MCP$FLDAS_monthly - MCP$Field_ET_monthly)
pbiasFLDAS <- 100 * sum(MCP$FLDAS_monthly - MCP$Field_ET_monthly) / sum(MCP$Field_ET_monthly)
maeFLDAS <- mean(abs(MCP$FLDAS_monthly - MCP$Field_ET_monthly))

spearman_corrFLDAS <- cor.test(MCP$Field_ET_monthly, MCP$FLDAS_monthly, method = "spearman")
print(spearman_corrFLDAS)
correlation_coefficientFLDAS <- spearman_corrFLDAS$estimate

# PTJPL
kge_valuePTjpl <- KGE(sim = MCP$PTjpl_monthly, obs = MCP$Field_ET_monthly)
rmse_valuePTjpl <- rmse(actual = MCP$Field_ET_monthly[!is.na(MCP$Field_ET_monthly) & !is.na(MCP$PTjpl_monthly)], 
                        predicted = MCP$PTjpl_monthly[!is.na(MCP$Field_ET_monthly) & !is.na(MCP$PTjpl_monthly)])
nse_valuePTjpl <- NSE(sim = MCP$PTjpl_monthly, obs = MCP$Field_ET_monthly)
rsr_valuePTjpl <- rmse_valuePTjpl / sd(MCP$Field_ET_monthly, na.rm = TRUE)

biasPTjpl <- mean(MCP$PTjpl_monthly - MCP$Field_ET_monthly, na.rm = TRUE)
pbiasPTjpl <- 100 * sum(MCP$PTjpl_monthly - MCP$Field_ET_monthly,na.rm = TRUE) / sum(MCP$Field_ET_monthly, na.rm = TRUE)
maePTjpl <- mean(abs(MCP$PTjpl_monthly - MCP$Field_ET_monthly), na.rm = TRUE)

spearman_corrPTjpl <- cor.test(MCP$Field_ET_monthly, MCP$PTjpl_monthly, method = "spearman")
print(spearman_corrPTjpl)
correlation_coefficientPTjpl <- spearman_corrPTjpl$estimate

df_stats <- data.frame(rsr_valueTC, rsr_valueWaPOR, rsr_valueSMAP, rsr_valueFLDAS, rsr_valueGLDAS, rsr_valueMOD16, rsr_valueSSEBop, rsr_valuePTjpl,
                       rmse_valueTC, rmse_valueWaPOR, rmse_valueSMAP, rmse_valueFLDAS, rmse_valueGLDAS, rmse_valueMOD16, rmse_valueSSEBop, rmse_valuePTjpl,
                       pbiasTC, pbiasWaPOR, pbiasSMAP, pbiasFLDAS, pbiasGLDAS, pbiasMOD16, pbiasSSEBop, pbiasPTjpl,
                       biasTC, biasWaPOR, biasSMAP, biasFLDAS, biasGLDAS, biasMOD16, biasSSEBop, biasPTjpl,
                       maeTC, maeWaPOR, maeSMAP, maeFLDAS, maeGLDAS, maeMOD16, maeSSEBop, maePTjpl,
                       nse_valueTC, nse_valueWaPOR, nse_valueSMAP, nse_valueFLDAS, nse_valueGLDAS, nse_valueMOD16, nse_valueSSEBop, nse_valuePTjpl,
                       kge_valueTC, kge_valueWaPOR, kge_valueSMAP, kge_valueFLDAS, kge_valueGLDAS, kge_valueMOD16, kge_valueSSEBop, kge_valuePTjpl,
                       correlation_coefficientTC, correlation_coefficientWaPOR, correlation_coefficientSMAP, correlation_coefficientFLDAS, correlation_coefficientGLDAS, correlation_coefficientMOD16, correlation_coefficientSSEBop, correlation_coefficientPTjpl)

# Reshape the data frame from wide to long format
df_long <- df_stats %>%
  pivot_longer(
    cols = everything(),
    names_to = c("metric", "site"),  # Separate columns for metric names and sites
    names_pattern = "^(pbias|nse_value|kge_value|correlation_coefficient)(.*)" # Specify explicit metric names
  ) %>%
  pivot_wider(
    names_from = metric,             # Separate each metric into its own column
    values_from = value
  )

# View the reshaped data
head(df_long)
setwd("C:\\Users\\Liam\\OneDrive - Stellenbosch University\\ET_Data\\Stats")
write.csv(df_stats, file = "Indv_Seasonal_MCP.csv", row.names = FALSE)

###################################################################################################
####################################################################################################
# MLP
# TC 
kge_valueTC <- KGE(sim = MLP$TC_monthly, obs = MLP$Field_ET_monthly)
rmse_valueTC <- rmse(actual = MLP$Field_ET_monthly, predicted = MLP$TC_monthly)
nse_valueTC <- NSE(sim = MLP$TC_monthly, obs = MLP$Field_ET_monthly)
rsr_valueTC <- rmse_valueTC / sd(MLP$Field_ET_monthly)

biasTC <- mean(MLP$TC_monthly - MLP$Field_ET_monthly)
pbiasTC <- 100 * sum(MLP$TC_monthly - MLP$Field_ET_monthly) / sum(MLP$Field_ET_monthly)
maeTC <- mean(abs(MLP$TC_monthly - MLP$Field_ET_monthly))

spearman_corrTC <- cor.test(MLP$Field_ET_monthly, MLP$TC_monthly, method = "spearman")
print(spearman_corrTC)
correlation_coefficientTC <- spearman_corrTC$estimate

# SMAP
kge_valueSMAP <- KGE(sim = MLP$SMAP_monthly, obs = MLP$Field_ET_monthly)
rmse_valueSMAP <- rmse(actual = MLP$Field_ET_monthly, predicted = MLP$SMAP_monthly)
nse_valueSMAP <- NSE(sim = MLP$SMAP_monthly, obs = MLP$Field_ET_monthly)
rsr_valueSMAP <- rmse_valueSMAP / sd(MLP$Field_ET_monthly)

biasSMAP <- mean(MLP$SMAP_monthly - MLP$Field_ET_monthly)
pbiasSMAP <- 100 * sum(MLP$SMAP_monthly - MLP$Field_ET_monthly) / sum(MLP$Field_ET_monthly)
maeSMAP <- mean(abs(MLP$SMAP_monthly - MLP$Field_ET_monthly))

spearman_corrSMAP <- cor.test(MLP$Field_ET_monthly, MLP$SMAP_monthly, method = "spearman")
print(spearman_corrSMAP)
correlation_coefficientSMAP <- spearman_corrSMAP$estimate

# SSEBop
kge_valueSSEBop <- KGE(sim = MLP$SSEBop_monthly, obs = MLP$Field_ET_monthly)
rmse_valueSSEBop <- rmse(actual = MLP$Field_ET_monthly[!is.na(MLP$Field_ET_monthly) & !is.na(MLP$SSEBop_monthly)], 
                         predicted = MLP$SSEBop_monthly[!is.na(MLP$Field_ET_monthly) & !is.na(MLP$SSEBop_monthly)])
nse_valueSSEBop <- NSE(sim = MLP$SSEBop_monthly, obs = MLP$Field_ET_monthly)
rsr_valueSSEBop <- rmse_valueSSEBop / sd(MLP$Field_ET_monthly, na.rm = TRUE)

biasSSEBop <- mean(MLP$SSEBop_monthly - MLP$Field_ET_monthly, na.rm = TRUE)
pbiasSSEBop <- 100 * sum(MLP$SSEBop_monthly - MLP$Field_ET_monthly, na.rm = TRUE) / sum(MLP$Field_ET_monthly, na.rm = TRUE)
maeSSEBop <- mean(abs(MLP$SSEBop_monthly - MLP$Field_ET_monthly), na.rm = TRUE)

spearman_corrSSEBop <- cor.test(MLP$Field_ET_monthly, MLP$SSEBop_monthly, method = "spearman")
print(spearman_corrSSEBop)
correlation_coefficientSSEBop <- spearman_corrSSEBop$estimate

# WAPOR
kge_valueWaPOR <- KGE(sim = MLP$WaPOR_monthly, obs = MLP$Field_ET_monthly)
rmse_valueWaPOR <- rmse(actual = MLP$Field_ET_monthly[!is.na(MLP$Field_ET_monthly) & !is.na(MLP$WaPOR_monthly)],
                        predicted = MLP$WaPOR_monthly[!is.na(MLP$Field_ET_monthly) & !is.na(MLP$WaPOR_monthly)])
nse_valueWaPOR <- NSE(sim = MLP$WaPOR_monthly, obs = MLP$Field_ET_monthly)
rsr_valueWaPOR <- rmse_valueWaPOR / sd(MLP$Field_ET_monthly, na.rm = TRUE)

biasWaPOR <- mean(MLP$WaPOR_monthly - MLP$Field_ET_monthly, na.rm = TRUE)
pbiasWaPOR <- 100 * sum(MLP$WaPOR_monthly - MLP$Field_ET_monthly, na.rm = TRUE) / sum(MLP$Field_ET_monthly, na.rm = TRUE)
maeWaPOR <- mean(abs(MLP$WaPOR_monthly - MLP$Field_ET_monthly), na.rm = TRUE)

spearman_corrWaPOR <- cor.test(MLP$Field_ET_monthly, MLP$WaPOR_monthly, method = "spearman")
print(spearman_corrWaPOR)
correlation_coefficientWaPOR <- spearman_corrWaPOR$estimate

# MOD16
kge_valueMOD16 <- KGE(sim = MLP$MOD16_monthly, obs = MLP$Field_ET_monthly)
rmse_valueMOD16 <- rmse(actual = MLP$Field_ET_monthly, predicted = MLP$MOD16_monthly)
nse_valueMOD16 <- NSE(sim = MLP$MOD16_monthly, obs = MLP$Field_ET_monthly)
rsr_valueMOD16 <- rmse_valueMOD16 / sd(MLP$Field_ET_monthly)

biasMOD16 <- mean(MLP$MOD16_monthly - MLP$Field_ET_monthly)
pbiasMOD16 <- 100 * sum(MLP$MOD16_monthly - MLP$Field_ET_monthly) / sum(MLP$Field_ET_monthly)
maeMOD16 <- mean(abs(MLP$MOD16_monthly - MLP$Field_ET_monthly))

spearman_corrMOD16 <- cor.test(MLP$Field_ET_monthly, MLP$MOD16_monthly, method = "spearman")
print(spearman_corrMOD16)
correlation_coefficientMOD16 <- spearman_corrMOD16$estimate

# GLDAS
kge_valueGLDAS <- KGE(sim = MLP$GLDAS_monthly, obs = MLP$Field_ET_monthly)
rmse_valueGLDAS <- rmse(actual = MLP$Field_ET_monthly, predicted = MLP$GLDAS_monthly)
nse_valueGLDAS <- NSE(sim = MLP$GLDAS_monthly, obs = MLP$Field_ET_monthly)
rsr_valueGLDAS <- rmse_valueGLDAS / sd(MLP$Field_ET_monthly)

biasGLDAS <- mean(MLP$GLDAS_monthly - MLP$Field_ET_monthly)
pbiasGLDAS <- 100 * sum(MLP$GLDAS_monthly - MLP$Field_ET_monthly) / sum(MLP$Field_ET_monthly)
maeGLDAS <- mean(abs(MLP$GLDAS_monthly - MLP$Field_ET_monthly))

spearman_corrGLDAS <- cor.test(MLP$Field_ET_monthly, MLP$GLDAS_monthly, method = "spearman")
print(spearman_corrGLDAS)
correlation_coefficientGLDAS <- spearman_corrGLDAS$estimate

# FLDAS
kge_valueFLDAS <- KGE(sim = MLP$FLDAS_monthly, obs = MLP$Field_ET_monthly)
rmse_valueFLDAS <- rmse(actual = MLP$Field_ET_monthly, predicted = MLP$FLDAS_monthly)
nse_valueFLDAS <- NSE(sim = MLP$FLDAS_monthly, obs = MLP$Field_ET_monthly)
rsr_valueFLDAS <- rmse_valueFLDAS / sd(MLP$Field_ET_monthly)

biasFLDAS <- mean(MLP$FLDAS_monthly - MLP$Field_ET_monthly)
pbiasFLDAS <- 100 * sum(MLP$FLDAS_monthly - MLP$Field_ET_monthly) / sum(MLP$Field_ET_monthly)
maeFLDAS <- mean(abs(MLP$FLDAS_monthly - MLP$Field_ET_monthly))

spearman_corrFLDAS <- cor.test(MLP$Field_ET_monthly, MLP$FLDAS_monthly, method = "spearman")
print(spearman_corrFLDAS)
correlation_coefficientFLDAS <- spearman_corrFLDAS$estimate


df_stats <- data.frame(rsr_valueTC, rsr_valueWaPOR, rsr_valueSMAP, rsr_valueFLDAS, rsr_valueGLDAS, rsr_valueMOD16, rsr_valueSSEBop, 
                       rmse_valueTC, rmse_valueWaPOR, rmse_valueSMAP, rmse_valueFLDAS, rmse_valueGLDAS, rmse_valueMOD16, rmse_valueSSEBop, 
                       pbiasTC, pbiasWaPOR, pbiasSMAP, pbiasFLDAS, pbiasGLDAS, pbiasMOD16, pbiasSSEBop,
                       biasTC, biasWaPOR, biasSMAP, biasFLDAS, biasGLDAS, biasMOD16, biasSSEBop, 
                       maeTC, maeWaPOR, maeSMAP, maeFLDAS, maeGLDAS, maeMOD16, maeSSEBop, 
                       nse_valueTC, nse_valueWaPOR, nse_valueSMAP, nse_valueFLDAS, nse_valueGLDAS, nse_valueMOD16, nse_valueSSEBop, 
                       kge_valueTC, kge_valueWaPOR, kge_valueSMAP, kge_valueFLDAS, kge_valueGLDAS, kge_valueMOD16, kge_valueSSEBop,
                       correlation_coefficientTC, correlation_coefficientWaPOR, correlation_coefficientSMAP, correlation_coefficientFLDAS, correlation_coefficientGLDAS, correlation_coefficientMOD16, correlation_coefficientSSEBop)

# Reshape the data frame from wide to long format
df_long <- df_stats %>%
  pivot_longer(
    cols = everything(),
    names_to = c("metric", "site"),  # Separate columns for metric names and sites
    names_pattern = "^(pbias|nse_value|kge_value|correlation_coefficient)(.*)" # Specify explicit metric names
  ) %>%
  pivot_wider(
    names_from = metric,             # Separate each metric into its own column
    values_from = value
  )

# View the reshaped data
head(df_long)
setwd("C:\\Users\\Liam\\OneDrive - Stellenbosch University\\ET_Data\\Stats")
write.csv(df_stats, file = "Indv_Seasonal_MLP.csv", row.names = FALSE)

###################################################################################################
####################################################################################################
# SKU
# TC 
kge_valueTC <- KGE(sim = SKU$TC_monthly, obs = SKU$Field_ET_monthly)
rmse_valueTC <- rmse(actual = SKU$Field_ET_monthly, predicted = SKU$TC_monthly)
nse_valueTC <- NSE(sim = SKU$TC_monthly, obs = SKU$Field_ET_monthly)
rsr_valueTC <- rmse_valueTC / sd(SKU$Field_ET_monthly)

biasTC <- mean(SKU$TC_monthly - SKU$Field_ET_monthly)
pbiasTC <- 100 * sum(SKU$TC_monthly - SKU$Field_ET_monthly) / sum(SKU$Field_ET_monthly)
maeTC <- mean(abs(SKU$TC_monthly - SKU$Field_ET_monthly))

spearman_corrTC <- cor.test(SKU$Field_ET_monthly, SKU$TC_monthly, method = "spearman")
print(spearman_corrTC)
correlation_coefficientTC <- spearman_corrTC$estimate

# SMAP
kge_valueSMAP <- KGE(sim = SKU$SMAP_monthly, obs = SKU$Field_ET_monthly)
rmse_valueSMAP <- rmse(actual = SKU$Field_ET_monthly, predicted = SKU$SMAP_monthly)
nse_valueSMAP <- NSE(sim = SKU$SMAP_monthly, obs = SKU$Field_ET_monthly)
rsr_valueSMAP <- rmse_valueSMAP / sd(SKU$Field_ET_monthly)

biasSMAP <- mean(SKU$SMAP_monthly - SKU$Field_ET_monthly)
pbiasSMAP <- 100 * sum(SKU$SMAP_monthly - SKU$Field_ET_monthly) / sum(SKU$Field_ET_monthly)
maeSMAP <- mean(abs(SKU$SMAP_monthly - SKU$Field_ET_monthly))

spearman_corrSMAP <- cor.test(SKU$Field_ET_monthly, SKU$SMAP_monthly, method = "spearman")
print(spearman_corrSMAP)
correlation_coefficientSMAP <- spearman_corrSMAP$estimate

# SSEBop
kge_valueSSEBop <- KGE(sim = SKU$SSEBop_monthly, obs = SKU$Field_ET_monthly)
rmse_valueSSEBop <- rmse(actual = SKU$Field_ET_monthly[!is.na(SKU$Field_ET_monthly) & !is.na(SKU$SSEBop_monthly)], 
                         predicted = SKU$SSEBop_monthly[!is.na(SKU$Field_ET_monthly) & !is.na(SKU$SSEBop_monthly)])
nse_valueSSEBop <- NSE(sim = SKU$SSEBop_monthly, obs = SKU$Field_ET_monthly)
rsr_valueSSEBop <- rmse_valueSSEBop / sd(SKU$Field_ET_monthly, na.rm = TRUE)

biasSSEBop <- mean(SKU$SSEBop_monthly - SKU$Field_ET_monthly, na.rm = TRUE)
pbiasSSEBop <- 100 * sum(SKU$SSEBop_monthly - SKU$Field_ET_monthly, na.rm = TRUE) / sum(SKU$Field_ET_monthly, na.rm = TRUE)
maeSSEBop <- mean(abs(SKU$SSEBop_monthly - SKU$Field_ET_monthly), na.rm = TRUE)

spearman_corrSSEBop <- cor.test(SKU$Field_ET_monthly, SKU$SSEBop_monthly, method = "spearman")
print(spearman_corrSSEBop)
correlation_coefficientSSEBop <- spearman_corrSSEBop$estimate

# WAPOR
kge_valueWaPOR <- KGE(sim = SKU$WaPOR_monthly, obs = SKU$Field_ET_monthly)
rmse_valueWaPOR <- rmse(actual = SKU$Field_ET_monthly[!is.na(SKU$Field_ET_monthly) & !is.na(SKU$WaPOR_monthly)],
                        predicted = SKU$WaPOR_monthly[!is.na(SKU$Field_ET_monthly) & !is.na(SKU$WaPOR_monthly)])
nse_valueWaPOR <- NSE(sim = SKU$WaPOR_monthly, obs = SKU$Field_ET_monthly)
rsr_valueWaPOR <- rmse_valueWaPOR / sd(SKU$Field_ET_monthly, na.rm = TRUE)

biasWaPOR <- mean(SKU$WaPOR_monthly - SKU$Field_ET_monthly, na.rm = TRUE)
pbiasWaPOR <- 100 * sum(SKU$WaPOR_monthly - SKU$Field_ET_monthly, na.rm = TRUE) / sum(SKU$Field_ET_monthly, na.rm = TRUE)
maeWaPOR <- mean(abs(SKU$WaPOR_monthly - SKU$Field_ET_monthly), na.rm = TRUE)

spearman_corrWaPOR <- cor.test(SKU$Field_ET_monthly, SKU$WaPOR_monthly, method = "spearman")
print(spearman_corrWaPOR)
correlation_coefficientWaPOR <- spearman_corrWaPOR$estimate

# MOD16
kge_valueMOD16 <- KGE(sim = SKU$MOD16_monthly, obs = SKU$Field_ET_monthly)
rmse_valueMOD16 <- rmse(actual = SKU$Field_ET_monthly, predicted = SKU$MOD16_monthly)
nse_valueMOD16 <- NSE(sim = SKU$MOD16_monthly, obs = SKU$Field_ET_monthly)
rsr_valueMOD16 <- rmse_valueMOD16 / sd(SKU$Field_ET_monthly)

biasMOD16 <- mean(SKU$MOD16_monthly - SKU$Field_ET_monthly)
pbiasMOD16 <- 100 * sum(SKU$MOD16_monthly - SKU$Field_ET_monthly) / sum(SKU$Field_ET_monthly)
maeMOD16 <- mean(abs(SKU$MOD16_monthly - SKU$Field_ET_monthly))

spearman_corrMOD16 <- cor.test(SKU$Field_ET_monthly, SKU$MOD16_monthly, method = "spearman")
print(spearman_corrMOD16)
correlation_coefficientMOD16 <- spearman_corrMOD16$estimate

# GLDAS
kge_valueGLDAS <- KGE(sim = SKU$GLDAS_monthly, obs = SKU$Field_ET_monthly)
rmse_valueGLDAS <- rmse(actual = SKU$Field_ET_monthly, predicted = SKU$GLDAS_monthly)
nse_valueGLDAS <- NSE(sim = SKU$GLDAS_monthly, obs = SKU$Field_ET_monthly)
rsr_valueGLDAS <- rmse_valueGLDAS / sd(SKU$Field_ET_monthly)

biasGLDAS <- mean(SKU$GLDAS_monthly - SKU$Field_ET_monthly)
pbiasGLDAS <- 100 * sum(SKU$GLDAS_monthly - SKU$Field_ET_monthly) / sum(SKU$Field_ET_monthly)
maeGLDAS <- mean(abs(SKU$GLDAS_monthly - SKU$Field_ET_monthly))

spearman_corrGLDAS <- cor.test(SKU$Field_ET_monthly, SKU$GLDAS_monthly, method = "spearman")
print(spearman_corrGLDAS)
correlation_coefficientGLDAS <- spearman_corrGLDAS$estimate

# FLDAS
kge_valueFLDAS <- KGE(sim = SKU$FLDAS_monthly, obs = SKU$Field_ET_monthly)
rmse_valueFLDAS <- rmse(actual = SKU$Field_ET_monthly, predicted = SKU$FLDAS_monthly)
nse_valueFLDAS <- NSE(sim = SKU$FLDAS_monthly, obs = SKU$Field_ET_monthly)
rsr_valueFLDAS <- rmse_valueFLDAS / sd(SKU$Field_ET_monthly)

biasFLDAS <- mean(SKU$FLDAS_monthly - SKU$Field_ET_monthly)
pbiasFLDAS <- 100 * sum(SKU$FLDAS_monthly - SKU$Field_ET_monthly) / sum(SKU$Field_ET_monthly)
maeFLDAS <- mean(abs(SKU$FLDAS_monthly - SKU$Field_ET_monthly))

spearman_corrFLDAS <- cor.test(SKU$Field_ET_monthly, SKU$FLDAS_monthly, method = "spearman")
print(spearman_corrFLDAS)
correlation_coefficientFLDAS <- spearman_corrFLDAS$estimate

df_stats <- data.frame(rsr_valueTC, rsr_valueWaPOR, rsr_valueSMAP, rsr_valueFLDAS, rsr_valueGLDAS, rsr_valueMOD16, rsr_valueSSEBop,
                       rmse_valueTC, rmse_valueWaPOR, rmse_valueSMAP, rmse_valueFLDAS, rmse_valueGLDAS, rmse_valueMOD16, rmse_valueSSEBop,
                       pbiasTC, pbiasWaPOR, pbiasSMAP, pbiasFLDAS, pbiasGLDAS, pbiasMOD16, pbiasSSEBop,
                       biasTC, biasWaPOR, biasSMAP, biasFLDAS, biasGLDAS, biasMOD16, biasSSEBop, 
                       maeTC, maeWaPOR, maeSMAP, maeFLDAS, maeGLDAS, maeMOD16, maeSSEBop, 
                       nse_valueTC, nse_valueWaPOR, nse_valueSMAP, nse_valueFLDAS, nse_valueGLDAS, nse_valueMOD16, nse_valueSSEBop, 
                       kge_valueTC, kge_valueWaPOR, kge_valueSMAP, kge_valueFLDAS, kge_valueGLDAS, kge_valueMOD16, kge_valueSSEBop, 
                       correlation_coefficientTC, correlation_coefficientWaPOR, correlation_coefficientSMAP, correlation_coefficientFLDAS, correlation_coefficientGLDAS, correlation_coefficientMOD16, correlation_coefficientSSEBop)

# Reshape the data frame from wide to long format
df_long <- df_stats %>%
  pivot_longer(
    cols = everything(),
    names_to = c("metric", "site"),  # Separate columns for metric names and sites
    names_pattern = "^(pbias|nse_value|kge_value|correlation_coefficient)(.*)" # Specify explicit metric names
  ) %>%
  pivot_wider(
    names_from = metric,             # Separate each metric into its own column
    values_from = value
  )

# View the reshaped data
head(df_long)
setwd("C:\\Users\\Liam\\OneDrive - Stellenbosch University\\ET_Data\\Stats")
write.csv(df_stats, file = "Indv_Seasonal_SKU.csv", row.names = FALSE)
