# Ensemble stats - OVERALL & LOG TRANSFORMED

# Load libraries
library(tidyverse)
library(plotly)
library(ggplot2)
library(hydroGOF) 

# Load the dataset
setwd("C:\\Users\\Liam\\OneDrive - Stellenbosch University\\ET_Data\\Ensemble_Data")

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

#  BF1
# Perform Spearman Correlation analysis: 
# BF1
BF1 <- BF1 %>%
  dplyr::rename(ensemble_min = min_ET, ensemble_max = max_ET, ensemble_mean = mean_ET)
head(BF1)

FieldBF1_vs_Ensemble <- BF1[, c('ensemble_mean', 'field_et')]
Field_BF1_filtered <- na.omit(FieldBF1_vs_Ensemble)
Field_BF1_filtered <- data.frame(lapply(Field_BF1_filtered, as.numeric))

shapiro_test_result <- shapiro.test(Field_BF1_filtered$ensemble_mean)
print(shapiro_test_result)

spearman_corrBF1 <- cor.test(Field_BF1_filtered$field_et, Field_BF1_filtered$ensemble_mean, method = "spearman")
print(spearman_corrBF1)
correlation_coefficientBF1 <- spearman_corrBF1$estimate
p_valueBF1 <- spearman_corrBF1$p.value

kge_valueBF1 <- KGE(sim = Field_BF1_filtered$ensemble_mean, obs = Field_BF1_filtered$field_et)
rmse_valueBF1 <- rmse(actual = Field_BF1_filtered$field_et, predicted = Field_BF1_filtered$ensemble_mean)
nse_valueBF1 <- NSE(sim = Field_BF1_filtered$ensemble_mean, obs = Field_BF1_filtered$field_et)
rsr_valueBF1 <- rmse_valueBF1 / sd(Field_BF1_filtered$field_et)

biasBF1 <- mean(Field_BF1_filtered$ensemble_mean - Field_BF1_filtered$field_et)
pbiasBF1 <- 100 * sum(Field_BF1_filtered$ensemble_mean - Field_BF1_filtered$field_et) / sum(Field_BF1_filtered$field_et)
maeBF1 <- mean(abs(Field_BF1_filtered$ensemble_mean - Field_BF1_filtered$field_et))
################################################################################

#  BF2
BF2 <- BF2 %>%
  
  dplyr::rename(ensemble_min = min_ET, ensemble_max = max_ET, ensemble_mean = mean_ET)
head(BF2)

# Perform Spearman Correlation analysis: 
# BF2
FieldBF2_vs_Ensemble <- BF2[, c('ensemble_mean', 'field_et')]
Field_BF2_filtered <- na.omit(FieldBF2_vs_Ensemble)
Field_BF2_filtered <- data.frame(lapply(Field_BF2_filtered, as.numeric))

shapiro_test_result <- shapiro.test(Field_BF2_filtered$ensemble_mean)
print(shapiro_test_result)

spearman_corrBF2 <- cor.test(Field_BF2_filtered$field_et, Field_BF2_filtered$ensemble_mean, method = "spearman")
print(spearman_corrBF2)
correlation_coefficientBF2 <- spearman_corrBF2$estimate
p_valueBF2 <- spearman_corrBF2$p.value

kge_valueBF2 <- KGE(sim = Field_BF2_filtered$ensemble_mean, obs = Field_BF2_filtered$field_et)
rmse_valueBF2 <- rmse(actual = Field_BF2_filtered$field_et, predicted = Field_BF2_filtered$ensemble_mean)
nse_valueBF2 <- NSE(sim = Field_BF2_filtered$ensemble_mean, obs = Field_BF2_filtered$field_et)
rsr_valueBF2 <- rmse_valueBF2 / sd(Field_BF2_filtered$field_et)

biasBF2 <- mean(Field_BF2_filtered$ensemble_mean - Field_BF2_filtered$field_et)
pbiasBF2 <- 100 * sum(Field_BF2_filtered$ensemble_mean - Field_BF2_filtered$field_et) / sum(Field_BF2_filtered$field_et)
maeBF2 <- mean(abs(Field_BF2_filtered$ensemble_mean - Field_BF2_filtered$field_et))
################################################################################
#  CP3
CP3 <- CP3 %>%
  dplyr::rename(ensemble_min = min_ET, ensemble_max = max_ET, ensemble_mean = mean_ET)
head(CP3)

# Perform Spearman Correlation analysis: 
# CP3
FieldCP3_vs_Ensemble <- CP3[, c('ensemble_mean', 'field_et')]
Field_CP3_filtered <- na.omit(FieldCP3_vs_Ensemble)
Field_CP3_filtered <- data.frame(lapply(Field_CP3_filtered, as.numeric))

shapiro_test_result <- shapiro.test(Field_CP3_filtered$ensemble_mean)
print(shapiro_test_result)

spearman_corrCP3 <- cor.test(Field_CP3_filtered$field_et, Field_CP3_filtered$ensemble_mean, method = "spearman")
print(spearman_corrCP3)
correlation_coefficientCP3 <- spearman_corrCP3$estimate
p_valueCP3 <- spearman_corrCP3$p.value

kge_valueCP3 <- KGE(sim = Field_CP3_filtered$ensemble_mean, obs = Field_CP3_filtered$field_et)
rmse_valueCP3 <- rmse(actual = Field_CP3_filtered$field_et, predicted = Field_CP3_filtered$ensemble_mean)
nse_valueCP3 <- NSE(sim = Field_CP3_filtered$ensemble_mean, obs = Field_CP3_filtered$field_et)
rsr_valueCP3 <- rmse_valueCP3 / sd(Field_CP3_filtered$field_et)

biasCP3 <- mean(Field_CP3_filtered$ensemble_mean - Field_CP3_filtered$field_et)
pbiasCP3 <- 100 * sum(Field_CP3_filtered$ensemble_mean - Field_CP3_filtered$field_et) / sum(Field_CP3_filtered$field_et)
maeCP3 <- mean(abs(Field_CP3_filtered$ensemble_mean - Field_CP3_filtered$field_et))
################################################################################
#  CP6
CP6 <- CP6 %>%
  dplyr::rename(ensemble_min = min_ET, ensemble_max = max_ET, ensemble_mean = mean_ET)
head(CP6)

# Perform Spearman Correlation analysis: 
# CP6
FieldCP6_vs_Ensemble <- CP6[, c('ensemble_mean', 'field_et')]
Field_CP6_filtered <- na.omit(FieldCP6_vs_Ensemble)
Field_CP6_filtered <- data.frame(lapply(Field_CP6_filtered, as.numeric))

shapiro_test_result <- shapiro.test(Field_CP6_filtered$ensemble_mean)
print(shapiro_test_result)

spearman_corrCP6 <- cor.test(Field_CP6_filtered$field_et, Field_CP6_filtered$ensemble_mean, method = "spearman")
print(spearman_corrCP6)
correlation_coefficientCP6 <- spearman_corrCP6$estimate
p_valueCP6 <- spearman_corrCP6$p.value

kge_valueCP6 <- KGE(sim = Field_CP6_filtered$ensemble_mean, obs = Field_CP6_filtered$field_et)
rmse_valueCP6 <- rmse(actual = Field_CP6_filtered$field_et, predicted = Field_CP6_filtered$ensemble_mean)
nse_valueCP6 <- NSE(sim = Field_CP6_filtered$ensemble_mean, obs = Field_CP6_filtered$field_et)
rsr_valueCP6 <- rmse_valueCP6 / sd(Field_CP6_filtered$field_et)

biasCP6 <- mean(Field_CP6_filtered$ensemble_mean - Field_CP6_filtered$field_et)
pbiasCP6 <- 100 * sum(Field_CP6_filtered$ensemble_mean - Field_CP6_filtered$field_et) / sum(Field_CP6_filtered$field_et)
maeCP6 <- mean(abs(Field_CP6_filtered$ensemble_mean - Field_CP6_filtered$field_et))
################################################################################
#  CP9
CP9 <- CP9 %>%
  dplyr::rename(ensemble_min = min_ET, ensemble_max = max_ET, ensemble_mean = mean_ET)
head(CP9)

# Perform Spearman Correlation analysis: 
# CP9
FieldCP9_vs_Ensemble <- CP9[, c('ensemble_mean', 'field_et')]
Field_CP9_filtered <- na.omit(FieldCP9_vs_Ensemble)
Field_CP9_filtered <- data.frame(lapply(Field_CP9_filtered, as.numeric))

shapiro_test_result <- shapiro.test(Field_CP9_filtered$ensemble_mean)
print(shapiro_test_result)

spearman_corrCP9 <- cor.test(Field_CP9_filtered$field_et, Field_CP9_filtered$ensemble_mean, method = "spearman")
print(spearman_corrCP9)
correlation_coefficientCP9 <- spearman_corrCP9$estimate
p_valueCP9 <- spearman_corrCP9$p.value

kge_valueCP9 <- KGE(sim = Field_CP9_filtered$ensemble_mean, obs = Field_CP9_filtered$field_et)
rmse_valueCP9 <- rmse(actual = Field_CP9_filtered$field_et, predicted = Field_CP9_filtered$ensemble_mean)
nse_valueCP9 <- NSE(sim = Field_CP9_filtered$ensemble_mean, obs = Field_CP9_filtered$field_et)
rsr_valueCP9 <- rmse_valueCP9 / sd(Field_CP9_filtered$field_et)

biasCP9 <- mean(Field_CP9_filtered$ensemble_mean - Field_CP9_filtered$field_et)
pbiasCP9 <- 100 * sum(Field_CP9_filtered$ensemble_mean - Field_CP9_filtered$field_et) / sum(Field_CP9_filtered$field_et)
maeCP9 <- mean(abs(Field_CP9_filtered$ensemble_mean - Field_CP9_filtered$field_et))
################################################################################
#  EW1
EW1 <- EW1 %>%
  dplyr::rename(ensemble_min = min_ET, ensemble_max = max_ET, ensemble_mean = mean_ET)
head(EW1)

# Perform Spearman Correlation analysis: 
# EW1
FieldEW1_vs_Ensemble <- EW1[, c('ensemble_mean', 'field_et')]
Field_EW1_filtered <- na.omit(FieldEW1_vs_Ensemble)
Field_EW1_filtered <- data.frame(lapply(Field_EW1_filtered, as.numeric))

shapiro_test_result <- shapiro.test(Field_EW1_filtered$ensemble_mean)
print(shapiro_test_result)

spearman_corrEW1 <- cor.test(Field_EW1_filtered$field_et, Field_EW1_filtered$ensemble_mean, method = "spearman")
print(spearman_corrEW1)
correlation_coefficientEW1 <- spearman_corrEW1$estimate
p_valueEW1 <- spearman_corrEW1$p.value

kge_valueEW1 <- KGE(sim = Field_EW1_filtered$ensemble_mean, obs = Field_EW1_filtered$field_et)
rmse_valueEW1 <- rmse(actual = Field_EW1_filtered$field_et, predicted = Field_EW1_filtered$ensemble_mean)
nse_valueEW1 <- NSE(sim = Field_EW1_filtered$ensemble_mean, obs = Field_EW1_filtered$field_et)
rsr_valueEW1 <- rmse_valueEW1 / sd(Field_EW1_filtered$field_et)

biasEW1 <- mean(Field_EW1_filtered$ensemble_mean - Field_EW1_filtered$field_et)
pbiasEW1 <- 100 * sum(Field_EW1_filtered$ensemble_mean - Field_EW1_filtered$field_et) / sum(Field_EW1_filtered$field_et)
maeEW1 <- mean(abs(Field_EW1_filtered$ensemble_mean - Field_EW1_filtered$field_et))
################################################################################
#  EW2
EW2 <- EW2 %>%
  dplyr::rename(ensemble_min = min_ET, ensemble_max = max_ET, ensemble_mean = mean_ET)
head(EW2)

# Perform Spearman Correlation analysis: 
# EW2
FieldEW2_vs_Ensemble <- EW2[, c('ensemble_mean', 'field_et')]
Field_EW2_filtered <- na.omit(FieldEW2_vs_Ensemble)
Field_EW2_filtered <- data.frame(lapply(Field_EW2_filtered, as.numeric))

shapiro_test_result <- shapiro.test(Field_EW2_filtered$ensemble_mean)
print(shapiro_test_result)

spearman_corrEW2 <- cor.test(Field_EW2_filtered$field_et, Field_EW2_filtered$ensemble_mean, method = "spearman")
print(spearman_corrEW2)
correlation_coefficientEW2 <- spearman_corrEW2$estimate
p_valueEW2 <- spearman_corrEW2$p.value

kge_valueEW2 <- KGE(sim = Field_EW2_filtered$ensemble_mean, obs = Field_EW2_filtered$field_et)
rmse_valueEW2 <- rmse(actual = Field_EW2_filtered$field_et, predicted = Field_EW2_filtered$ensemble_mean)
nse_valueEW2 <- NSE(sim = Field_EW2_filtered$ensemble_mean, obs = Field_EW2_filtered$field_et)
rsr_valueEW2 <- rmse_valueEW2 / sd(Field_EW2_filtered$field_et)

biasEW2 <- mean(Field_EW2_filtered$ensemble_mean - Field_EW2_filtered$field_et)
pbiasEW2 <- 100 * sum(Field_EW2_filtered$ensemble_mean - Field_EW2_filtered$field_et) / sum(Field_EW2_filtered$field_et)
maeEW2 <- mean(abs(Field_EW2_filtered$ensemble_mean - Field_EW2_filtered$field_et))
################################################################################

#  EZU
EZU <- EZU %>%
  dplyr::rename(ensemble_min = min_ET, ensemble_max = max_ET, ensemble_mean = mean_ET)
head(EZU)

# Perform Spearman Correlation analysis: 
# EZU
FieldEZU_vs_Ensemble <- EZU[, c('ensemble_mean', 'field_et')]
Field_EZU_filtered <- na.omit(FieldEZU_vs_Ensemble)
Field_EZU_filtered <- data.frame(lapply(Field_EZU_filtered, as.numeric))

shapiro_test_result <- shapiro.test(Field_EZU_filtered$ensemble_mean)
print(shapiro_test_result)

spearman_corrEZU <- cor.test(Field_EZU_filtered$field_et, Field_EZU_filtered$ensemble_mean, method = "spearman")
print(spearman_corrEZU)
correlation_coefficientEZU <- spearman_corrEZU$estimate
p_valueEZU <- spearman_corrEZU$p.value

kge_valueEZU <- KGE(sim = Field_EZU_filtered$ensemble_mean, obs = Field_EZU_filtered$field_et)
rmse_valueEZU <- rmse(actual = Field_EZU_filtered$field_et, predicted = Field_EZU_filtered$ensemble_mean)
nse_valueEZU <- NSE(sim = Field_EZU_filtered$ensemble_mean, obs = Field_EZU_filtered$field_et)
rsr_valueEZU <- rmse_valueEZU / sd(Field_EZU_filtered$field_et)

biasEZU <- mean(Field_EZU_filtered$ensemble_mean - Field_EZU_filtered$field_et)
pbiasEZU <- 100 * sum(Field_EZU_filtered$ensemble_mean - Field_EZU_filtered$field_et) / sum(Field_EZU_filtered$field_et)
maeEZU <- mean(abs(Field_EZU_filtered$ensemble_mean - Field_EZU_filtered$field_et))
################################################################################

#  JHK
JHK <- JHK %>%
  dplyr::rename(ensemble_min = min_ET, ensemble_max = max_ET, ensemble_mean = mean_ET)
head(JHK)

# Perform Spearman Correlation analysis: 
# JHK
FieldJHK_vs_Ensemble <- JHK[, c('ensemble_mean', 'field_et')]
Field_JHK_filtered <- na.omit(FieldJHK_vs_Ensemble)
Field_JHK_filtered <- data.frame(lapply(Field_JHK_filtered, as.numeric))

shapiro_test_result <- shapiro.test(Field_JHK_filtered$ensemble_mean)
print(shapiro_test_result)

spearman_corrJHK <- cor.test(Field_JHK_filtered$field_et, Field_JHK_filtered$ensemble_mean, method = "spearman")
print(spearman_corrJHK)
correlation_coefficientJHK <- spearman_corrJHK$estimate
p_valueJHK <- spearman_corrJHK$p.value

kge_valueJHK <- KGE(sim = Field_JHK_filtered$ensemble_mean, obs = Field_JHK_filtered$field_et)
rmse_valueJHK <- rmse(actual = Field_JHK_filtered$field_et, predicted = Field_JHK_filtered$ensemble_mean)
nse_valueJHK <- NSE(sim = Field_JHK_filtered$ensemble_mean, obs = Field_JHK_filtered$field_et)
rsr_valueJHK <- rmse_valueJHK / sd(Field_JHK_filtered$field_et)

biasJHK <- mean(Field_JHK_filtered$ensemble_mean - Field_JHK_filtered$field_et)
pbiasJHK <- 100 * sum(Field_JHK_filtered$ensemble_mean - Field_JHK_filtered$field_et) / sum(Field_JHK_filtered$field_et)
maeJHK <- mean(abs(Field_JHK_filtered$ensemble_mean - Field_JHK_filtered$field_et))
################################################################################

#  MB1
MB1 <- MB1 %>%
  dplyr::rename(ensemble_min = min_ET, ensemble_max = max_ET, ensemble_mean = mean_ET)
head(MB1)

# Perform Spearman Correlation analysis: 
# MB1
FieldMB1_vs_Ensemble <- MB1[, c('ensemble_mean', 'field_et')]
Field_MB1_filtered <- na.omit(FieldMB1_vs_Ensemble)
Field_MB1_filtered <- data.frame(lapply(Field_MB1_filtered, as.numeric))

shapiro_test_result <- shapiro.test(Field_MB1_filtered$ensemble_mean)
print(shapiro_test_result)

spearman_corrMB1 <- cor.test(Field_MB1_filtered$field_et, Field_MB1_filtered$ensemble_mean, method = "spearman")
print(spearman_corrMB1)
correlation_coefficientMB1 <- spearman_corrMB1$estimate
p_valueMB1 <- spearman_corrMB1$p.value

kge_valueMB1 <- KGE(sim = Field_MB1_filtered$ensemble_mean, obs = Field_MB1_filtered$field_et)
rmse_valueMB1 <- rmse(actual = Field_MB1_filtered$field_et, predicted = Field_MB1_filtered$ensemble_mean)
nse_valueMB1 <- NSE(sim = Field_MB1_filtered$ensemble_mean, obs = Field_MB1_filtered$field_et)
rsr_valueMB1 <- rmse_valueMB1 / sd(Field_MB1_filtered$field_et)

biasMB1 <- mean(Field_MB1_filtered$ensemble_mean - Field_MB1_filtered$field_et)
pbiasMB1 <- 100 * sum(Field_MB1_filtered$ensemble_mean - Field_MB1_filtered$field_et) / sum(Field_MB1_filtered$field_et)
maeMB1 <- mean(abs(Field_MB1_filtered$ensemble_mean - Field_MB1_filtered$field_et))
################################################################################

#  MB2
MB2 <- MB2 %>%
  dplyr::rename(ensemble_min = min_ET, ensemble_max = max_ET, ensemble_mean = mean_ET)
head(MB2)

# Perform Spearman Correlation analysis: 
# MB2
FieldMB2_vs_Ensemble <- MB2[, c('ensemble_mean', 'field_et')]
Field_MB2_filtered <- na.omit(FieldMB2_vs_Ensemble)
Field_MB2_filtered <- data.frame(lapply(Field_MB2_filtered, as.numeric))

shapiro_test_result <- shapiro.test(Field_MB2_filtered$ensemble_mean)
print(shapiro_test_result)

spearman_corrMB2 <- cor.test(Field_MB2_filtered$field_et, Field_MB2_filtered$ensemble_mean, method = "spearman")
print(spearman_corrMB2)
correlation_coefficientMB2 <- spearman_corrMB2$estimate
p_valueMB2 <- spearman_corrMB2$p.value

kge_valueMB2 <- KGE(sim = Field_MB2_filtered$ensemble_mean, obs = Field_MB2_filtered$field_et)
rmse_valueMB2 <- rmse(actual = Field_MB2_filtered$field_et, predicted = Field_MB2_filtered$ensemble_mean)
nse_valueMB2 <- NSE(sim = Field_MB2_filtered$ensemble_mean, obs = Field_MB2_filtered$field_et)
rsr_valueMB2 <- rmse_valueMB2 / sd(Field_MB2_filtered$field_et)

biasMB2 <- mean(Field_MB2_filtered$ensemble_mean - Field_MB2_filtered$field_et)
pbiasMB2 <- 100 * sum(Field_MB2_filtered$ensemble_mean - Field_MB2_filtered$field_et) / sum(Field_MB2_filtered$field_et)
maeMB2 <- mean(abs(Field_MB2_filtered$ensemble_mean - Field_MB2_filtered$field_et))
################################################################################

#  MCP
MCP <- MCP %>%
  dplyr::rename(ensemble_min = min_ET, ensemble_max = max_ET, ensemble_mean = mean_ET)
head(MCP)

# Perform Spearman Correlation analysis: 
# MCP
FieldMCP_vs_Ensemble <- MCP[, c('ensemble_mean', 'field_et')]
Field_MCP_filtered <- na.omit(FieldMCP_vs_Ensemble)
Field_MCP_filtered <- data.frame(lapply(Field_MCP_filtered, as.numeric))

shapiro_test_result <- shapiro.test(Field_MCP_filtered$ensemble_mean)
print(shapiro_test_result)

spearman_corrMCP <- cor.test(Field_MCP_filtered$field_et, Field_MCP_filtered$ensemble_mean, method = "spearman")
print(spearman_corrMCP)
correlation_coefficientMCP <- spearman_corrMCP$estimate
p_valueMCP <- spearman_corrMCP$p.value

kge_valueMCP <- KGE(sim = Field_MCP_filtered$ensemble_mean, obs = Field_MCP_filtered$field_et)
rmse_valueMCP <- rmse(actual = Field_MCP_filtered$field_et, predicted = Field_MCP_filtered$ensemble_mean)
nse_valueMCP <- NSE(sim = Field_MCP_filtered$ensemble_mean, obs = Field_MCP_filtered$field_et)
rsr_valueMCP <- rmse_valueMCP / sd(Field_MCP_filtered$field_et)

biasMCP <- mean(Field_MCP_filtered$ensemble_mean - Field_MCP_filtered$field_et)
pbiasMCP <- 100 * sum(Field_MCP_filtered$ensemble_mean - Field_MCP_filtered$field_et) / sum(Field_MCP_filtered$field_et)
maeMCP <- mean(abs(Field_MCP_filtered$ensemble_mean - Field_MCP_filtered$field_et))
################################################################################

#  MLP
MLP <- MLP %>%
  dplyr::rename(ensemble_min = min_ET, ensemble_max = max_ET, ensemble_mean = mean_ET)
head(MLP)

# Perform Spearman Correlation analysis: 
# MLP
FieldMLP_vs_Ensemble <- MLP[, c('ensemble_mean', 'field_et')]
Field_MLP_filtered <- na.omit(FieldMLP_vs_Ensemble)
Field_MLP_filtered <- data.frame(lapply(Field_MLP_filtered, as.numeric))

shapiro_test_result <- shapiro.test(Field_MLP_filtered$ensemble_mean)
print(shapiro_test_result)

spearman_corrMLP <- cor.test(Field_MLP_filtered$field_et, Field_MLP_filtered$ensemble_mean, method = "spearman")
print(spearman_corrMLP)
correlation_coefficientMLP <- spearman_corrMLP$estimate
p_valueMLP <- spearman_corrMLP$p.value

kge_valueMLP <- KGE(sim = Field_MLP_filtered$ensemble_mean, obs = Field_MLP_filtered$field_et)
rmse_valueMLP <- rmse(actual = Field_MLP_filtered$field_et, predicted = Field_MLP_filtered$ensemble_mean)
nse_valueMLP <- NSE(sim = Field_MLP_filtered$ensemble_mean, obs = Field_MLP_filtered$field_et)
rsr_valueMLP <- rmse_valueMLP / sd(Field_MLP_filtered$field_et)

biasMLP <- mean(Field_MLP_filtered$ensemble_mean - Field_MLP_filtered$field_et)
pbiasMLP <- 100 * sum(Field_MLP_filtered$ensemble_mean - Field_MLP_filtered$field_et) / sum(Field_MLP_filtered$field_et)
maeMLP <- mean(abs(Field_MLP_filtered$ensemble_mean - Field_MLP_filtered$field_et))
################################################################################

#  SKU
SKU <- SKU %>%
  dplyr::rename(ensemble_min = min_ET, ensemble_max = max_ET, ensemble_mean = mean_ET)
head(SKU)

# Perform Spearman Correlation analysis: 
# SKU
FieldSKU_vs_Ensemble <- SKU[, c('ensemble_mean', 'field_et')]
Field_SKU_filtered <- na.omit(FieldSKU_vs_Ensemble)
Field_SKU_filtered <- data.frame(lapply(Field_SKU_filtered, as.numeric))

shapiro_test_result <- shapiro.test(Field_SKU_filtered$ensemble_mean)
print(shapiro_test_result)

spearman_corrSKU <- cor.test(Field_SKU_filtered$field_et, Field_SKU_filtered$ensemble_mean, method = "spearman")
print(spearman_corrSKU)
correlation_coefficientSKU <- spearman_corrSKU$estimate
p_valueSKU <- spearman_corrSKU$p.value

kge_valueSKU <- KGE(sim = Field_SKU_filtered$ensemble_mean, obs = Field_SKU_filtered$field_et)
rmse_valueSKU <- rmse(actual = Field_SKU_filtered$field_et, predicted = Field_SKU_filtered$ensemble_mean)
nse_valueSKU <- NSE(sim = Field_SKU_filtered$ensemble_mean, obs = Field_SKU_filtered$field_et)
rsr_valueSKU <- rmse_valueSKU / sd(Field_SKU_filtered$field_et)

biasSKU <- mean(Field_SKU_filtered$ensemble_mean - Field_SKU_filtered$field_et)
pbiasSKU <- 100 * sum(Field_SKU_filtered$ensemble_mean - Field_SKU_filtered$field_et) / sum(Field_SKU_filtered$field_et)
maeSKU <- mean(abs(Field_SKU_filtered$ensemble_mean - Field_SKU_filtered$field_et))

################################################################################

# Linear R scatterplots + R2 (coeff of determination)

# Scatter plots - Field vs satellite
max_limit <- 150

# BF1
plot(Field_BF1_filtered$field_et, Field_BF1_filtered$ensemble_mean, main="BF1 - Field-measured ET vs Satellite-derived ET", xlab="Field-measured ET (mm/month)", ylab="Ensemble ET (mm/month)", col = "red", 
     xlim = c(0, max_limit),  # Setting x-axis limits from 0 to max_limit
     ylim = c(0, max_limit))  # Setting y-axis limits from 0 to max_limit)))

fit1 <- lm(Field_BF1_filtered$ensemble_mean ~ Field_BF1_filtered$field_et)
abline(fit1, col="red")

# BF2
plot(Field_BF2_filtered$field_et, Field_BF2_filtered$ensemble_mean, main="BF2 - Field-measured ET vs Satellite-derived ET", xlab="Field-measured ET (mm/month)", ylab="Ensemble ET (mm/month)", col = "red", 
     xlim = c(0, max_limit),  # Setting x-axis limits from 0 to max_limit
     ylim = c(0, max_limit))  # Setting y-axis limits from 0 to max_limit)))

fit2 <- lm(Field_BF2_filtered$ensemble_mean ~ Field_BF2_filtered$field_et)
abline(fit2, col="red")

# CP3
plot(Field_CP3_filtered$field_et, Field_CP3_filtered$ensemble_mean, main="CP3 - Field-measured ET vs Satellite-derived ET", xlab="Field-measured ET (mm/month)", ylab="Ensemble ET (mm/month)", col = "red", 
     xlim = c(0, max_limit),  # Setting x-axis limits from 0 to max_limit
     ylim = c(0, max_limit))  # Setting y-axis limits from 0 to max_limit)))

fit3 <- lm(Field_CP3_filtered$ensemble_mean ~ Field_CP3_filtered$field_et)
abline(fit3, col="red")


# CP6
plot(Field_CP6_filtered$field_et, Field_CP6_filtered$ensemble_mean, main="CP6 - Field-measured ET vs Satellite-derived ET", xlab="Field-measured ET (mm/month)", ylab="Ensemble ET (mm/month)", col = "red", 
     xlim = c(0, max_limit),  # Setting x-axis limits from 0 to max_limit
     ylim = c(0, max_limit))  # Setting y-axis limits from 0 to max_limit)))

fit4 <- lm(Field_CP6_filtered$ensemble_mean ~ Field_CP6_filtered$field_et)
abline(fit4, col="red")


# CP9
plot(Field_CP9_filtered$field_et, Field_CP9_filtered$ensemble_mean, main="CP9 - Field-measured ET vs Satellite-derived ET", xlab="Field-measured ET (mm/month)", ylab="Ensemble ET (mm/month)", col = "red", 
     xlim = c(0, max_limit),  # Setting x-axis limits from 0 to max_limit
     ylim = c(0, max_limit))  # Setting y-axis limits from 0 to max_limit)))

fit5 <- lm(Field_CP9_filtered$ensemble_mean ~ Field_CP9_filtered$field_et)
abline(fit5, col="red")

# EW1
plot(Field_EW1_filtered$field_et, Field_EW1_filtered$ensemble_mean, main="EW1 - Field-measured ET vs Satellite-derived ET", xlab="Field-measured ET (mm/month)", ylab="Ensemble ET (mm/month)", col = "red", 
     xlim = c(0, max_limit),  # Setting x-axis limits from 0 to max_limit
     ylim = c(0, max_limit))  # Setting y-axis limits from 0 to max_limit)))

fit6 <- lm(Field_EW1_filtered$ensemble_mean ~ Field_EW1_filtered$field_et)
abline(fit6, col="red")


# EW2
plot(Field_EW2_filtered$field_et, Field_EW2_filtered$ensemble_mean, main="EW2 - Field-measured ET vs Satellite-derived ET", xlab="Field-measured ET (mm/month)", ylab="Ensemble ET (mm/month)", col = "red", 
     xlim = c(0, max_limit),  # Setting x-axis limits from 0 to max_limit
     ylim = c(0, max_limit))  # Setting y-axis limits from 0 to max_limit)))

fit7 <- lm(Field_EW2_filtered$ensemble_mean ~ Field_EW2_filtered$field_et)
abline(fit7, col="red")


# EZU
plot(Field_EZU_filtered$field_et, Field_EZU_filtered$ensemble_mean, main="EZU - Field-measured ET vs Satellite-derived ET", xlab="Field-measured ET (mm/month)", ylab="Ensemble ET (mm/month)", col = "red", 
     xlim = c(0, max_limit),  # Setting x-axis limits from 0 to max_limit
     ylim = c(0, max_limit))  # Setting y-axis limits from 0 to max_limit)))

fit8 <- lm(Field_EZU_filtered$ensemble_mean ~ Field_EZU_filtered$field_et)
abline(fit8, col="red")

# JHK
plot(Field_JHK_filtered$field_et, Field_JHK_filtered$ensemble_mean, main="JHK - Field-measured ET vs Satellite-derived ET", xlab="Field-measured ET (mm/month)", ylab="Ensemble ET (mm/month)", col = "red", 
     xlim = c(0, max_limit),  # Setting x-axis limits from 0 to max_limit
     ylim = c(0, max_limit))  # Setting y-axis limits from 0 to max_limit)))

fit9 <- lm(Field_JHK_filtered$ensemble_mean ~ Field_JHK_filtered$field_et)
abline(fit9, col="red")

# MB1
plot(Field_MB1_filtered$field_et, Field_MB1_filtered$ensemble_mean, main="MB1 - Field-measured ET vs Satellite-derived ET", xlab="Field-measured ET (mm/month)", ylab="Ensemble ET (mm/month)", col = "red", 
     xlim = c(0, max_limit),  # Setting x-axis limits from 0 to max_limit
     ylim = c(0, max_limit))  # Setting y-axis limits from 0 to max_limit)))

fit10 <- lm(Field_MB1_filtered$ensemble_mean ~ Field_MB1_filtered$field_et)
abline(fit10, col="red")


# MB2
plot(Field_MB2_filtered$field_et, Field_MB2_filtered$ensemble_mean, main="MB2 - Field-measured ET vs Satellite-derived ET", xlab="Field-measured ET (mm/month)", ylab="Ensemble ET (mm/month)", col = "red", 
     xlim = c(0, max_limit),  # Setting x-axis limits from 0 to max_limit
     ylim = c(0, max_limit))  # Setting y-axis limits from 0 to max_limit)))

fit11 <- lm(Field_MB2_filtered$ensemble_mean ~ Field_MB2_filtered$field_et)
abline(fit11, col="red")

# MCP
plot(Field_MCP_filtered$field_et, Field_MCP_filtered$ensemble_mean, main="MCP - Field-measured ET vs Satellite-derived ET", xlab="Field-measured ET (mm/month)", ylab="Ensemble ET (mm/month)", col = "red", 
     xlim = c(0, max_limit),  # Setting x-axis limits from 0 to max_limit
     ylim = c(0, max_limit))  # Setting y-axis limits from 0 to max_limit)))

fit12 <- lm(Field_MCP_filtered$ensemble_mean ~ Field_MCP_filtered$field_et)
abline(fit12, col="red")

# MLP
plot(Field_MLP_filtered$field_et, Field_MLP_filtered$ensemble_mean, main="MLP - Field-measured ET vs Satellite-derived ET", xlab="Field-measured ET (mm/month)", ylab="Ensemble ET (mm/month)", col = "red", 
     xlim = c(0, max_limit),  # Setting x-axis limits from 0 to max_limit
     ylim = c(0, max_limit))  # Setting y-axis limits from 0 to max_limit)))

fit13 <- lm(Field_MLP_filtered$ensemble_mean ~ Field_MLP_filtered$field_et)
abline(fit13, col="red")

# SKU
plot(Field_SKU_filtered$field_et, Field_SKU_filtered$ensemble_mean, main="SKU - Field-measured ET vs Satellite-derived ET", xlab="Field-measured ET (mm/month)", ylab="Ensemble ET (mm/month)", col = "red", 
     xlim = c(0, max_limit),  # Setting x-axis limits from 0 to max_limit
     ylim = c(0, max_limit))  # Setting y-axis limits from 0 to max_limit)))

fit14 <- lm(Field_SKU_filtered$ensemble_mean ~ Field_SKU_filtered$field_et)
abline(fit14, col="red")

#########################

R2_BF1 <- summary(fit1)
R2_BF2 <- summary(fit2)
R2_CP3 <- summary(fit3)
R2_CP6 <- summary(fit4)
R2_CP9 <- summary(fit5)
R2_EW1 <- summary(fit6)
R2_EW2 <- summary(fit7)
R2_EZU <- summary(fit8)
R2_JHK <- summary(fit9)
R2_MB1 <- summary(fit10)
R2_MB2 <- summary(fit11)
R2_MCP <- summary(fit12)
R2_MLP <- summary(fit13)
R2_SKU <- summary(fit14)

r_squared_BF1 <- R2_BF1$r.squared
r_squared_BF2 <- R2_BF2$r.squared
r_squared_CP3 <- R2_CP3$r.squared
r_squared_CP6 <- R2_CP6$r.squared
r_squared_CP9 <- R2_CP9$r.squared
r_squared_EW1 <- R2_EW1$r.squared
r_squared_EW2 <- R2_EW2$r.squared
r_squared_EZU <- R2_EZU$r.squared
r_squared_JHK <- R2_JHK$r.squared
r_squared_MB1 <- R2_MB1$r.squared
r_squared_MB2 <- R2_MB2$r.squared
r_squared_MCP <- R2_MCP$r.squared
r_squared_MLP <- R2_MLP$r.squared
r_squared_SKU <- R2_SKU$r.squared

df_stats <- data.frame(rsr_valueBF1, rsr_valueBF2, rsr_valueCP3, rsr_valueCP6, rsr_valueCP9, rsr_valueEW1, rsr_valueEW2,
                       rsr_valueEZU, rsr_valueJHK, rsr_valueMB1, rsr_valueMB2, rsr_valueMCP, rsr_valueMLP, rsr_valueSKU,
                       rmse_valueBF1, rmse_valueBF2, rmse_valueCP3, rmse_valueCP6, rmse_valueCP9, rmse_valueEW1, rmse_valueEW2,
                       rmse_valueEZU, rmse_valueJHK, rmse_valueMB1, rmse_valueMB2, rmse_valueMCP, rmse_valueMLP, rmse_valueSKU,
                       pbiasBF1, pbiasBF2, pbiasCP3, pbiasCP6, pbiasCP9, pbiasEW1, pbiasEW2,
                       pbiasEZU, pbiasJHK, pbiasMB1, pbiasMB2, pbiasMCP, pbiasMLP, pbiasSKU,
                       biasBF1, biasBF2, biasCP3, biasCP6, biasCP9, biasEW1, biasEW2,
                       biasEZU, biasJHK, biasMB1, biasMB2, biasMCP, biasMLP, biasSKU,
                       maeBF1, maeBF2, maeCP3, maeCP6, maeCP9, maeEW1, maeEW2,
                       maeEZU, maeJHK, maeMB1, maeMB2, maeMCP, maeMLP, maeSKU,
                       nse_valueBF1, nse_valueBF2, nse_valueCP3, nse_valueCP6, nse_valueCP9, nse_valueEW1, nse_valueEW2,
                       nse_valueEZU, nse_valueJHK, nse_valueMB1, nse_valueMB2, nse_valueMCP, nse_valueMLP, nse_valueSKU,
                       kge_valueBF1, kge_valueBF2, kge_valueCP3, kge_valueCP6, kge_valueCP9, kge_valueEW1, kge_valueEW2,
                       kge_valueEZU, kge_valueJHK, kge_valueMB1, kge_valueMB2, kge_valueMCP, kge_valueMLP, kge_valueSKU,
                       p_valueBF1, p_valueBF2, p_valueCP3, p_valueCP6, p_valueCP9, p_valueEW1, p_valueEW2,
                       p_valueEZU, p_valueJHK, p_valueMB1, p_valueMB2, p_valueMCP, p_valueMLP, p_valueSKU,
                       correlation_coefficientBF1, correlation_coefficientBF2, correlation_coefficientCP3, correlation_coefficientCP6, correlation_coefficientCP9, correlation_coefficientEW1, correlation_coefficientEW2,
                       correlation_coefficientEZU, correlation_coefficientJHK, correlation_coefficientMB1, correlation_coefficientMB2, correlation_coefficientMCP, correlation_coefficientMLP, correlation_coefficientSKU,
                       r_squared_BF1, r_squared_BF2, r_squared_CP3, r_squared_CP6, r_squared_CP9, r_squared_EW1, r_squared_EW2,
                       r_squared_EZU, r_squared_JHK, r_squared_MB1, r_squared_MB2, r_squared_MCP, r_squared_MLP, r_squared_SKU
)

# Reshape the data frame from wide to long format
df_long <- df_stats %>%
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
head(df_long)

setwd("C:\\Users\\Liam\\OneDrive - Stellenbosch University\\ET_Data\\Stats")
write.csv(df_long, file = "Ensemble_Stats.csv", row.names = FALSE)

###################################################################################
######## LOG-TRANSFORMED ###########################################################
###################################################################################

#  BF1
# Log transform data
BF1[, -1] <- log10(BF1[, -1] + 1)
BF1

# Perform Spearman Correlation analysis: 
# BF1
FieldBF1_vs_Ensemble <- BF1[, c('ensemble_mean', 'field_et')]
Field_BF1_filtered <- na.omit(FieldBF1_vs_Ensemble)
Field_BF1_filtered <- data.frame(lapply(Field_BF1_filtered, as.numeric))

shapiro_test_result <- shapiro.test(Field_BF1_filtered$ensemble_mean)
print(shapiro_test_result)

spearman_corrBF1 <- cor.test(Field_BF1_filtered$field_et, Field_BF1_filtered$ensemble_mean, method = "spearman")
print(spearman_corrBF1)
correlation_coefficientBF1 <- spearman_corrBF1$estimate
p_valueBF1 <- spearman_corrBF1$p.value

kge_valueBF1 <- KGE(sim = Field_BF1_filtered$ensemble_mean, obs = Field_BF1_filtered$field_et)
rmse_valueBF1 <- rmse(actual = Field_BF1_filtered$field_et, predicted = Field_BF1_filtered$ensemble_mean)
nse_valueBF1 <- NSE(sim = Field_BF1_filtered$ensemble_mean, obs = Field_BF1_filtered$field_et)
rsr_valueBF1 <- rmse_valueBF1 / sd(Field_BF1_filtered$field_et)

biasBF1 <- mean(Field_BF1_filtered$ensemble_mean - Field_BF1_filtered$field_et)
pbiasBF1 <- 100 * sum(Field_BF1_filtered$ensemble_mean - Field_BF1_filtered$field_et) / sum(Field_BF1_filtered$field_et)
maeBF1 <- mean(abs(Field_BF1_filtered$ensemble_mean - Field_BF1_filtered$field_et))
################################################################################

#  BF2
# Log transform data
BF2[, -1] <- log10(BF2[, -1] + 1)
BF2

# Perform Spearman Correlation analysis: 
# BF2
FieldBF2_vs_Ensemble <- BF2[, c('ensemble_mean', 'field_et')]
Field_BF2_filtered <- na.omit(FieldBF2_vs_Ensemble)
Field_BF2_filtered <- data.frame(lapply(Field_BF2_filtered, as.numeric))

shapiro_test_result <- shapiro.test(Field_BF2_filtered$ensemble_mean)
print(shapiro_test_result)

spearman_corrBF2 <- cor.test(Field_BF2_filtered$field_et, Field_BF2_filtered$ensemble_mean, method = "spearman")
print(spearman_corrBF2)
correlation_coefficientBF2 <- spearman_corrBF2$estimate
p_valueBF2 <- spearman_corrBF2$p.value

kge_valueBF2 <- KGE(sim = Field_BF2_filtered$ensemble_mean, obs = Field_BF2_filtered$field_et)
rmse_valueBF2 <- rmse(actual = Field_BF2_filtered$field_et, predicted = Field_BF2_filtered$ensemble_mean)
nse_valueBF2 <- NSE(sim = Field_BF2_filtered$ensemble_mean, obs = Field_BF2_filtered$field_et)
rsr_valueBF2 <- rmse_valueBF2 / sd(Field_BF2_filtered$field_et)

biasBF2 <- mean(Field_BF2_filtered$ensemble_mean - Field_BF2_filtered$field_et)
pbiasBF2 <- 100 * sum(Field_BF2_filtered$ensemble_mean - Field_BF2_filtered$field_et) / sum(Field_BF2_filtered$field_et)
maeBF2 <- mean(abs(Field_BF2_filtered$ensemble_mean - Field_BF2_filtered$field_et))
################################################################################
#  CP3
# Log transform data
CP3[, -1] <- log10(CP3[, -1] + 1)
CP3

# Perform Spearman Correlation analysis: 
# CP3
FieldCP3_vs_Ensemble <- CP3[, c('ensemble_mean', 'field_et')]
Field_CP3_filtered <- na.omit(FieldCP3_vs_Ensemble)
Field_CP3_filtered <- data.frame(lapply(Field_CP3_filtered, as.numeric))

shapiro_test_result <- shapiro.test(Field_CP3_filtered$ensemble_mean)
print(shapiro_test_result)

spearman_corrCP3 <- cor.test(Field_CP3_filtered$field_et, Field_CP3_filtered$ensemble_mean, method = "spearman")
print(spearman_corrCP3)
correlation_coefficientCP3 <- spearman_corrCP3$estimate
p_valueCP3 <- spearman_corrCP3$p.value

kge_valueCP3 <- KGE(sim = Field_CP3_filtered$ensemble_mean, obs = Field_CP3_filtered$field_et)
rmse_valueCP3 <- rmse(actual = Field_CP3_filtered$field_et, predicted = Field_CP3_filtered$ensemble_mean)
nse_valueCP3 <- NSE(sim = Field_CP3_filtered$ensemble_mean, obs = Field_CP3_filtered$field_et)
rsr_valueCP3 <- rmse_valueCP3 / sd(Field_CP3_filtered$field_et)

biasCP3 <- mean(Field_CP3_filtered$ensemble_mean - Field_CP3_filtered$field_et)
pbiasCP3 <- 100 * sum(Field_CP3_filtered$ensemble_mean - Field_CP3_filtered$field_et) / sum(Field_CP3_filtered$field_et)
maeCP3 <- mean(abs(Field_CP3_filtered$ensemble_mean - Field_CP3_filtered$field_et))
################################################################################
#  CP6
# Log transform data
CP6[, -1] <- log10(CP6[, -1] + 1)
CP6

# Perform Spearman Correlation analysis: 
# CP6
FieldCP6_vs_Ensemble <- CP6[, c('ensemble_mean', 'field_et')]
Field_CP6_filtered <- na.omit(FieldCP6_vs_Ensemble)
Field_CP6_filtered <- data.frame(lapply(Field_CP6_filtered, as.numeric))

shapiro_test_result <- shapiro.test(Field_CP6_filtered$ensemble_mean)
print(shapiro_test_result)

spearman_corrCP6 <- cor.test(Field_CP6_filtered$field_et, Field_CP6_filtered$ensemble_mean, method = "spearman")
print(spearman_corrCP6)
correlation_coefficientCP6 <- spearman_corrCP6$estimate
p_valueCP6 <- spearman_corrCP6$p.value

kge_valueCP6 <- KGE(sim = Field_CP6_filtered$ensemble_mean, obs = Field_CP6_filtered$field_et)
rmse_valueCP6 <- rmse(actual = Field_CP6_filtered$field_et, predicted = Field_CP6_filtered$ensemble_mean)
nse_valueCP6 <- NSE(sim = Field_CP6_filtered$ensemble_mean, obs = Field_CP6_filtered$field_et)
rsr_valueCP6 <- rmse_valueCP6 / sd(Field_CP6_filtered$field_et)

biasCP6 <- mean(Field_CP6_filtered$ensemble_mean - Field_CP6_filtered$field_et)
pbiasCP6 <- 100 * sum(Field_CP6_filtered$ensemble_mean - Field_CP6_filtered$field_et) / sum(Field_CP6_filtered$field_et)
maeCP6 <- mean(abs(Field_CP6_filtered$ensemble_mean - Field_CP6_filtered$field_et))
################################################################################
#  CP9
# Log transform data
CP9[, -1] <- log10(CP9[, -1] + 1)
CP9

# Perform Spearman Correlation analysis: 
# CP9
FieldCP9_vs_Ensemble <- CP9[, c('ensemble_mean', 'field_et')]
Field_CP9_filtered <- na.omit(FieldCP9_vs_Ensemble)
Field_CP9_filtered <- data.frame(lapply(Field_CP9_filtered, as.numeric))

shapiro_test_result <- shapiro.test(Field_CP9_filtered$ensemble_mean)
print(shapiro_test_result)

spearman_corrCP9 <- cor.test(Field_CP9_filtered$field_et, Field_CP9_filtered$ensemble_mean, method = "spearman")
print(spearman_corrCP9)
correlation_coefficientCP9 <- spearman_corrCP9$estimate
p_valueCP9 <- spearman_corrCP9$p.value

kge_valueCP9 <- KGE(sim = Field_CP9_filtered$ensemble_mean, obs = Field_CP9_filtered$field_et)
rmse_valueCP9 <- rmse(actual = Field_CP9_filtered$field_et, predicted = Field_CP9_filtered$ensemble_mean)
nse_valueCP9 <- NSE(sim = Field_CP9_filtered$ensemble_mean, obs = Field_CP9_filtered$field_et)
rsr_valueCP9 <- rmse_valueCP9 / sd(Field_CP9_filtered$field_et)

biasCP9 <- mean(Field_CP9_filtered$ensemble_mean - Field_CP9_filtered$field_et)
pbiasCP9 <- 100 * sum(Field_CP9_filtered$ensemble_mean - Field_CP9_filtered$field_et) / sum(Field_CP9_filtered$field_et)
maeCP9 <- mean(abs(Field_CP9_filtered$ensemble_mean - Field_CP9_filtered$field_et))
################################################################################
#  EW1
# Log transform data
EW1[, -1] <- log10(EW1[, -1] + 1)
EW1

# Perform Spearman Correlation analysis: 
# EW1
FieldEW1_vs_Ensemble <- EW1[, c('ensemble_mean', 'field_et')]
Field_EW1_filtered <- na.omit(FieldEW1_vs_Ensemble)
Field_EW1_filtered <- data.frame(lapply(Field_EW1_filtered, as.numeric))

shapiro_test_result <- shapiro.test(Field_EW1_filtered$ensemble_mean)
print(shapiro_test_result)

spearman_corrEW1 <- cor.test(Field_EW1_filtered$field_et, Field_EW1_filtered$ensemble_mean, method = "spearman")
print(spearman_corrEW1)
correlation_coefficientEW1 <- spearman_corrEW1$estimate
p_valueEW1 <- spearman_corrEW1$p.value

kge_valueEW1 <- KGE(sim = Field_EW1_filtered$ensemble_mean, obs = Field_EW1_filtered$field_et)
rmse_valueEW1 <- rmse(actual = Field_EW1_filtered$field_et, predicted = Field_EW1_filtered$ensemble_mean)
nse_valueEW1 <- NSE(sim = Field_EW1_filtered$ensemble_mean, obs = Field_EW1_filtered$field_et)
rsr_valueEW1 <- rmse_valueEW1 / sd(Field_EW1_filtered$field_et)

biasEW1 <- mean(Field_EW1_filtered$ensemble_mean - Field_EW1_filtered$field_et)
pbiasEW1 <- 100 * sum(Field_EW1_filtered$ensemble_mean - Field_EW1_filtered$field_et) / sum(Field_EW1_filtered$field_et)
maeEW1 <- mean(abs(Field_EW1_filtered$ensemble_mean - Field_EW1_filtered$field_et))
################################################################################
#  EW2
# Log transform data
EW2[, -1] <- log10(EW2[, -1] + 1)
EW2

# Perform Spearman Correlation analysis: 
# EW2
FieldEW2_vs_Ensemble <- EW2[, c('ensemble_mean', 'field_et')]
Field_EW2_filtered <- na.omit(FieldEW2_vs_Ensemble)
Field_EW2_filtered <- data.frame(lapply(Field_EW2_filtered, as.numeric))

shapiro_test_result <- shapiro.test(Field_EW2_filtered$ensemble_mean)
print(shapiro_test_result)

spearman_corrEW2 <- cor.test(Field_EW2_filtered$field_et, Field_EW2_filtered$ensemble_mean, method = "spearman")
print(spearman_corrEW2)
correlation_coefficientEW2 <- spearman_corrEW2$estimate
p_valueEW2 <- spearman_corrEW2$p.value

kge_valueEW2 <- KGE(sim = Field_EW2_filtered$ensemble_mean, obs = Field_EW2_filtered$field_et)
rmse_valueEW2 <- rmse(actual = Field_EW2_filtered$field_et, predicted = Field_EW2_filtered$ensemble_mean)
nse_valueEW2 <- NSE(sim = Field_EW2_filtered$ensemble_mean, obs = Field_EW2_filtered$field_et)
rsr_valueEW2 <- rmse_valueEW2 / sd(Field_EW2_filtered$field_et)

biasEW2 <- mean(Field_EW2_filtered$ensemble_mean - Field_EW2_filtered$field_et)
pbiasEW2 <- 100 * sum(Field_EW2_filtered$ensemble_mean - Field_EW2_filtered$field_et) / sum(Field_EW2_filtered$field_et)
maeEW2 <- mean(abs(Field_EW2_filtered$ensemble_mean - Field_EW2_filtered$field_et))
################################################################################

#  EZU
# Log transform data
EZU[, -1] <- log10(EZU[, -1] + 1)
EZU

# Perform Spearman Correlation analysis: 
# EZU
FieldEZU_vs_Ensemble <- EZU[, c('ensemble_mean', 'field_et')]
Field_EZU_filtered <- na.omit(FieldEZU_vs_Ensemble)
Field_EZU_filtered <- data.frame(lapply(Field_EZU_filtered, as.numeric))

shapiro_test_result <- shapiro.test(Field_EZU_filtered$ensemble_mean)
print(shapiro_test_result)

spearman_corrEZU <- cor.test(Field_EZU_filtered$field_et, Field_EZU_filtered$ensemble_mean, method = "spearman")
print(spearman_corrEZU)
correlation_coefficientEZU <- spearman_corrEZU$estimate
p_valueEZU <- spearman_corrEZU$p.value

kge_valueEZU <- KGE(sim = Field_EZU_filtered$ensemble_mean, obs = Field_EZU_filtered$field_et)
rmse_valueEZU <- rmse(actual = Field_EZU_filtered$field_et, predicted = Field_EZU_filtered$ensemble_mean)
nse_valueEZU <- NSE(sim = Field_EZU_filtered$ensemble_mean, obs = Field_EZU_filtered$field_et)
rsr_valueEZU <- rmse_valueEZU / sd(Field_EZU_filtered$field_et)

biasEZU <- mean(Field_EZU_filtered$ensemble_mean - Field_EZU_filtered$field_et)
pbiasEZU <- 100 * sum(Field_EZU_filtered$ensemble_mean - Field_EZU_filtered$field_et) / sum(Field_EZU_filtered$field_et)
maeEZU <- mean(abs(Field_EZU_filtered$ensemble_mean - Field_EZU_filtered$field_et))
################################################################################

#  JHK
# Log transform data
JHK[, -1] <- log10(JHK[, -1] + 1)
JHK

# Perform Spearman Correlation analysis: 
# JHK
FieldJHK_vs_Ensemble <- JHK[, c('ensemble_mean', 'field_et')]
Field_JHK_filtered <- na.omit(FieldJHK_vs_Ensemble)
Field_JHK_filtered <- data.frame(lapply(Field_JHK_filtered, as.numeric))

shapiro_test_result <- shapiro.test(Field_JHK_filtered$ensemble_mean)
print(shapiro_test_result)

spearman_corrJHK <- cor.test(Field_JHK_filtered$field_et, Field_JHK_filtered$ensemble_mean, method = "spearman")
print(spearman_corrJHK)
correlation_coefficientJHK <- spearman_corrJHK$estimate
p_valueJHK <- spearman_corrJHK$p.value

kge_valueJHK <- KGE(sim = Field_JHK_filtered$ensemble_mean, obs = Field_JHK_filtered$field_et)
rmse_valueJHK <- rmse(actual = Field_JHK_filtered$field_et, predicted = Field_JHK_filtered$ensemble_mean)
nse_valueJHK <- NSE(sim = Field_JHK_filtered$ensemble_mean, obs = Field_JHK_filtered$field_et)
rsr_valueJHK <- rmse_valueJHK / sd(Field_JHK_filtered$field_et)

biasJHK <- mean(Field_JHK_filtered$ensemble_mean - Field_JHK_filtered$field_et)
pbiasJHK <- 100 * sum(Field_JHK_filtered$ensemble_mean - Field_JHK_filtered$field_et) / sum(Field_JHK_filtered$field_et)
maeJHK <- mean(abs(Field_JHK_filtered$ensemble_mean - Field_JHK_filtered$field_et))
################################################################################

#  MB1
# Log transform data
MB1[, -1] <- log10(MB1[, -1] + 1)
MB1

# Perform Spearman Correlation analysis: 
# MB1
FieldMB1_vs_Ensemble <- MB1[, c('ensemble_mean', 'field_et')]
Field_MB1_filtered <- na.omit(FieldMB1_vs_Ensemble)
Field_MB1_filtered <- data.frame(lapply(Field_MB1_filtered, as.numeric))

shapiro_test_result <- shapiro.test(Field_MB1_filtered$ensemble_mean)
print(shapiro_test_result)

spearman_corrMB1 <- cor.test(Field_MB1_filtered$field_et, Field_MB1_filtered$ensemble_mean, method = "spearman")
print(spearman_corrMB1)
correlation_coefficientMB1 <- spearman_corrMB1$estimate
p_valueMB1 <- spearman_corrMB1$p.value

kge_valueMB1 <- KGE(sim = Field_MB1_filtered$ensemble_mean, obs = Field_MB1_filtered$field_et)
rmse_valueMB1 <- rmse(actual = Field_MB1_filtered$field_et, predicted = Field_MB1_filtered$ensemble_mean)
nse_valueMB1 <- NSE(sim = Field_MB1_filtered$ensemble_mean, obs = Field_MB1_filtered$field_et)
rsr_valueMB1 <- rmse_valueMB1 / sd(Field_MB1_filtered$field_et)

biasMB1 <- mean(Field_MB1_filtered$ensemble_mean - Field_MB1_filtered$field_et)
pbiasMB1 <- 100 * sum(Field_MB1_filtered$ensemble_mean - Field_MB1_filtered$field_et) / sum(Field_MB1_filtered$field_et)
maeMB1 <- mean(abs(Field_MB1_filtered$ensemble_mean - Field_MB1_filtered$field_et))
################################################################################

#  MB2
# Log transform data
MB2[, -1] <- log10(MB2[, -1] + 1)
MB2

# Perform Spearman Correlation analysis: 
# MB2
FieldMB2_vs_Ensemble <- MB2[, c('ensemble_mean', 'field_et')]
Field_MB2_filtered <- na.omit(FieldMB2_vs_Ensemble)
Field_MB2_filtered <- data.frame(lapply(Field_MB2_filtered, as.numeric))

shapiro_test_result <- shapiro.test(Field_MB2_filtered$ensemble_mean)
print(shapiro_test_result)

spearman_corrMB2 <- cor.test(Field_MB2_filtered$field_et, Field_MB2_filtered$ensemble_mean, method = "spearman")
print(spearman_corrMB2)
correlation_coefficientMB2 <- spearman_corrMB2$estimate
p_valueMB2 <- spearman_corrMB2$p.value

kge_valueMB2 <- KGE(sim = Field_MB2_filtered$ensemble_mean, obs = Field_MB2_filtered$field_et)
rmse_valueMB2 <- rmse(actual = Field_MB2_filtered$field_et, predicted = Field_MB2_filtered$ensemble_mean)
nse_valueMB2 <- NSE(sim = Field_MB2_filtered$ensemble_mean, obs = Field_MB2_filtered$field_et)
rsr_valueMB2 <- rmse_valueMB2 / sd(Field_MB2_filtered$field_et)

biasMB2 <- mean(Field_MB2_filtered$ensemble_mean - Field_MB2_filtered$field_et)
pbiasMB2 <- 100 * sum(Field_MB2_filtered$ensemble_mean - Field_MB2_filtered$field_et) / sum(Field_MB2_filtered$field_et)
maeMB2 <- mean(abs(Field_MB2_filtered$ensemble_mean - Field_MB2_filtered$field_et))
################################################################################

#  MCP
# Log transform data
MCP[, -1] <- log10(MCP[, -1] + 1)
MCP

# Perform Spearman Correlation analysis: 
# MCP
FieldMCP_vs_Ensemble <- MCP[, c('ensemble_mean', 'field_et')]
Field_MCP_filtered <- na.omit(FieldMCP_vs_Ensemble)
Field_MCP_filtered <- data.frame(lapply(Field_MCP_filtered, as.numeric))

shapiro_test_result <- shapiro.test(Field_MCP_filtered$ensemble_mean)
print(shapiro_test_result)

spearman_corrMCP <- cor.test(Field_MCP_filtered$field_et, Field_MCP_filtered$ensemble_mean, method = "spearman")
print(spearman_corrMCP)
correlation_coefficientMCP <- spearman_corrMCP$estimate
p_valueMCP <- spearman_corrMCP$p.value

kge_valueMCP <- KGE(sim = Field_MCP_filtered$ensemble_mean, obs = Field_MCP_filtered$field_et)
rmse_valueMCP <- rmse(actual = Field_MCP_filtered$field_et, predicted = Field_MCP_filtered$ensemble_mean)
nse_valueMCP <- NSE(sim = Field_MCP_filtered$ensemble_mean, obs = Field_MCP_filtered$field_et)
rsr_valueMCP <- rmse_valueMCP / sd(Field_MCP_filtered$field_et)

biasMCP <- mean(Field_MCP_filtered$ensemble_mean - Field_MCP_filtered$field_et)
pbiasMCP <- 100 * sum(Field_MCP_filtered$ensemble_mean - Field_MCP_filtered$field_et) / sum(Field_MCP_filtered$field_et)
maeMCP <- mean(abs(Field_MCP_filtered$ensemble_mean - Field_MCP_filtered$field_et))
################################################################################

#  MLP
# Log transform data
MLP[, -1] <- log10(MLP[, -1] + 1)
MLP

# Perform Spearman Correlation analysis: 
# MLP
FieldMLP_vs_Ensemble <- MLP[, c('ensemble_mean', 'field_et')]
Field_MLP_filtered <- na.omit(FieldMLP_vs_Ensemble)
Field_MLP_filtered <- data.frame(lapply(Field_MLP_filtered, as.numeric))

shapiro_test_result <- shapiro.test(Field_MLP_filtered$ensemble_mean)
print(shapiro_test_result)

spearman_corrMLP <- cor.test(Field_MLP_filtered$field_et, Field_MLP_filtered$ensemble_mean, method = "spearman")
print(spearman_corrMLP)
correlation_coefficientMLP <- spearman_corrMLP$estimate
p_valueMLP <- spearman_corrMLP$p.value

kge_valueMLP <- KGE(sim = Field_MLP_filtered$ensemble_mean, obs = Field_MLP_filtered$field_et)
rmse_valueMLP <- rmse(actual = Field_MLP_filtered$field_et, predicted = Field_MLP_filtered$ensemble_mean)
nse_valueMLP <- NSE(sim = Field_MLP_filtered$ensemble_mean, obs = Field_MLP_filtered$field_et)
rsr_valueMLP <- rmse_valueMLP / sd(Field_MLP_filtered$field_et)

biasMLP <- mean(Field_MLP_filtered$ensemble_mean - Field_MLP_filtered$field_et)
pbiasMLP <- 100 * sum(Field_MLP_filtered$ensemble_mean - Field_MLP_filtered$field_et) / sum(Field_MLP_filtered$field_et)
maeMLP <- mean(abs(Field_MLP_filtered$ensemble_mean - Field_MLP_filtered$field_et))
################################################################################

#  SKU
# Log transform data
SKU[, -1] <- log10(SKU[, -1] + 1)
SKU

# Perform Spearman Correlation analysis: 
# SKU
FieldSKU_vs_Ensemble <- SKU[, c('ensemble_mean', 'field_et')]
Field_SKU_filtered <- na.omit(FieldSKU_vs_Ensemble)
Field_SKU_filtered <- data.frame(lapply(Field_SKU_filtered, as.numeric))

shapiro_test_result <- shapiro.test(Field_SKU_filtered$ensemble_mean)
print(shapiro_test_result)

spearman_corrSKU <- cor.test(Field_SKU_filtered$field_et, Field_SKU_filtered$ensemble_mean, method = "spearman")
print(spearman_corrSKU)
correlation_coefficientSKU <- spearman_corrSKU$estimate
p_valueSKU <- spearman_corrSKU$p.value

kge_valueSKU <- KGE(sim = Field_SKU_filtered$ensemble_mean, obs = Field_SKU_filtered$field_et)
rmse_valueSKU <- rmse(actual = Field_SKU_filtered$field_et, predicted = Field_SKU_filtered$ensemble_mean)
nse_valueSKU <- NSE(sim = Field_SKU_filtered$ensemble_mean, obs = Field_SKU_filtered$field_et)
rsr_valueSKU <- rmse_valueSKU / sd(Field_SKU_filtered$field_et)

biasSKU <- mean(Field_SKU_filtered$ensemble_mean - Field_SKU_filtered$field_et)
pbiasSKU <- 100 * sum(Field_SKU_filtered$ensemble_mean - Field_SKU_filtered$field_et) / sum(Field_SKU_filtered$field_et)
maeSKU <- mean(abs(Field_SKU_filtered$ensemble_mean - Field_SKU_filtered$field_et))

################################################################################

# Linear R scatterplots + R2 (coeff of determination)

# Scatter plots - Field vs satellite
max_limit <- 150

# BF1
plot(Field_BF1_filtered$field_et, Field_BF1_filtered$ensemble_mean, main="BF1 - Field-measured ET vs Satellite-derived ET", xlab="Field-measured ET (mm/month)", ylab="Ensemble ET (mm/month)", col = "red", 
     xlim = c(0, max_limit),  # Setting x-axis limits from 0 to max_limit
     ylim = c(0, max_limit))  # Setting y-axis limits from 0 to max_limit)))

fit1 <- lm(Field_BF1_filtered$ensemble_mean ~ Field_BF1_filtered$field_et)
abline(fit1, col="red")

# BF2
plot(Field_BF2_filtered$field_et, Field_BF2_filtered$ensemble_mean, main="BF2 - Field-measured ET vs Satellite-derived ET", xlab="Field-measured ET (mm/month)", ylab="Ensemble ET (mm/month)", col = "red", 
     xlim = c(0, max_limit),  # Setting x-axis limits from 0 to max_limit
     ylim = c(0, max_limit))  # Setting y-axis limits from 0 to max_limit)))

fit2 <- lm(Field_BF2_filtered$ensemble_mean ~ Field_BF2_filtered$field_et)
abline(fit2, col="red")

# CP3
plot(Field_CP3_filtered$field_et, Field_CP3_filtered$ensemble_mean, main="CP3 - Field-measured ET vs Satellite-derived ET", xlab="Field-measured ET (mm/month)", ylab="Ensemble ET (mm/month)", col = "red", 
     xlim = c(0, max_limit),  # Setting x-axis limits from 0 to max_limit
     ylim = c(0, max_limit))  # Setting y-axis limits from 0 to max_limit)))

fit3 <- lm(Field_CP3_filtered$ensemble_mean ~ Field_CP3_filtered$field_et)
abline(fit3, col="red")


# CP6
plot(Field_CP6_filtered$field_et, Field_CP6_filtered$ensemble_mean, main="CP6 - Field-measured ET vs Satellite-derived ET", xlab="Field-measured ET (mm/month)", ylab="Ensemble ET (mm/month)", col = "red", 
     xlim = c(0, max_limit),  # Setting x-axis limits from 0 to max_limit
     ylim = c(0, max_limit))  # Setting y-axis limits from 0 to max_limit)))

fit4 <- lm(Field_CP6_filtered$ensemble_mean ~ Field_CP6_filtered$field_et)
abline(fit4, col="red")


# CP9
plot(Field_CP9_filtered$field_et, Field_CP9_filtered$ensemble_mean, main="CP9 - Field-measured ET vs Satellite-derived ET", xlab="Field-measured ET (mm/month)", ylab="Ensemble ET (mm/month)", col = "red", 
     xlim = c(0, max_limit),  # Setting x-axis limits from 0 to max_limit
     ylim = c(0, max_limit))  # Setting y-axis limits from 0 to max_limit)))

fit5 <- lm(Field_CP9_filtered$ensemble_mean ~ Field_CP9_filtered$field_et)
abline(fit5, col="red")

# EW1
plot(Field_EW1_filtered$field_et, Field_EW1_filtered$ensemble_mean, main="EW1 - Field-measured ET vs Satellite-derived ET", xlab="Field-measured ET (mm/month)", ylab="Ensemble ET (mm/month)", col = "red", 
     xlim = c(0, max_limit),  # Setting x-axis limits from 0 to max_limit
     ylim = c(0, max_limit))  # Setting y-axis limits from 0 to max_limit)))

fit6 <- lm(Field_EW1_filtered$ensemble_mean ~ Field_EW1_filtered$field_et)
abline(fit6, col="red")


# EW2
plot(Field_EW2_filtered$field_et, Field_EW2_filtered$ensemble_mean, main="EW2 - Field-measured ET vs Satellite-derived ET", xlab="Field-measured ET (mm/month)", ylab="Ensemble ET (mm/month)", col = "red", 
     xlim = c(0, max_limit),  # Setting x-axis limits from 0 to max_limit
     ylim = c(0, max_limit))  # Setting y-axis limits from 0 to max_limit)))

fit7 <- lm(Field_EW2_filtered$ensemble_mean ~ Field_EW2_filtered$field_et)
abline(fit7, col="red")


# EZU
plot(Field_EZU_filtered$field_et, Field_EZU_filtered$ensemble_mean, main="EZU - Field-measured ET vs Satellite-derived ET", xlab="Field-measured ET (mm/month)", ylab="Ensemble ET (mm/month)", col = "red", 
     xlim = c(0, max_limit),  # Setting x-axis limits from 0 to max_limit
     ylim = c(0, max_limit))  # Setting y-axis limits from 0 to max_limit)))

fit8 <- lm(Field_EZU_filtered$ensemble_mean ~ Field_EZU_filtered$field_et)
abline(fit8, col="red")

# JHK
plot(Field_JHK_filtered$field_et, Field_JHK_filtered$ensemble_mean, main="JHK - Field-measured ET vs Satellite-derived ET", xlab="Field-measured ET (mm/month)", ylab="Ensemble ET (mm/month)", col = "red", 
     xlim = c(0, max_limit),  # Setting x-axis limits from 0 to max_limit
     ylim = c(0, max_limit))  # Setting y-axis limits from 0 to max_limit)))

fit9 <- lm(Field_JHK_filtered$ensemble_mean ~ Field_JHK_filtered$field_et)
abline(fit9, col="red")

# MB1
plot(Field_MB1_filtered$field_et, Field_MB1_filtered$ensemble_mean, main="MB1 - Field-measured ET vs Satellite-derived ET", xlab="Field-measured ET (mm/month)", ylab="Ensemble ET (mm/month)", col = "red", 
     xlim = c(0, max_limit),  # Setting x-axis limits from 0 to max_limit
     ylim = c(0, max_limit))  # Setting y-axis limits from 0 to max_limit)))

fit10 <- lm(Field_MB1_filtered$ensemble_mean ~ Field_MB1_filtered$field_et)
abline(fit10, col="red")


# MB2
plot(Field_MB2_filtered$field_et, Field_MB2_filtered$ensemble_mean, main="MB2 - Field-measured ET vs Satellite-derived ET", xlab="Field-measured ET (mm/month)", ylab="Ensemble ET (mm/month)", col = "red", 
     xlim = c(0, max_limit),  # Setting x-axis limits from 0 to max_limit
     ylim = c(0, max_limit))  # Setting y-axis limits from 0 to max_limit)))

fit11 <- lm(Field_MB2_filtered$ensemble_mean ~ Field_MB2_filtered$field_et)
abline(fit11, col="red")

# MCP
plot(Field_MCP_filtered$field_et, Field_MCP_filtered$ensemble_mean, main="MCP - Field-measured ET vs Satellite-derived ET", xlab="Field-measured ET (mm/month)", ylab="Ensemble ET (mm/month)", col = "red", 
     xlim = c(0, max_limit),  # Setting x-axis limits from 0 to max_limit
     ylim = c(0, max_limit))  # Setting y-axis limits from 0 to max_limit)))

fit12 <- lm(Field_MCP_filtered$ensemble_mean ~ Field_MCP_filtered$field_et)
abline(fit12, col="red")

# MLP
plot(Field_MLP_filtered$field_et, Field_MLP_filtered$ensemble_mean, main="MLP - Field-measured ET vs Satellite-derived ET", xlab="Field-measured ET (mm/month)", ylab="Ensemble ET (mm/month)", col = "red", 
     xlim = c(0, max_limit),  # Setting x-axis limits from 0 to max_limit
     ylim = c(0, max_limit))  # Setting y-axis limits from 0 to max_limit)))

fit13 <- lm(Field_MLP_filtered$ensemble_mean ~ Field_MLP_filtered$field_et)
abline(fit13, col="red")

# SKU
plot(Field_SKU_filtered$field_et, Field_SKU_filtered$ensemble_mean, main="SKU - Field-measured ET vs Satellite-derived ET", xlab="Field-measured ET (mm/month)", ylab="Ensemble ET (mm/month)", col = "red", 
     xlim = c(0, max_limit),  # Setting x-axis limits from 0 to max_limit
     ylim = c(0, max_limit))  # Setting y-axis limits from 0 to max_limit)))

fit14 <- lm(Field_SKU_filtered$ensemble_mean ~ Field_SKU_filtered$field_et)
abline(fit14, col="red")

#########################

R2_BF1 <- summary(fit1)
R2_BF2 <- summary(fit2)
R2_CP3 <- summary(fit3)
R2_CP6 <- summary(fit4)
R2_CP9 <- summary(fit5)
R2_EW1 <- summary(fit6)
R2_EW2 <- summary(fit7)
R2_EZU <- summary(fit8)
R2_JHK <- summary(fit9)
R2_MB1 <- summary(fit10)
R2_MB2 <- summary(fit11)
R2_MCP <- summary(fit12)
R2_MLP <- summary(fit13)
R2_SKU <- summary(fit14)

r_squared_BF1 <- R2_BF1$r.squared
r_squared_BF2 <- R2_BF2$r.squared
r_squared_CP3 <- R2_CP3$r.squared
r_squared_CP6 <- R2_CP6$r.squared
r_squared_CP9 <- R2_CP9$r.squared
r_squared_EW1 <- R2_EW1$r.squared
r_squared_EW2 <- R2_EW2$r.squared
r_squared_EZU <- R2_EZU$r.squared
r_squared_JHK <- R2_JHK$r.squared
r_squared_MB1 <- R2_MB1$r.squared
r_squared_MB2 <- R2_MB2$r.squared
r_squared_MCP <- R2_MCP$r.squared
r_squared_MLP <- R2_MLP$r.squared
r_squared_SKU <- R2_SKU$r.squared

df_stats <- data.frame(rsr_valueBF1, rsr_valueBF2, rsr_valueCP3, rsr_valueCP6, rsr_valueCP9, rsr_valueEW1, rsr_valueEW2,
                       rsr_valueEZU, rsr_valueJHK, rsr_valueMB1, rsr_valueMB2, rsr_valueMCP, rsr_valueMLP, rsr_valueSKU,
                       rmse_valueBF1, rmse_valueBF2, rmse_valueCP3, rmse_valueCP6, rmse_valueCP9, rmse_valueEW1, rmse_valueEW2,
                       rmse_valueEZU, rmse_valueJHK, rmse_valueMB1, rmse_valueMB2, rmse_valueMCP, rmse_valueMLP, rmse_valueSKU,
                       pbiasBF1, pbiasBF2, pbiasCP3, pbiasCP6, pbiasCP9, pbiasEW1, pbiasEW2,
                       pbiasEZU, pbiasJHK, pbiasMB1, pbiasMB2, pbiasMCP, pbiasMLP, pbiasSKU,
                       biasBF1, biasBF2, biasCP3, biasCP6, biasCP9, biasEW1, biasEW2,
                       biasEZU, biasJHK, biasMB1, biasMB2, biasMCP, biasMLP, biasSKU,
                       maeBF1, maeBF2, maeCP3, maeCP6, maeCP9, maeEW1, maeEW2,
                       maeEZU, maeJHK, maeMB1, maeMB2, maeMCP, maeMLP, maeSKU,
                       nse_valueBF1, nse_valueBF2, nse_valueCP3, nse_valueCP6, nse_valueCP9, nse_valueEW1, nse_valueEW2,
                       nse_valueEZU, nse_valueJHK, nse_valueMB1, nse_valueMB2, nse_valueMCP, nse_valueMLP, nse_valueSKU,
                       kge_valueBF1, kge_valueBF2, kge_valueCP3, kge_valueCP6, kge_valueCP9, kge_valueEW1, kge_valueEW2,
                       kge_valueEZU, kge_valueJHK, kge_valueMB1, kge_valueMB2, kge_valueMCP, kge_valueMLP, kge_valueSKU,
                       p_valueBF1, p_valueBF2, p_valueCP3, p_valueCP6, p_valueCP9, p_valueEW1, p_valueEW2,
                       p_valueEZU, p_valueJHK, p_valueMB1, p_valueMB2, p_valueMCP, p_valueMLP, p_valueSKU,
                       correlation_coefficientBF1, correlation_coefficientBF2, correlation_coefficientCP3, correlation_coefficientCP6, correlation_coefficientCP9, correlation_coefficientEW1, correlation_coefficientEW2,
                       correlation_coefficientEZU, correlation_coefficientJHK, correlation_coefficientMB1, correlation_coefficientMB2, correlation_coefficientMCP, correlation_coefficientMLP, correlation_coefficientSKU,
                       r_squared_BF1, r_squared_BF2, r_squared_CP3, r_squared_CP6, r_squared_CP9, r_squared_EW1, r_squared_EW2,
                       r_squared_EZU, r_squared_JHK, r_squared_MB1, r_squared_MB2, r_squared_MCP, r_squared_MLP, r_squared_SKU
)

# Reshape the data frame from wide to long format
df_long <- df_stats %>%
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
head(df_long)

setwd("C:\\Users\\Liam\\OneDrive - Stellenbosch University\\ET_Data\\Stats")
write.csv(df_long, file = "Ensemble_Logged_Stats.csv", row.names = FALSE)

