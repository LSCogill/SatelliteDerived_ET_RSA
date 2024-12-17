# Interannual: Example for Befontein - Site 1 (BF1)

library(lubridate)
library(ggplot2)
library(hydroGOF) 
library(dplyr)
library(xts)
library(zoo)

# Inter annual variability

# Load dataset
setwd("C:\\Users\\Liam\\OneDrive - Stellenbosch University\\ET_Data\\Month_vs_Month_ET_Data")
BF1<-read.csv("ET_Monthly_BF1.csv",skip=0, sep = ",")
BF1$Month_Year <- as.Date(BF1$Month_Year, format = "%d/%m/%Y")

# Split into hydrological years (if the timeseries spans over the course of two or more years)
year1 <- BF1[6:17, ]
year2 <- BF1[18:29, ]
year3 <- BF1[30:41, ]

# Mean ET for each of the yrs
mean_yr1 <- apply(year1[, 2:10], 2, mean, na.rm = TRUE)
mean_yr2 <- apply(year2[, 2:10], 2, mean, na.rm = TRUE)
mean_yr3 <- apply(year3[, 2:10], 2, mean, na.rm = TRUE)

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

write.csv(yr_df_BF1, file = "Inter_BF1.csv", row.names = FALSE)

# TC 
kge_valueTC <- KGE(sim = yr_df_BF1$TC_monthly, obs = yr_df_BF1$Field_ET_monthly)
rmse_valueTC <- rmse(actual = yr_df_BF1$Field_ET_monthly, predicted = yr_df_BF1$TC_monthly)
nse_valueTC <- NSE(sim = yr_df_BF1$TC_monthly, obs = yr_df_BF1$Field_ET_monthly)
rsr_valueTC <- rmse_valueTC / sd(yr_df_BF1$Field_ET_monthly)

biasTC <- mean(yr_df_BF1$TC_monthly - yr_df_BF1$Field_ET_monthly)
pbiasTC <- 100 * sum(yr_df_BF1$TC_monthly - yr_df_BF1$Field_ET_monthly) / sum(yr_df_BF1$Field_ET_monthly)
maeTC <- mean(abs(yr_df_BF1$TC_monthly - yr_df_BF1$Field_ET_monthly))

spearman_corrTC <- cor.test(yr_df_BF1$Field_ET_monthly, yr_df_BF1$TC_monthly, method = "spearman")
print(spearman_corrTC)
correlation_coefficientTC <- spearman_corrTC$estimate

# SMAP
kge_valueSMAP <- KGE(sim = yr_df_BF1$SMAP_monthly, obs = yr_df_BF1$Field_ET_monthly)
rmse_valueSMAP <- rmse(actual = yr_df_BF1$Field_ET_monthly, predicted = yr_df_BF1$SMAP_monthly)
nse_valueSMAP <- NSE(sim = yr_df_BF1$SMAP_monthly, obs = yr_df_BF1$Field_ET_monthly)
rsr_valueSMAP <- rmse_valueSMAP / sd(yr_df_BF1$Field_ET_monthly)

biasSMAP <- mean(yr_df_BF1$SMAP_monthly - yr_df_BF1$Field_ET_monthly)
pbiasSMAP <- 100 * sum(yr_df_BF1$SMAP_monthly - yr_df_BF1$Field_ET_monthly) / sum(yr_df_BF1$Field_ET_monthly)
maeSMAP <- mean(abs(yr_df_BF1$SMAP_monthly - yr_df_BF1$Field_ET_monthly))

spearman_corrSMAP <- cor.test(yr_df_BF1$Field_ET_monthly, yr_df_BF1$SMAP_monthly, method = "spearman")
print(spearman_corrSMAP)
correlation_coefficientSMAP <- spearman_corrSMAP$estimate

# SSEBop
kge_valueSSEBop <- KGE(sim = yr_df_BF1$SSEBop_monthly, obs = yr_df_BF1$Field_ET_monthly)
rmse_valueSSEBop <- rmse(actual = yr_df_BF1$Field_ET_monthly [!is.na(yr_df_BF1$Field_ET_monthly) & !is.na(yr_df_BF1$SSEBop_monthly)], 
                         predicted = yr_df_BF1$SSEBop_monthly [!is.na(yr_df_BF1$Field_ET_monthly) & !is.na(yr_df_BF1$SSEBop_monthly)])
nse_valueSSEBop <- NSE(sim = yr_df_BF1$SSEBop_monthly, obs = yr_df_BF1$Field_ET_monthly)
rsr_valueSSEBop <- rmse_valueSSEBop / sd(yr_df_BF1$Field_ET_monthly, na.rm = TRUE)

biasSSEBop <- mean(yr_df_BF1$SSEBop_monthly - yr_df_BF1$Field_ET_monthly, na.rm = TRUE)
pbiasSSEBop <- 100 * sum(yr_df_BF1$SSEBop_monthly - yr_df_BF1$Field_ET_monthly, na.rm = TRUE) / sum(yr_df_BF1$Field_ET_monthly, na.rm = TRUE)
maeSSEBop <- mean(abs(yr_df_BF1$SSEBop_monthly - yr_df_BF1$Field_ET_monthly), na.rm = TRUE)

spearman_corrSSEBop <- cor.test(yr_df_BF1$Field_ET_monthly, yr_df_BF1$SSEBop_monthly, method = "spearman")
print(spearman_corrSSEBop)
correlation_coefficientSSEBop <- spearman_corrSSEBop$estimate

# WAPOR
kge_valueWaPOR <- KGE(sim = yr_df_BF1$WaPOR_monthly, obs = yr_df_BF1$Field_ET_monthly)
rmse_valueWaPOR <- rmse(actual = yr_df_BF1$Field_ET_monthly, predicted = yr_df_BF1$WaPOR_monthly)
nse_valueWaPOR <- NSE(sim = yr_df_BF1$WaPOR_monthly, obs = yr_df_BF1$Field_ET_monthly)
rsr_valueWaPOR <- rmse_valueWaPOR / sd(yr_df_BF1$Field_ET_monthly)

biasWaPOR <- mean(yr_df_BF1$WaPOR_monthly - yr_df_BF1$Field_ET_monthly)
pbiasWaPOR <- 100 * sum(yr_df_BF1$WaPOR_monthly - yr_df_BF1$Field_ET_monthly) / sum(yr_df_BF1$Field_ET_monthly)
maeWaPOR <- mean(abs(yr_df_BF1$WaPOR_monthly - yr_df_BF1$Field_ET_monthly))

spearman_corrWaPOR <- cor.test(yr_df_BF1$Field_ET_monthly, yr_df_BF1$WaPOR_monthly, method = "spearman")
print(spearman_corrWaPOR)
correlation_coefficientWaPOR <- spearman_corrWaPOR$estimate

# MOD16
kge_valueMOD16 <- KGE(sim = yr_df_BF1$MOD16_monthly, obs = yr_df_BF1$Field_ET_monthly)
rmse_valueMOD16 <- rmse(actual = yr_df_BF1$Field_ET_monthly, predicted = yr_df_BF1$MOD16_monthly)
nse_valueMOD16 <- NSE(sim = yr_df_BF1$MOD16_monthly, obs = yr_df_BF1$Field_ET_monthly)
rsr_valueMOD16 <- rmse_valueMOD16 / sd(yr_df_BF1$Field_ET_monthly)

biasMOD16 <- mean(yr_df_BF1$MOD16_monthly - yr_df_BF1$Field_ET_monthly)
pbiasMOD16 <- 100 * sum(yr_df_BF1$MOD16_monthly - yr_df_BF1$Field_ET_monthly) / sum(yr_df_BF1$Field_ET_monthly)
maeMOD16 <- mean(abs(yr_df_BF1$MOD16_monthly - yr_df_BF1$Field_ET_monthly))

spearman_corrMOD16 <- cor.test(yr_df_BF1$Field_ET_monthly, yr_df_BF1$MOD16_monthly, method = "spearman")
print(spearman_corrMOD16)
correlation_coefficientMOD16 <- spearman_corrMOD16$estimate

# GLDAS
kge_valueGLDAS <- KGE(sim = yr_df_BF1$GLDAS_monthly, obs = yr_df_BF1$Field_ET_monthly)
rmse_valueGLDAS <- rmse(actual = yr_df_BF1$Field_ET_monthly, predicted = yr_df_BF1$GLDAS_monthly)
nse_valueGLDAS <- NSE(sim = yr_df_BF1$GLDAS_monthly, obs = yr_df_BF1$Field_ET_monthly)
rsr_valueGLDAS <- rmse_valueGLDAS / sd(yr_df_BF1$Field_ET_monthly)

biasGLDAS <- mean(yr_df_BF1$GLDAS_monthly - yr_df_BF1$Field_ET_monthly)
pbiasGLDAS <- 100 * sum(yr_df_BF1$GLDAS_monthly - yr_df_BF1$Field_ET_monthly) / sum(yr_df_BF1$Field_ET_monthly)
maeGLDAS <- mean(abs(yr_df_BF1$GLDAS_monthly - yr_df_BF1$Field_ET_monthly))

spearman_corrGLDAS <- cor.test(yr_df_BF1$Field_ET_monthly, yr_df_BF1$GLDAS_monthly, method = "spearman")
print(spearman_corrGLDAS)
correlation_coefficientGLDAS <- spearman_corrGLDAS$estimate

# FLDAS
kge_valueFLDAS <- KGE(sim = yr_df_BF1$FLDAS_monthly, obs = yr_df_BF1$Field_ET_monthly)
rmse_valueFLDAS <- rmse(actual = yr_df_BF1$Field_ET_monthly, predicted = yr_df_BF1$FLDAS_monthly)
nse_valueFLDAS <- NSE(sim = yr_df_BF1$FLDAS_monthly, obs = yr_df_BF1$Field_ET_monthly)
rsr_valueFLDAS <- rmse_valueFLDAS / sd(yr_df_BF1$Field_ET_monthly)

biasFLDAS <- mean(yr_df_BF1$FLDAS_monthly - yr_df_BF1$Field_ET_monthly)
pbiasFLDAS <- 100 * sum(yr_df_BF1$FLDAS_monthly - yr_df_BF1$Field_ET_monthly) / sum(yr_df_BF1$Field_ET_monthly)
maeFLDAS <- mean(abs(yr_df_BF1$FLDAS_monthly - yr_df_BF1$Field_ET_monthly))

spearman_corrFLDAS <- cor.test(yr_df_BF1$Field_ET_monthly, yr_df_BF1$FLDAS_monthly, method = "spearman")
print(spearman_corrFLDAS)
correlation_coefficientFLDAS <- spearman_corrFLDAS$estimate

# PTJPL
kge_valuePTjpl <- KGE(sim = yr_df_BF1$PTjpl_monthly, obs = yr_df_BF1$Field_ET_monthly)
rmse_valuePTjpl <- rmse(actual = yr_df_BF1$Field_ET_monthly, predicted = yr_df_BF1$PTjpl_monthly)
nse_valuePTjpl <- NSE(sim = yr_df_BF1$PTjpl_monthly, obs = yr_df_BF1$Field_ET_monthly)
rsr_valuePTjpl <- rmse_valuePTjpl / sd(yr_df_BF1$Field_ET_monthly)

biasPTjpl <- mean(yr_df_BF1$PTjpl_monthly - yr_df_BF1$Field_ET_monthly)
pbiasPTjpl <- 100 * sum(yr_df_BF1$PTjpl_monthly - yr_df_BF1$Field_ET_monthly) / sum(yr_df_BF1$Field_ET_monthly)
maePTjpl <- mean(abs(yr_df_BF1$PTjpl_monthly - yr_df_BF1$Field_ET_monthly))

spearman_corrPTjpl <- cor.test(yr_df_BF1$Field_ET_monthly, yr_df_BF1$PTjpl_monthly, method = "spearman")
print(spearman_corrPTjpl)
correlation_coefficientPTjpl <- spearman_corrPTjpl$estimate

###########
# R-squared
fit1 <- lm(yr_df_BF1$TC_monthly ~ yr_df_BF1$Field_ET_monthly)
fit2 <- lm(yr_df_BF1$SMAP_monthly ~ yr_df_BF1$Field_ET_monthly)
fit3 <- lm(yr_df_BF1$WaPOR_monthly ~ yr_df_BF1$Field_ET_monthly)
fit4 <- lm(yr_df_BF1$GLDAS_monthly ~ yr_df_BF1$Field_ET_monthly)
fit5 <- lm(yr_df_BF1$MOD16_monthly ~ yr_df_BF1$Field_ET_monthly)
fit6 <- lm(yr_df_BF1$PTjpl_monthly ~ yr_df_BF1$Field_ET_monthly)
fit7 <- lm(yr_df_BF1$FLDAS_monthly ~ yr_df_BF1$Field_ET_monthly)
fit8 <- lm(yr_df_BF1$SSEBop_monthly ~ yr_df_BF1$Field_ET_monthly)

R2_TC <- summary(fit1)
R2_SMAP <- summary(fit2)
R2_WaPOR <- summary(fit3)
R2_GLDAS <- summary(fit4)
R2_MOD16 <- summary(fit5)
R2_PTjpl <- summary(fit6)
R2_FLDAS <- summary(fit7)
R2_SSEBop <- summary(fit8)

r_squared_TC <- R2_TC$r.squared
r_squared_SMAP <- R2_SMAP$r.squared
r_squared_WaPOR <- R2_WaPOR$r.squared
r_squared_GLDAS <- R2_GLDAS$r.squared
r_squared_MOD16 <- R2_MOD16$r.squared
r_squared_PTjpl <- R2_PTjpl$r.squared
r_squared_FLDAS <- R2_FLDAS$r.squared
r_squared_SSEBop <- R2_SSEBop$r.squared

# Plots
# Plot each years data together on a single plot
# Show the error or bias of each product from the field ET (on a straight line 0) across year 1-3

# Mean ET yr1 - yr3
Original_TSplot <- plot_ly(yr_df_BF1, type = 'scatter', mode = 'lines+markers',
                           line = list(dash = 'dash'),
                           marker = list(symbol = 'circle', size = 15)) %>%
  add_trace(x = ~year, y = ~Field_ET_monthly, name = 'Field_ET', 
            line = list(color = 'black', width = 4),
            marker = list(color = 'black', symbol = 'circle', size = 15)) %>%
  add_trace(x = ~year, y = ~TC_monthly, name = 'TerraClimate', 
            line = list(color = 'blue'), marker = list(color = 'blue')) %>%
  add_trace(x = ~year, y = ~SMAP_monthly, name = 'SMAP', 
            line = list(color = 'orange'), marker = list(color = 'orange')) %>%
  add_trace(x = ~year, y = ~SSEBop_monthly, name = 'SSEBop', 
            line = list(color = 'magenta'), marker = list(color = 'magenta')) %>%
  add_trace(x = ~year, y = ~GLDAS_monthly, name = 'GLDAS', 
            line = list(color = 'red'), marker = list(color = 'red')) %>%
  add_trace(x = ~year, y = ~FLDAS_monthly, name = 'FLDAS', 
            line = list(color = 'purple'), marker = list(color = 'purple')) %>%
  add_trace(x = ~year, y = ~WaPOR_monthly, name = 'WaPOR', 
            line = list(color = 'limegreen'), marker = list(color = 'limegreen')) %>%
  add_trace(x = ~year, y = ~MOD16_monthly, name = 'MOD16', 
            line = list(color = 'saddlebrown'), marker = list(color = 'saddlebrown')) %>%
  add_trace(x = ~year, y = ~PTjpl_monthly, name = 'PT-JPL', 
            line = list(color = 'yellow'), marker = list(color = 'yellow')) %>%
  layout(title = 'Mean ET (mm/month): BF1',legend=list(title=list(text='ET_Product')),
         xaxis = list(title = 'Year', tickvals = c(0,1,2), ticktext = c('2020', '2021', '2022')), yaxis = list(title = 'Evapotranspiration (mm/month)'), width = 1000)

Original_TSplot

# Difference from field ET yr1 - yr3
yr_df_BF1$DifferenceField <- yr_df_BF1$Field_ET_monthly - yr_df_BF1$Field_ET_monthly
yr_df_BF1$DifferenceTC <- yr_df_BF1$TC_monthly - yr_df_BF1$Field_ET_monthly
yr_df_BF1$DifferenceSMAP <- yr_df_BF1$SMAP_monthly - yr_df_BF1$Field_ET_monthly
yr_df_BF1$DifferenceSSEBop <- yr_df_BF1$SSEBop_monthly - yr_df_BF1$Field_ET_monthly
yr_df_BF1$DifferenceGLDAS <- yr_df_BF1$GLDAS_monthly - yr_df_BF1$Field_ET_monthly
yr_df_BF1$DifferenceFLDAS <- yr_df_BF1$FLDAS_monthly - yr_df_BF1$Field_ET_monthly
yr_df_BF1$DifferenceWaPOR <- yr_df_BF1$WaPOR_monthly - yr_df_BF1$Field_ET_monthly
yr_df_BF1$DifferenceMOD16 <- yr_df_BF1$MOD16_monthly - yr_df_BF1$Field_ET_monthly
yr_df_BF1$DifferencePTjpl <- yr_df_BF1$PTjpl_monthly - yr_df_BF1$Field_ET_monthly

Diff_TSplot <- plot_ly(yr_df_BF1, type = 'scatter', mode = 'lines+markers',
                           line = list(dash = 'dash'),
                           marker = list(symbol = 'circle', size = 15))%>%
  add_trace(y = ~DifferenceField, name = 'Field_ET', line = list(color = 'black', width = 4),
            marker = list(color = 'black', symbol = 'circle', size = 15))%>%
  add_trace(y = ~DifferenceTC, name = 'TerraClimate_ET')%>%
  add_trace(y = ~DifferenceSMAP, name = 'SMAP_ET')%>%
  add_trace(y = ~DifferenceSSEBop, name = 'SSEBop_ET')%>%
  add_trace(y = ~DifferenceGLDAS, name = 'GLDAS_ET')%>%
  add_trace(y = ~DifferenceFLDAS, name = 'FLDAS_ET')%>%
  add_trace(y = ~DifferenceWaPOR, name = 'WaPOR_ET')%>%
  add_trace(y = ~DifferenceMOD16, name = 'MOD16_ET')%>%
  add_trace(y = ~DifferencePTjpl, name = 'PTjpl_ET')%>%
  layout(title = 'Difference from field measured ET (mm/month): BF1',legend=list(title=list(text='ET_Product')),
         xaxis = list(title = 'Year', tickvals = c(0,1,2), ticktext = c('2020', '2021', '2021')), yaxis = list(title = 'Difference in Mean ET (mm/month)'), width = 1000)

Diff_TSplot

# Export all stats to a csv
df_stats <- data.frame(rsr_valueTC, rsr_valueWaPOR, rsr_valueSMAP, rsr_valueFLDAS, rsr_valueGLDAS, rsr_valueMOD16, rsr_valueSSEBop, rsr_valuePTjpl,
                       rmse_valueTC, rmse_valueWaPOR, rmse_valueSMAP, rmse_valueFLDAS, rmse_valueGLDAS, rmse_valueMOD16, rmse_valueSSEBop, rmse_valuePTjpl,
                       pbiasTC, pbiasWaPOR, pbiasSMAP, pbiasFLDAS, pbiasGLDAS, pbiasMOD16, pbiasSSEBop, pbiasPTjpl,
                       biasTC, biasWaPOR, biasSMAP, biasFLDAS, biasGLDAS, biasMOD16, biasSSEBop, biasPTjpl,
                       maeTC, maeWaPOR, maeSMAP, maeFLDAS, maeGLDAS, maeMOD16, maeSSEBop, maePTjpl,
                       nse_valueTC, nse_valueWaPOR, nse_valueSMAP, nse_valueFLDAS, nse_valueGLDAS, nse_valueMOD16, nse_valueSSEBop, nse_valuePTjpl,
                       kge_valueTC, kge_valueWaPOR, kge_valueSMAP, kge_valueFLDAS, kge_valueGLDAS, kge_valueMOD16, kge_valueSSEBop, kge_valuePTjpl,
                       correlation_coefficientTC, correlation_coefficientWaPOR, correlation_coefficientSMAP, correlation_coefficientFLDAS, correlation_coefficientGLDAS, correlation_coefficientMOD16, correlation_coefficientSSEBop, correlation_coefficientPTjpl, 
                       r_squared_TC, r_squared_WaPOR, r_squared_SMAP, r_squared_FLDAS, r_squared_GLDAS, r_squared_MOD16, r_squared_SSEBop, r_squared_PTjpl)

setwd("C:\\Users\\Liam\\OneDrive - Stellenbosch University\\ET_Data\\Stats")
write.csv(df_stats, file = "InterStats_raw_BF1.csv", row.names = FALSE)

