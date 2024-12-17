# Seasonal for example of Benfontein - Site 1 (BF1)
library(lubridate)
library(ggplot2)
library(hydroGOF) 
library(dplyr)
library(xts)
library(zoo)

# Load the dataset
setwd("C:\\Users\\Liam\\OneDrive - Stellenbosch University\\ET_Data\\Month_vs_Month_ET_Data")
BF1 <- read.csv("ET_Seasonal_BF1.csv", skip=0, sep = ",")
BF1$Month_Year <- as.Date(BF1$Month_Year, format = "%d/%m/%Y")

# Plot original data for TS plot = (i) Slider version + (ii) Neat version for PNG export 

# (i) TS with slider and legend
ts_plot(BF1,
        title = "Evapotranspiration: BF1",
        Xtitle = "Date",
        Ytitle = "ET (mm/month)",
        slider = TRUE)

# (ii) PNG Export with monthly intervals (x-axis)
Seasonal_TSplot <- plot_ly(BF1, type = 'scatter', mode = 'lines')%>%
  add_trace(x = ~Month_Year, y = ~Field_ET_monthly, name = 'Field_ET', line = list(color = 'black', width = 4))%>%
  add_trace(x = ~Month_Year, y = ~TC_monthly, name = 'TerraClimate_ET')%>%
  add_trace(x = ~Month_Year, y = ~SMAP_monthly, name = 'SMAP_ET')%>%
  add_trace(x = ~Month_Year, y = ~SSEBop_monthly, name = 'SSEBop_ET')%>%
  add_trace(x = ~Month_Year, y = ~GLDAS_monthly, name = 'GLDAS_ET')%>%
  add_trace(x = ~Month_Year, y = ~FLDAS_monthly, name = 'FLDAS_ET')%>%
  add_trace(x = ~Month_Year, y = ~WaPOR_monthly, name = 'WaPOR_ET')%>%
  add_trace(x = ~Month_Year, y = ~MOD16_monthly, name = 'MOD16_ET')%>%
  add_trace(x = ~Month_Year, y = ~PTjpl_monthly, name = 'PTjpl_ET')%>%
  layout(title = 'Evapotranspiration (mm/month): BF1',legend=list(title=list(text='ET_Product')),
         xaxis = list(title = 'Date', dtick = "M1", tickformat="%b %Y", tickangle = -45), yaxis = list(title = 'Evapotranspiration (mm/month)'), width = 1500)

Seasonal_TSplot

# MS Plots
Seasonal_TSplot <- plot_ly(BF1, type = 'scatter', mode = 'lines') %>%
  add_trace(x = ~Month_Year, y = ~Field_ET_monthly, name = 'Field_ET', line = list(color = 'black', width = 4)) %>%
  add_trace(x = ~Month_Year, y = ~TC_monthly, name = 'TerraClimate_ET', line = list(color = 'blue')) %>%
  add_trace(x = ~Month_Year, y = ~SMAP_monthly, name = 'SMAP_ET', line = list(color = 'orange')) %>%
  add_trace(x = ~Month_Year, y = ~SSEBop_monthly, name = 'SSEBop_ET', line = list(color = 'magenta')) %>%
  add_trace(x = ~Month_Year, y = ~GLDAS_monthly, name = 'GLDAS_ET', line = list(color = 'red')) %>%
  add_trace(x = ~Month_Year, y = ~FLDAS_monthly, name = 'FLDAS_ET', line = list(color = 'purple')) %>%
  add_trace(x = ~Month_Year, y = ~WaPOR_monthly, name = 'WaPOR_ET', line = list(color = 'limegreen')) %>%
  add_trace(x = ~Month_Year, y = ~MOD16_monthly, name = 'MOD16_ET', line = list(color = 'saddlebrown')) %>%
  add_trace(x = ~Month_Year, y = ~PTjpl_monthly, name = 'PTjpl_ET', line = list(color = 'yellow')) %>%
  layout(
    title = '',
    xaxis = list(title = '', dtick = "M1", tickformat = "%b %Y", tickangle = -45),
    yaxis = list(title = 'Evapotranspiration (mm/month)', showticklabels = TRUE),
    showlegend = TRUE,
    width = 1500
  )

Seasonal_TSplot


####################################################
# Perform Spearman Correlation analysis: Seasonal Data
# TC
Field_vs_TC <- BF1[, c('TC_monthly', 'Field_ET_monthly')]
Field_TC_filtered <- na.omit(Field_vs_TC)
Field_TC_filtered <- data.frame(lapply(Field_TC_filtered, as.numeric))

shapiro_test_result <- shapiro.test(Field_TC_filtered$TC_monthly)
print(shapiro_test_result)

spearman_corrTC <- cor.test(Field_TC_filtered$Field_ET_monthly, Field_TC_filtered$TC_monthly, method = "spearman")
print(spearman_corrTC)
correlation_coefficientTC <- spearman_corrTC$estimate
p_valueTC <- spearman_corrTC$p.value

# SMAP
Field_vs_SMAP <- BF1[, c('SMAP_monthly', 'Field_ET_monthly')]
Field_SMAP_filtered <- na.omit(Field_vs_SMAP)
Field_SMAP_filtered <- data.frame(lapply(Field_SMAP_filtered, as.numeric))

shapiro_test_result <- shapiro.test(Field_SMAP_filtered$SMAP_monthly)
print(shapiro_test_result)

spearman_corrSMAP <- cor.test(Field_SMAP_filtered$Field_ET_monthly, Field_SMAP_filtered$SMAP_monthly, method = "spearman")
print(spearman_corrSMAP)
correlation_coefficientSMAP <- spearman_corrSMAP$estimate
p_valueSMAP <- spearman_corrSMAP$p.value

# FLDAS
Field_vs_FLDAS <- BF1[, c('FLDAS_monthly', 'Field_ET_monthly')]
Field_FLDAS_filtered <- na.omit(Field_vs_FLDAS)
Field_FLDAS_filtered <- data.frame(lapply(Field_FLDAS_filtered, as.numeric))

shapiro_test_result <- shapiro.test(Field_FLDAS_filtered$FLDAS_monthly)
print(shapiro_test_result)

spearman_corrFLDAS <- cor.test(Field_FLDAS_filtered$Field_ET_monthly, Field_FLDAS_filtered$FLDAS_monthly, method = "spearman")
print(spearman_corrFLDAS)
correlation_coefficientFLDAS <- spearman_corrFLDAS$estimate
p_valueFLDAS <- spearman_corrFLDAS$p.value

#GLDAS
Field_vs_GLDAS <- BF1[, c('GLDAS_monthly', 'Field_ET_monthly')]
Field_GLDAS_filtered <- na.omit(Field_vs_GLDAS)
Field_GLDAS_filtered <- data.frame(lapply(Field_GLDAS_filtered, as.numeric))

shapiro_test_result <- shapiro.test(Field_GLDAS_filtered$GLDAS_monthly)
print(shapiro_test_result)

spearman_corrGLDAS <- cor.test(Field_GLDAS_filtered$Field_ET_monthly, Field_GLDAS_filtered$GLDAS_monthly, method = "spearman")
print(spearman_corrGLDAS)
correlation_coefficientGLDAS <- spearman_corrGLDAS$estimate
p_valueGLDAS <- spearman_corrGLDAS$p.value

#MOD16
Field_vs_MOD16 <- BF1[, c('MOD16_monthly', 'Field_ET_monthly')]
Field_MOD16_filtered <- na.omit(Field_vs_MOD16)
Field_MOD16_filtered <- data.frame(lapply(Field_MOD16_filtered, as.numeric))

shapiro_test_result <- shapiro.test(Field_MOD16_filtered$MOD16_monthly)
print(shapiro_test_result)

spearman_corrMOD16 <- cor.test(Field_MOD16_filtered$Field_ET_monthly, Field_MOD16_filtered$MOD16_monthly, method = "spearman")
print(spearman_corrMOD16)
correlation_coefficientMOD16 <- spearman_corrMOD16$estimate
p_valueMOD16 <- spearman_corrMOD16$p.value

#SSEBOP
Field_vs_SSEBop <- BF1[, c('SSEBop_monthly', 'Field_ET_monthly')]
Field_SSEBop_filtered <- na.omit(Field_vs_SSEBop)
Field_SSEBop_filtered <- data.frame(lapply(Field_SSEBop_filtered, as.numeric))

shapiro_test_result <- shapiro.test(Field_SSEBop_filtered$SSEBop_monthly)
print(shapiro_test_result)

spearman_corrSSEBop <- cor.test(Field_SSEBop_filtered$Field_ET_monthly, Field_SSEBop_filtered$SSEBop_monthly, method = "spearman")
print(spearman_corrSSEBop)
correlation_coefficientSSEBop <- spearman_corrSSEBop$estimate
p_valueSSEBop <- spearman_corrSSEBop$p.value

#WAPOR
Field_vs_WaPOR <- BF1[, c('WaPOR_monthly', 'Field_ET_monthly')]
Field_WaPOR_filtered <- na.omit(Field_vs_WaPOR)
Field_WaPOR_filtered <- data.frame(lapply(Field_WaPOR_filtered, as.numeric))

shapiro_test_result <- shapiro.test(Field_WaPOR_filtered$WaPOR_monthly)
print(shapiro_test_result)

spearman_corrWaPOR <- cor.test(Field_WaPOR_filtered$Field_ET_monthly, Field_WaPOR_filtered$WaPOR_monthly, method = "spearman")
print(spearman_corrWaPOR)
correlation_coefficientWaPOR <- spearman_corrWaPOR$estimate
p_valueWaPOR <- spearman_corrWaPOR$p.value

#PTJPL
Field_vs_PTjpl <- BF1[, c('PTjpl_monthly', 'Field_ET_monthly')]
Field_PTjpl_filtered <- na.omit(Field_vs_PTjpl)
Field_PTjpl_filtered <- data.frame(lapply(Field_PTjpl_filtered, as.numeric))

shapiro_test_result <- shapiro.test(Field_PTjpl_filtered$PTjpl_monthly)
print(shapiro_test_result)

spearman_corrPTjpl <- cor.test(Field_PTjpl_filtered$Field_ET_monthly, Field_PTjpl_filtered$PTjpl_monthly, method = "spearman")
print(spearman_corrPTjpl)
correlation_coefficientPTjpl <- spearman_corrPTjpl$estimate
p_valuePTjpl <- spearman_corrPTjpl$p.value


#####################################################################
# Conduct other statistical analyses for various performance metrics:
# KGE, RMSE, NSE, RSR, Bias, PBias, MAE
#TC
kge_valueTC <- KGE(sim = Field_TC_filtered$TC_monthly, obs = Field_TC_filtered$Field_ET_monthly)
rmse_valueTC <- rmse(actual = Field_TC_filtered$Field_ET_monthly, predicted = Field_TC_filtered$TC_monthly)
nse_valueTC <- NSE(sim = Field_TC_filtered$TC_monthly, obs = Field_TC_filtered$Field_ET_monthly)
rsr_valueTC <- rmse_valueTC / sd(Field_TC_filtered$Field_ET_monthly)

biasTC <- mean(Field_TC_filtered$TC_monthly - Field_TC_filtered$Field_ET_monthly)
pbiasTC <- 100 * sum(Field_TC_filtered$TC_monthly - Field_TC_filtered$Field_ET_monthly) / sum(Field_TC_filtered$Field_ET_monthly)
maeTC <- mean(abs(Field_TC_filtered$TC_monthly - Field_TC_filtered$Field_ET_monthly))

#GLDAS
kge_valueGLDAS <- KGE(sim = Field_GLDAS_filtered$GLDAS_monthly, obs = Field_GLDAS_filtered$Field_ET_monthly)
rmse_valueGLDAS <- rmse(actual = Field_GLDAS_filtered$Field_ET_monthly, predicted = Field_GLDAS_filtered$GLDAS_monthly)
nse_valueGLDAS <- NSE(sim = Field_GLDAS_filtered$GLDAS_monthly, obs = Field_GLDAS_filtered$Field_ET_monthly)
rsr_valueGLDAS <- rmse_valueGLDAS / sd(Field_GLDAS_filtered$Field_ET_monthly)

biasGLDAS <- mean(Field_GLDAS_filtered$GLDAS_monthly - Field_GLDAS_filtered$Field_ET_monthly)
pbiasGLDAS <- 100 * sum(Field_GLDAS_filtered$GLDAS_monthly - Field_GLDAS_filtered$Field_ET_monthly) / sum(Field_GLDAS_filtered$Field_ET_monthly)
maeGLDAS <- mean(abs(Field_GLDAS_filtered$GLDAS_monthly - Field_GLDAS_filtered$Field_ET_monthly))

#FLDAS
kge_valueFLDAS <- KGE(sim = Field_FLDAS_filtered$FLDAS_monthly, obs = Field_FLDAS_filtered$Field_ET_monthly)
rmse_valueFLDAS <- rmse(actual = Field_FLDAS_filtered$Field_ET_monthly, predicted = Field_FLDAS_filtered$FLDAS_monthly)
nse_valueFLDAS <- NSE(sim = Field_FLDAS_filtered$FLDAS_monthly, obs = Field_FLDAS_filtered$Field_ET_monthly)
rsr_valueFLDAS <- rmse_valueFLDAS / sd(Field_FLDAS_filtered$Field_ET_monthly)

biasFLDAS <- mean(Field_FLDAS_filtered$FLDAS_monthly - Field_FLDAS_filtered$Field_ET_monthly)
pbiasFLDAS <- 100 * sum(Field_FLDAS_filtered$FLDAS_monthly - Field_FLDAS_filtered$Field_ET_monthly) / sum(Field_FLDAS_filtered$Field_ET_monthly)
maeFLDAS <- mean(abs(Field_FLDAS_filtered$FLDAS_monthly - Field_FLDAS_filtered$Field_ET_monthly))

#SMAP
kge_valueSMAP <- KGE(sim = Field_SMAP_filtered$SMAP_monthly, obs = Field_SMAP_filtered$Field_ET_monthly)
rmse_valueSMAP <- rmse(actual = Field_SMAP_filtered$Field_ET_monthly, predicted = Field_SMAP_filtered$SMAP_monthly)
nse_valueSMAP <- NSE(sim = Field_SMAP_filtered$SMAP_monthly, obs = Field_SMAP_filtered$Field_ET_monthly)
rsr_valueSMAP <- rmse_valueSMAP / sd(Field_SMAP_filtered$Field_ET_monthly)

biasSMAP <- mean(Field_SMAP_filtered$SMAP_monthly - Field_SMAP_filtered$Field_ET_monthly)
pbiasSMAP <- 100 * sum(Field_SMAP_filtered$SMAP_monthly - Field_SMAP_filtered$Field_ET_monthly) / sum(Field_SMAP_filtered$Field_ET_monthly)
maeSMAP <- mean(abs(Field_SMAP_filtered$SMAP_monthly - Field_SMAP_filtered$Field_ET_monthly))

#MOD16
kge_valueMOD16 <- KGE(sim = Field_MOD16_filtered$MOD16_monthly, obs = Field_MOD16_filtered$Field_ET_monthly)
rmse_valueMOD16 <- rmse(actual = Field_MOD16_filtered$Field_ET_monthly, predicted = Field_MOD16_filtered$MOD16_monthly)
nse_valueMOD16 <- NSE(sim = Field_MOD16_filtered$MOD16_monthly, obs = Field_MOD16_filtered$Field_ET_monthly)
rsr_valueMOD16 <- rmse_valueMOD16 / sd(Field_MOD16_filtered$Field_ET_monthly)

biasMOD16 <- mean(Field_MOD16_filtered$MOD16_monthly - Field_MOD16_filtered$Field_ET_monthly)
pbiasMOD16 <- 100 * sum(Field_MOD16_filtered$MOD16_monthly - Field_MOD16_filtered$Field_ET_monthly) / sum(Field_MOD16_filtered$Field_ET_monthly)
maeMOD16 <- mean(abs(Field_MOD16_filtered$MOD16_monthly - Field_MOD16_filtered$Field_ET_monthly))

#SSEBOP
kge_valueSSEBop <- KGE(sim = Field_SSEBop_filtered$SSEBop_monthly, obs = Field_SSEBop_filtered$Field_ET_monthly)
rmse_valueSSEBop <- rmse(actual = Field_SSEBop_filtered$Field_ET_monthly, predicted = Field_SSEBop_filtered$SSEBop_monthly)
nse_valueSSEBop <- NSE(sim = Field_SSEBop_filtered$SSEBop_monthly, obs = Field_SSEBop_filtered$Field_ET_monthly)
rsr_valueSSEBop <- rmse_valueSSEBop / sd(Field_SSEBop_filtered$Field_ET_monthly)

biasSSEBop <- mean(Field_SSEBop_filtered$SSEBop_monthly - Field_SSEBop_filtered$Field_ET_monthly)
pbiasSSEBop <- 100 * sum(Field_SSEBop_filtered$SSEBop_monthly - Field_SSEBop_filtered$Field_ET_monthly) / sum(Field_SSEBop_filtered$Field_ET_monthly)
maeSSEBop <- mean(abs(Field_SSEBop_filtered$SSEBop_monthly - Field_SSEBop_filtered$Field_ET_monthly))

#WAPOR
kge_valueWaPOR <- KGE(sim = Field_WaPOR_filtered$WaPOR_monthly, obs = Field_WaPOR_filtered$Field_ET_monthly)
rmse_valueWaPOR <- rmse(actual = Field_WaPOR_filtered$Field_ET_monthly, predicted = Field_WaPOR_filtered$WaPOR_monthly)
nse_valueWaPOR <- NSE(sim = Field_WaPOR_filtered$WaPOR_monthly, obs = Field_WaPOR_filtered$Field_ET_monthly)
rsr_valueWaPOR <- rmse_valueWaPOR / sd(Field_WaPOR_filtered$Field_ET_monthly)

biasWaPOR <- mean(Field_WaPOR_filtered$WaPOR_monthly - Field_WaPOR_filtered$Field_ET_monthly)
pbiasWaPOR <- 100 * sum(Field_WaPOR_filtered$WaPOR_monthly - Field_WaPOR_filtered$Field_ET_monthly) / sum(Field_WaPOR_filtered$Field_ET_monthly)
maeWaPOR <- mean(abs(Field_WaPOR_filtered$WaPOR_monthly - Field_WaPOR_filtered$Field_ET_monthly))

#PTJPL
kge_valuePTjpl <- KGE(sim = Field_PTjpl_filtered$PTjpl_monthly, obs = Field_PTjpl_filtered$Field_ET_monthly)
rmse_valuePTjpl <- rmse(actual = Field_PTjpl_filtered$Field_ET_monthly, predicted = Field_PTjpl_filtered$PTjpl_monthly)
nse_valuePTjpl <- NSE(sim = Field_PTjpl_filtered$PTjpl_monthly, obs = Field_PTjpl_filtered$Field_ET_monthly)
rsr_valuePTjpl <- rmse_valuePTjpl / sd(Field_PTjpl_filtered$Field_ET_monthly)

biasPTjpl <- mean(Field_PTjpl_filtered$PTjpl_monthly - Field_PTjpl_filtered$Field_ET_monthly)
pbiasPTjpl <- 100 * sum(Field_PTjpl_filtered$PTjpl_monthly - Field_PTjpl_filtered$Field_ET_monthly) / sum(Field_PTjpl_filtered$Field_ET_monthly)
maePTjpl <- mean(abs(Field_PTjpl_filtered$PTjpl_monthly - Field_PTjpl_filtered$Field_ET_monthly))

######################################################################

