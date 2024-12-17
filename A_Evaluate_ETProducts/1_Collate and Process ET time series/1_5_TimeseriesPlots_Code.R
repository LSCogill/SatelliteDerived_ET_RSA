# Plotting time series of ET at a monthly time-step:
# Showing field-measured ET vs satellite-derived ET
##################################################################################

# Install relevant packages
library(hydroGOF) 
library(dygraphs)
library(highcharter)
library(dplyr)
library(xts)
library(zoo)
library(lubridate)
library(readr)

# Set working directory and load the ET dataset of Field_ET_monthly, SMAP_monthly, GLDAS_monthly, MOD16_monthly, Ptjpl_monthly, TC_monthly, WaPOR_monthly
# Benfontein - Site 1: BF1 is used as an example here
setwd("C:\\Users\\Liam\\OneDrive - Stellenbosch University\\ET_Data\\Month_vs_Month_ET_Data")
BF1 <-read.csv("ET_Monthly_BF1.csv",skip=0, sep = ",")
BF1$Month_Year <- as.Date(BF1$Month_Year, format = "%d/%m/%Y")

BF1 <- as.xts(BF1 [, -1], order.by = BF1$Month_Year)
head(BF1)

# Plot the field-measured ET first (in black) and add the satellite-derived ET (in colour) thereafter
plot(BF1$Field_ET_monthly, main = "BF1 - Field ET vs Satellite ET", lwd = 5, xlab = "Date",  ylab = "ET (mm/month)", ylim = c(0,150), yaxt = "n")

lines(BF1$TC_monthly, lwd = 2, col = "green")
lines(BF1$SMAP_monthly, lwd = 2, col = "red")
lines(BF1$WaPOR_monthly, lwd = 2, col = "blue")
lines(BF1$GLDAS_monthly, lwd = 2, col = "orange")
lines(BF1$MOD16_monthly, lwd = 2, col = "purple")
lines(BF1$PTjpl_monthly, lwd = 2, col = "yellow")
