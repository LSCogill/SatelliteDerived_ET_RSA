# Aggregate field-measured data to monthly ET. This is comprised of two options:
# 1) For datasets that ha been gapfilled, aggregate the 30min ET/LE to Daily ET/LE; then proceed to (2) which includes aggregating Daily ET to Monthly ET
# 2) For data that is already in Daily ET format, aggregate Daily ET to Monthly ET
################################################################################################

# 1) Aggregate 30min to Daily ET

# Install relevant packages 
library(dplyr)
library(xts)
library(lubridate)
library(readr)

# Set working directory and read in the gap filled csv
setwd("C:\\Users\\Liam\\OneDrive - Stellenbosch University\\ET_Data\\Gapfilled_Field_Data")
d<-read.csv("__Site Code Here__gf.csv",skip=0, sep = ",")

d[d == "NaN"] <- NA
d[d == "NAN"] <- NA

# Sample data frame with a column named TIMESTAMP
df <- data.frame(d$DateTime, d$LE_uStar_f, stringsAsFactors = FALSE)

# Convert TIMESTAMP column to datetime object
df$d.DateTime <- strptime(df$d.DateTime, format = "%Y/%m/%d %H:%M")

# Format TIMESTAMP column as per desired output format
# This isolates all readings for the respective day to have the same date and can be aggregated from there
df$d.DateTime <- format(df$d.DateTime, format = "%Y%m%d")

#Rename variable names
colnames(df)[colnames(df) == "d.DateTime"] <- "DATE"
colnames(df)[colnames(df) == "d.LE_uStar_f"] <- "DAILY_LE"
print(df)

# Group by day and sum values
# *NOTE: NA values (values that could not be gap filled) will result in a NA value for the whole day
daily_sum <- df %>%
  group_by(DATE) %>%
  summarise(Sum = sum(DAILY_LE))

# Display the result
print(daily_sum)

# Write the csv to the Daily ET folder
setwd("C:\\Users\\Liam\\OneDrive - Stellenbosch University\\ET_Data\\Daily_Agg_Field_Data")
write.csv(daily_sum, "__Site Code Here__LE.csv", row.names = FALSE)


############################################################################################

# 2) Aggregate Daily ET to Monthly ET

# Sample data frames
setwd("C:\\Users\\Liam\\OneDrive - Stellenbosch University\\ET_Data\\Daily_Agg_Field_Data")
d<-read.csv("__Site Code Here__LE.csv",skip=0, sep = ",")   
d[d == "NaN"] <- NA
d[d == "NA"] <- NA

# Sample data frame with a column named TIMESTAMP
df <- data.frame(d$DATE, d$Sum, stringsAsFactors = FALSE)

# Convert TIMESTAMP column to datetime object
df$d.DATE <- strptime(df$d.DATE, format = "%Y%m%d")

# Format TIMESTAMP column as per desired output format
# This isolates all readings for the respective month to have the same date and can be aggregated from there
df$d.DATE <- format(df$d.DATE, format = "%Y/%m")

# Print the result
print(df)

# Group by month and sum values
# *NOTE: NA values (values that could not be gap filled) will result in a NA value for the whole month
monthly_sum <- df %>%
  group_by(d.DATE) %>%
  summarise(Sum = sum(d.Sum))

# Display the result
print(monthly_sum)

# Write the csv to the Monthly ET folder
setwd("C:\\Users\\Liam\\OneDrive - Stellenbosch University\\ET_Data\\Monthly_Agg_Field_Data")
write.csv(monthly_sum, "__Site Code Here__Monthly_fieldET.csv", row.names = FALSE)
