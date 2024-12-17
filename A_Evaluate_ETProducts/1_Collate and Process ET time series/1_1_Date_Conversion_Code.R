# Date formatting is comprised of two options:
# 1) If data is in a 30min processed format, it needs to be formatted for gap filling in ReddyProc
# 2) If data is in a daily format, it cannot be gap filled with Reddy Proc and dates are formatted for the next step which is aggregating daily ET to monthly ET
#####################################################################################################################################################################

# 1) 30min date/timestamp formatting:
# Convert certain date or timestamp formats to the standard 'Y/m/d H:M' format for ReddyProc 

# Install relevant packages/libraries
library(readr)
library(lubridate)

# set working directory and read in the csv with the incorrect date/timestamp format
setwd("C:\\Users\\Liam\\OneDrive - Stellenbosch University\\ET_Data\\Raw_Field_Data")
df<-read.csv("__Site Code Here__.csv",skip=0, sep = ",")

# Sample data frame with a column named TIMESTAMP
df <- data.frame(d$TIMESTAMP, stringsAsFactors = FALSE)

# Convert TIMESTAMP column to datetime object
df$d.TIMESTAMP <- strptime(df$d.TIMESTAMP, format = "%Y-%m-%d %H:%M")
# OR: df$d.TIMESTAMP <- strptime(df$d.TIMESTAMP, format = "%Y%m%d%H%M")
# OR: df$d.TIMESTAMP <- strptime(df$d.TIMESTAMP, format = "%Y%m%d%H%M%S")

# Format TIMESTAMP column as per desired output format
df$d.TIMESTAMP <- format(df$d.TIMESTAMP, format = "%Y/%m/%d %H:%M")

#Rename var names
colnames(df)[colnames(df) == "d.TIMESTAMP"] <- "DATE"

# Print the result
print(df)

# Write the csv to the respective folder to run ReddyProc
write.csv(df, "_Site Code Here_ET/LE.csv", row.names = FALSE)

########################################################################################################################

# 2) Daily ET formatting
# Convert certain date or timestamp formats to the standard 'Y/m/d H:M' format for monthly aggregating 

# Install relevant libraries
library(readr)
library(lubridate)

# set working directory and read in the csv with the incorrect date/timestamp format
setwd("C:\\Users\\Liam\\OneDrive - Stellenbosch University\\ET_Data\\Raw_Field_Data")
df<-read.csv("__Site Code Here__.csv",skip=0, sep = ",")

# Sample data frame with a column named TIMESTAMP
df <- data.frame(d$TIMESTAMP, stringsAsFactors = FALSE)

# Convert TIMESTAMP column to datetime object
df$d.TIMESTAMP <- strptime(df$d.TIMESTAMP, format = "%m/%d/%Y %H:%M:%S")
# OR: df$d.TIMESTAMP <- strptime(df$d.TIMESTAMP, format = "%m/%d/%Y")

# Format TIMESTAMP column as per desired output format
df$d.TIMESTAMP <- format(df$d.TIMESTAMP, format = "%Y/%m/%d %H:%M")

#Rename var names
colnames(df)[colnames(df) == "d.TIMESTAMP"] <- "DATE"

# Print the result
print(df)

# Write the csv to the respective folder to run ReddyProc
write.csv(df, "_Site Code Here_DAILY_ET/LE.csv", row.names = FALSE)