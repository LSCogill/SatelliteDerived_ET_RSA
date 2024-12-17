# Sense check original LE vs gap filled LE
############################################################################################

# Load the ggplot2 package
library(ggplot2)

# Set working directory and read in gapfilled csv
setwd("C:\\Users\\Liam\\OneDrive - Stellenbosch University\\ET_Data\\Gapfilled_Field_Data")
df<-read.csv("__Site Code Here__.csv",skip=0, sep = ",")

d[d == "NaN"] <- NA
d[d == "NAN"] <- NA

# Create a data frame which includes the filled data (x) and original data (y)
x <- d$LE_uStar_f
y <- d$LE_uStar_orig
data <- data.frame(x, y)

# Summary of data frame establish min and max for the plot
summary(data)
range(data$x)

min_x <- min(x)
max_x <- max(x)

print(min_x)
print(max_x)

# Find indices of NA values
na_indices <- which(is.na(y))

# Plot the original vs gap filled data
plot(x, y, xlab = "X", ylab = "Y", main = "Filled (X) vs Unfilled (Y)", col = ifelse(is.na(y), "black", "black"))
legend("topright", legend = c("NA", "Data"), col = c("black", "black"), pch = 1)
plot(x, y, xlab = "X", ylab = "Y", main = "Filled (X) vs Unfilled (Y)")


