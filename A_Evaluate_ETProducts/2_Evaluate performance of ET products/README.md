# Step 2
The second step includes evaluating the performance of the satellite-derived ET products at each of the 14 flux tower stations as well as for grouped biomes. 

** Conducted in R-studio using R notebooks


The evaluation is conducted in R-studio and is comprised of:
2.1) Transform data for the different levels including: Log-transformed, Interannual, and Seasonal
2.2) Testing normality of the data and perform a Correlation between field-measured and satellite-derived ET time series
2.3) Calculating PBias and KGE

## 2.1) Transform time series datasets for different levels
Here data is transformed for analyses within each of the levels
- Overall
- Log transformed
- Interannual
- Seasonal

## 2.2) Normality and Correlation analysis
Here, a Shapiro-Wilks test is performed to test for normality of the data.
A correlation analysis is conducted thereafter to assess the strength and direction of relationship of satellite-derived ET to field-measured ET. 

## 2.3) Calculate other performance metrics
KGE and PBias are calculated for each satellite-derived ET product.
