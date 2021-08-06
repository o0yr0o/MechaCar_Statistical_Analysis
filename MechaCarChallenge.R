# 1. Linear Regression to Predict MPG

# load the dplyr package
library(dplyr)

# import and read the file as a dataframe
MechaCar_data <- read.csv(file='MechaCar_mpg.csv', check.names = F, stringsAsFactors = F)

# create linear regression model
MechaCar_regression <- lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, data = MechaCar_data)

# summarize linear model to determine the p-value and the r-squared value
summary(MechaCar_regression)


# 2. Create Visualizations for the Trip Analysis

# import and read the file as a table
Suspension_table <- read.csv(file='Suspension_Coil.csv', check.names = F, stringsAsFactors = F)

# summarize the table to get the mean, median, variance, and sd of the PSI column
total_summary <- Suspension_table %>% summarize(Mean=mean(PSI), Median=median(PSI), Variance=var(PSI), SD=sd(PSI))

# group each manufacturing lot by the mean, median, variance, and sd of the PSI column
lot_summary <- Suspension_table %>% group_by(Manufacturing_Lot) %>% summarize(Mean=mean(PSI), Median=median(PSI), Variance=var(PSI), SD=sd(PSI))


# 3. T-Tests on Suspension Coils

# determine if the PSI across all lots is statistically different from the population mean of 1500
t.test(Suspension_table$PSI, mu=1500)

# determine if the PSI for each lot is statistically different from the population mean of 1500
t.test(subset(Suspension_table, Manufacturing_Lot=='Lot1')$PSI, mu=1500)
t.test(subset(Suspension_table, Manufacturing_Lot=='Lot2')$PSI, mu=1500)
t.test(subset(Suspension_table, Manufacturing_Lot=='Lot3')$PSI, mu=1500)