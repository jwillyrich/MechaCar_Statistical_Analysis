# Part 1: Linear Regression to Predict MPG
# Step 3 >> Use the library() function to load the dplyr package.
library(dplyr)

# Step 4 >> Import and read in the MechaCar_mpg.csv file as a dataframe.
MechaCar_MPG <- read.csv(file='MechaCar_mpg.csv', check.names=F, stringsAsFactors = F)
# Dislay the top 6 rows of the new DataFrame
head(MechaCar_MPG)                       

# Step 5 >> Perform linear regression using the lm() function. In the lm() function, pass in all six variables (i.e,. columns) 
# and add the dataframe you created in Step 4 as the data parameter.
lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, data=MechaCar_MPG)

# Step 6 >> Using the summary() function, determine the p-value and the r-squared value for the linear 
# regression model.
summary(lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, data=MechaCar_MPG))

# Part 2: Create Visualizations for the Trip Analysis
# Step 2 >> In your MechaCarChallenge.RScript, import and read in the Suspension_Coil.csv file as a table
suspCoils_table <- read.csv(file='Suspension_Coil.csv', check.names=F, stringsAsFactors = F)
# Dislay the top 6 rows of the new table
head(suspCoils_table)

# Step 3 >> Write an RScript that creates a total_summary dataframe using the summarize() function to get the mean, 
# median, variance, and standard deviation of the suspension coil’s PSI column
total_summary <- suspCoils_table %>% summarise(Mean=mean(PSI), Median=median(PSI), Variance=var(PSI), SD=sd(PSI), .groups = 'keep')

# Step 4 >> Write an RScript that creates a lot_summary dataframe using the group_by() and the summarize() functions to group 
# each manufacturing lot by the mean, median, variance, and standard deviation of the suspension coil’s PSI column
lot_summary <- suspCoils_table %>% group_by(Manufacturing_Lot) %>% summarise(Mean=mean(PSI), Median=median(PSI), Variance=var(PSI), SD=sd(PSI), .groups = 'keep')

# Part 3: T-Tests on Suspension Coils
# Step 1 >> Write an RScript using the t.test() function to determine if the PSI across all manufacturing lots 
# is statistically different from the population mean of 1,500 pounds per square inch

# Method 1: Create smaller subsets of data then run t.test()
allCoilData <- suspCoils_table['PSI']

# Method 1 con't: Run t.test with data tables created from above
t.test(allCoilData[["PSI"]], mu=1500)

# Method 2: Run the t.test without creating subsets data 
t.test(suspCoils_table$PSI, mu=1500)

# Step 2 >> Next, write three more RScripts in your MechaCarChallenge.RScript using the t.test() 
# function and its subset() argument to determine if the PSI for each manufacturing lot is statistically 
# different from the population mean of 1,500 pounds per square inch.
lot1dataset <- subset(suspCoils_table, Manufacturing_Lot == 'Lot1')
lot2dataset <- subset(suspCoils_table, Manufacturing_Lot == 'Lot2')
lot3dataset <- subset(suspCoils_table, Manufacturing_Lot == 'Lot3')

# Method 1 con't: Run t.test with data tables created from above
t.test(lot1dataset$PSI, mu=1500)
t.test(lot2dataset$PSI, mu=1500)
t.test(lot3dataset$PSI, mu=1500)

# Method 2: Run the t.test without creating subsets data 
t.test(x=subset(suspCoils_table, Manufacturing_Lot == "Lot1")$PSI, mu=1500)
t.test(x=subset(suspCoils_table, Manufacturing_Lot == "Lot2")$PSI, mu=1500)
t.test(x=subset(suspCoils_table, Manufacturing_Lot == "Lot3")$PSI, mu=1500)

# Part 4: Design a Study Comparing the MechaCar to the Competition