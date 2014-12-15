# Exploratory Data Analysis Course Project 2
# Assignment Goal: explore the National Emissions Inventory database 
# and see what it say about fine particulate matter pollution in the 
# United States over the 10-year period 1999–2008

# Datasets: 
# 1) PM2.5 Emissions Data (summarySCC_PM25.rds): 
# This file contains a data frame with all of the PM2.5 emissions data 
# for 1999, 2002, 2005, and 2008. 
# 2) Source Classification Code Table (Source_Classification_Code.rds): 
# This table provides a mapping from the SCC digit strings in the Emissions 
# table to the actual name of the PM2.5 source.

# Question 1: Have total emissions from PM2.5 decreased in the United States 
# from 1999 to 2008? Using the base plotting system, make a plot showing 
# the total PM2.5 emission from all sources for each of the years 
# 1999, 2002, 2005, and 2008.

# If a directory has not been created to hold PM25 data, make one.
if(!file.exists("PM25_Data")) {
    dir.create("PM25_Data")
}

# Download zip file
zipURL <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
download.file(zipURL,"./PM25_Data/temp",method = "curl")

# unzip zip file
unzip("./PM25_Data/temp", exdir = "./PM25_Data/")
# Delete zip file
unlink("./PM25_Data/temp")

# Read data frames from files
NEI <- readRDS("./PM25_Data/SummarySCC_PM25.rds")
SCC <- readRDS("./PM25_Data/Source_Classification_Code.rds")

# Convert NEI$year to a factor and use this factor to split the data 
NEI$year <- as.factor(as.character(NEI$year))
splitByYear <- split(NEI,NEI$year)

# Total emissions for each year
yearlyTotals <- sapply(splitByYear, function(x) sum(x$Emissions,na.rm = TRUE))

# Launch graphics device for png-formatted files
png("plot1.png")

# Expand left marigin by 1
par(mar = c(5.1,5.1,4.1,1.1))

# Plot yearly totals 
barplot(yearlyTotals)
title(main = expression('Total Triennial PM'[2.5]*' Emissions for the United States'),
      xlab = 'Year', ylab = expression('Total PM'[2.5]*' Emissions in Tons'))
# Close graphics device
dev.off()