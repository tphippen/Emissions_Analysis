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

# Question 3: Of the four types of sources indicated by the type 
# (point, nonpoint, onroad, nonroad) variable, which of these four 
# sources have seen decreases in emissions from 1999–2008 for Baltimore City? 
# Which have seen increases in emissions from 1999–2008? 
# Use the ggplot2 plotting system to make a plot answer this question.

# R Packages required for the running of this script
# Base
# ggplot2
# plyr
library(ggplot2)
library(plyr)
library(grid)

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

# subset data by fips == "24510 to isolate all records for Baltimore City, Maryland
baltimore <- NEI[NEI$fips == "24510",]

# Convert year and source type to factors
baltimore$year <- as.factor(as.character(baltimore$year))
baltimore$type <- as.factor(baltimore$type)

# Split data by year and type
# Calculate the total emissions, mean and standard error of the mean 
# for each group
baltByYearType <- ddply(baltimore, c("year", "type"), summarise,
                  Total_Emissions = sum(Emissions),
                  Mean_Emissions = mean(Emissions), 
                  se = sqrt(var(Emissions)/length(Emissions)))


# First panel of plot will be a bar graph showing total PM_2.5 emissions for each type 
# for the years 1999, 2002, 2005, and 2008
totalGrob <- ggplot(baltByYearType, aes(x = type, y = Total_Emissions, fill = year)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = expression(atop('Triennial PM'[2.5]*' Emissions',
                                 paste('for Baltimore City by Source Type'))),
         y = expression('Total PM'[2.5]*' Emissions in Tons'),
         x = "Source Type")

# Because there are unequal numbers of records contributing to each source type, 
# totals for each year may not accurately reflect the situation for each source type
# For second panel, plotted mean emissions for each source type against year.
# Each type is plotted to a facet since each type has a different range for y values
meansGrob <- ggplot(baltByYearType, aes(x = year, y = Mean_Emissions, group = 1)) +
    geom_line() + geom_point() + 
    geom_errorbar(aes(ymin = Mean_Emissions - se, ymax = Mean_Emissions + se), width = .2) +
    facet_grid(type ~ ., scales = "free") +
    labs(y = expression('Mean PM'[2.5]*' Emissions in Tons'), x = "Year")


# Launch graphics device for png-formatted files
png("plot3.png", width = 720, height = 600)

# Write panels to the plot
# To write the panels to the plot, I modified code from Hadley Wickham's book,
# "ggplot2 Elegant Graphics for Data Analysis", section 8.4, page 154 
# published by Springer Science + Business Media LLC, 2009
grid.newpage()
# Initialize plot layout to have one row and five columns
pushViewport(viewport(layout = grid.layout(1,5)))

# Hadley Wickham's function
vplayout <- function(x,y) {
    viewport(layout.pos.row= x, layout.pos.col= y)
}

# Write totals bar graph panel to the first three columns of the layout
print(totalGrob, vp = vplayout(1,1:3))

# Write faceted panel to the fourth and fifth column of the layout
print(meansGrob, vp = vplayout(1,4:5))

# Close graphics device
dev.off()
