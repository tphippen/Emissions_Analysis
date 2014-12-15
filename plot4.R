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

# Question 4: Across the United States, 
# how have emissions from coal combustion-related sources changed from 1999–2008?

# Opted to analyze sources categorized as Fuel Combustion under SCC$EI.Sector
# Out of those sources selected those that pertained to coal combustion.
# Using this selection, source SCCs fall into four categories
# 1) Electric Generation
# 2) Industrial Boilers
# 3) Commercial/Institutional
# 4) Residential 

# R Packages required for the running of this script
# Base
# ggplot2
# plyr
library(ggplot2)
library(plyr)
# Don't need to download package for grid.
# Will be able to load package if you've loaded ggplot2
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

# Generate vectors for each of the four coal combusion categories listed above.
# Each vector contains the SCCs belonging to that category.
electricCoalSCCs <- as.character(SCC$SCC[grepl("Fuel Comb - Electric Generation - Coal", SCC$EI.Sector)])
industrialCoalSCCs <- as.character(SCC$SCC[grepl("Fuel Comb - Industrial Boilers, ICEs - Coal", SCC$EI.Sector)])
institutionalCoalSCCs <- as.character(SCC$SCC[grepl("Fuel Comb - Comm/Institutional - Coal", SCC$EI.Sector)])
residentialCoalSCCs <- as.character(SCC$SCC[grepl("Fuel Comb - Residential - Other", SCC$EI.Sector) & 
                                                grepl("Coal",SCC$Short.Name)])

# Convert NEI$year to a factor so that it can be used to split the data
NEI$year <- as.factor(as.character(NEI$year))

# Use vectors containing the four categories of SCCs to subest large NEI dataset.
# This generates four dataframes, one for each of the four coal combustion categories
electricCoal <- NEI[NEI$SCC %in% electricCoalSCCs,]
industrialCoal <- NEI[NEI$SCC %in% industrialCoalSCCs,]
institutionalCoal <- NEI[NEI$SCC %in% institutionalCoalSCCs,]
residentialCoal <- NEI[NEI$SCC %in% residentialCoalSCCs,]

# Add factor variable, CombustionPurpose to each of the four coal combustion category data frames
electricCoal$CombustionPurpose <- as.factor(rep("Electric Generation", length(electricCoal$SCC)))
industrialCoal$CombustionPurpose <- as.factor(rep("Industrial Boilers", length(industrialCoal$SCC)))
institutionalCoal$CombustionPurpose <- as.factor(rep("Comm/Institutional", length(institutionalCoal$SCC)))
residentialCoal$CombustionPurpose <- as.factor(rep("Residential", length(residentialCoal$SCC)))

# Merge the four coal combustion dataframes into a larger one
totalCoal <- rbind(electricCoal, industrialCoal, institutionalCoal, residentialCoal)

# Split TotalCoal by year and combustion purpose and calculate the following statistics for each group
# 1) total emissions
# 2) mean
# 3) standard error of the mean 
statsByYearPurpose <- ddply(totalCoal, c("year","CombustionPurpose"), summarise,
                     Total_Emissions = sum(Emissions),
                     Mean_Emissions = mean(Emissions), 
                     se = sqrt(var(Emissions)/length(Emissions)))

# First panel of plot will be a stacked bar graph showing total PM_2.5 emissions for coal combustion
totalGrob <- ggplot(statsByYearPurpose, aes(x = year, y = Total_Emissions, fill = CombustionPurpose)) +
    geom_bar(stat = "identity") +
    labs(title = expression(atop('PM'[2.5]*' Emissions from Coal Combustion', paste('Across the United States'))),
         y =  expression('Total PM'[2.5]*' Emissions in Tons'), x = "Year", fill = "Combustion Purpose")

# Because there are unequal numbers of sources contributing to each Combustion Purpose Group, 
# totals for each year may not accurately reflect what is going on for each Combustion Purpose Group.
# Also, compared to the other sectors, residential sector is tiny. Difficult to see on bar graph.
# For second panel, plotted mean emissions for each Combustion Purpose Group against year.
# Each Combustion Purpose group is plotted to a facet since each group has a different range for y values
meansGrob <- ggplot(statsByYearPurpose, aes(x = year, y = Mean_Emissions, group = 1, colour = CombustionPurpose)) +
    geom_line() + geom_point() + 
    geom_errorbar(aes(ymin = Mean_Emissions - se, ymax = Mean_Emissions + se), width = .2) +
    facet_grid(CombustionPurpose ~ ., scales = "free") +
    labs(y = expression('Mean PM'[2.5]*' Emissions in Tons'), x = "Year", colour = "Combustion Purpose")

# Launch graphics device for png-formatted files
png("plot4.png", width = 720, height = 600)

# Write panels to the plot
# To write the panels to the plot, I modified code from Hadley Wickham's book,
# "ggplot2 Elegant Graphics for Data Analysis", section 8.4, page 154 
# published by Springer Science + Business Media LLC, 2009
grid.newpage()
# Initialize plot layout to have one row and four columns
pushViewport(viewport(layout = grid.layout(1,4)))
# Hadley Wickham's function
vplayout <- function(x,y) {
    viewport(layout.pos.row= x, layout.pos.col= y)
}

# Write bar graph panel to the first two columns of the layout
print(totalGrob, vp = vplayout(1,1:2))
# Write faceted panel to the third and fourth column of the layout
print(meansGrob, vp = vplayout(1,3:4))

# Close graphics device
dev.off()