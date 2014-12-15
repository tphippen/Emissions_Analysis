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

# Question 5: How have emissions from motor vehicle sources changed 
# from 1999–2008 in Baltimore City?

# For this analysis, am defining motor vehicle as On-road Vehicles and Engines
# These vehicles fall into four EI sectors
# 1) Mobile - On-Road Gasoline Light Duty Vehicles
# 2) Mobile - On-Road Diesel Light Duty Vehicles
# 3) Mobile - On-Road Gasoline Heavy Duty Vehicles
# 4) Mobile - On-Road Diesel Heavy Duty Vehicles

# The following are considered light duty vehicles by the EPA:   
# "Passenger cars and light trucks: minivans, passenger vans, 
# pickup trucks, and sport-utility vehicles"
# Heavy duty vehicles include, "Heavy trucks and buses: large pick-ups, 
# delivery trucks, recreational vehicles (RVs), and semi trucks.
# See http://www.epa.gov/otaq/standards/basicinfo.htm

# R Packages required for the running of this script
# Base
# ggplot2
# plyr
library(ggplot2)
library(plyr)

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

# Convert year to a factor
baltimore$year <- as.factor(as.character(baltimore$year))

# Subset SCC$SCC using grepl() to isolate those SCCs 
# that fall into each of the following four categories
# 1) Mobile - On-Road Gasoline Light Duty Vehicles
# 2) Mobile - On-Road Diesel Light Duty Vehicles
# 3) Mobile - On-Road Gasoline Heavy Duty Vehicles
# 4) Mobile - On-Road Diesel Heavy Duty Vehicles
gasLightSCCs <- SCC$SCC[grepl("On-Road Gasoline Light Duty Vehicles", SCC$EI.Sector)]
dieselLightSCCs <- SCC$SCC[grepl("On-Road Diesel Light Duty Vehicles", SCC$EI.Sector)]
gasHeavySCCs <- SCC$SCC[grepl("On-Road Gasoline Heavy Duty Vehicles", SCC$EI.Sector)]
dieselHeavySCCs <- SCC$SCC[grepl("On-Road Diesel Heavy Duty Vehicles", SCC$EI.Sector)]

# Use vectors containing the four categories of SCCs to subest baltimore dataset.
# This generates four dataframes, one for each of the four categories of motor vehicle
gasLight <- baltimore[baltimore$SCC %in% gasLightSCCs,]
dieselLight <- baltimore[baltimore$SCC %in% dieselLightSCCs,]
gasHeavy <- baltimore[baltimore$SCC %in% gasHeavySCCs,]
dieselHeavy <- baltimore[baltimore$SCC %in% dieselHeavySCCs,]

# Add factor variables, Duty and Fuel 
# to each of the four motor vehicle category dataframes
gasLight$Duty <- as.factor(rep("Light (cars & light trucks)", length(gasLight$SCC)))
gasLight$Fuel <- as.factor(rep("Gasoline", length(gasLight$SCC)))
dieselLight$Duty <- as.factor(rep("Light (cars & light trucks)", length(dieselLight$SCC)))
dieselLight$Fuel <- as.factor(rep("Diesel", length(dieselLight$SCC)))
gasHeavy$Duty <- as.factor(rep("Heavy (buses and heavy trucks)", length(gasHeavy$SCC)))
gasHeavy$Fuel <- as.factor(rep("Gasoline", length(gasHeavy$SCC)))
dieselHeavy$Duty <- as.factor(rep("Heavy (buses and heavy trucks)", length(dieselHeavy$SCC)))
dieselHeavy$Fuel <- as.factor(rep("Diesel", length(dieselHeavy$SCC)))

# Merge the four motor vehicle dataframes into a larger one
allMotorVehicle <- rbind(gasLight, dieselLight, gasHeavy, dieselHeavy)

# Split "all motor vehicle" Data Frame by year, duty (light or heavy), and fuel (gasoline or diesel)
# and calculate total emissions for each group
statsYearDutyFuel <- ddply(allMotorVehicle,c("year","Duty","Fuel"), summarise,
                           Total_Emissions = sum(Emissions))
                           
# Add column to split data frame for vehicle classification 
statsYearDutyFuel$Vehicle_Classification <- 
    as.factor(paste(statsYearDutyFuel$Duty, statsYearDutyFuel$Fuel, sep = ", "))

# Generate stacked bar graph showing total PM_2.5 emissions for each vehicle classification
totalGrob <- 
    ggplot(statsYearDutyFuel, aes(x = year, y = Total_Emissions, fill = Vehicle_Classification)) + 
    geom_bar(stat = "identity") +
    labs(title = expression(atop('Total Motor Vehicle PM'[2.5]*' Emissions', paste('for Baltimore City'))),
         y =  expression('Total PM'[2.5]*' Emissions in Tons'), x = "Year", fill = "Vehicle Classification") +
    theme(plot.title = element_text(hjust = 0.5))

# Launch graphics device for png-formatted files
png("plot5.png")

# Write graphic to file
print(totalGrob)
# Close graphics device
dev.off()