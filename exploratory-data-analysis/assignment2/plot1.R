setwd("~/Repos/datasciencecoursera//exploratory-data-analysis/assignment2/")

## Read the files
scc <- readRDS("./data/Source_Classification_Code.rds")
summary <- readRDS("./data//summarySCC_PM25.rds")

## Have total emissions from PM2.5 decreased in the United States from 1999 to 2008?

# Calculate Totals
totals <- aggregate(summary$Emissions, by = list(summary$year), FUN = sum)
names(totals) <- c("Year", "Emissions")

# Plot data
png("plot1.png")
barplot(totals$Emissions, 
        col = "lightblue",
        border = FALSE,
        main = "Total PM2.5 Emission per Year",
        names.arg = totals$Year,
        xlab = "Year",
        ylab = "Total PM2.5 Emission")
dev.off()
