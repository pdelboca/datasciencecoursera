setwd("~/Repos/datasciencecoursera//exploratory-data-analysis/assignment2/")

scc <- readRDS("./data/Source_Classification_Code.rds")
summary <- readRDS("./data//summarySCC_PM25.rds")

## Have total emissions from PM2.5 decreased in the United States from 1999 to 2008?

totals <- aggregate(summary$Emissions, by = list(summary$year), FUN = sum)
names(totals) <- c("Year", "Emissions")

png("plot1.png")
plot(totals$Year,totals$Emissions, 
     main = "Total PM2.5 Emission per Year",
     xlab = "Year",
     ylab = "Total PM2.5 Emission",
     pch = 20)
dev.off()
