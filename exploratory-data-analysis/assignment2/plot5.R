setwd("~/Repos/datasciencecoursera//exploratory-data-analysis/assignment2/")
## How have emissions from motor vehicle sources changed from 1999–2008 in 
## Baltimore City?

# Load data
scc <- readRDS("./data/Source_Classification_Code.rds")
summary <- readRDS("./data//summarySCC_PM25.rds")

# Subset
scc.motor.vehicle <- subset(scc, grepl("Vehicles",scc$EI.Sector))
summary.baltimore <- subset(summary, fips == "24510")
summary.baltimore.vehicles <- subset(summary.baltimore, 
                                     summary.baltimore$SCC %in% scc.motor.vehicle$SCC)


# Summary
totals <- with(summary.baltimore.vehicles,
                 aggregate(Emissions, by = list(year), FUN = sum))
names(totals) <- c("Year","Emissions")

# Plot
library(ggplot2)
png("plot5.png",width = 550)
ggplot(totals, aes(Year,Emissions)) + geom_line() + 
    labs(title = "Emissions from motor vehicle sources from 1999–2008 in Baltimore City")
dev.off()
