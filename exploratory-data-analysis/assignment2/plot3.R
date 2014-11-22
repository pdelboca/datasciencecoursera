setwd("~/Repos/datasciencecoursera//exploratory-data-analysis/assignment2/")

## Of the four types of sources indicated by the type (point, nonpoint, onroad, 
## nonroad) variable, which of these four sources have seen decreases in 
## emissions from 1999–2008 for Baltimore City? Which have seen increases in 
## emissions from 1999–2008?

# Load data
scc <- readRDS("./data/Source_Classification_Code.rds")
summary <- readRDS("./data//summarySCC_PM25.rds")

# Subset
summary.baltimore <- subset(summary, fips == "24510")
totals <- aggregate(summary.baltimore$Emissions, 
                    by = list(summary.baltimore$type, summary.baltimore$year),
                    FUN = sum)
names(totals) <- c("type","year","Emission")

# Plot
library(ggplot2)
png("plot3.png")
ggplot(totals, aes(year, Emission,group=type)) + geom_line(aes(color=type)) +
    labs(title = "Emission Totals in Baltimore City by Type and Year",
         x = "Year",
         y = "Total Emission")
dev.off()