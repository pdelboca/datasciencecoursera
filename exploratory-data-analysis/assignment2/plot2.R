setwd("~/Repos/datasciencecoursera//exploratory-data-analysis/assignment2/")

## Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") from 1999 to 2008?

# Load data
scc <- readRDS("./data/Source_Classification_Code.rds")
summary <- readRDS("./data//summarySCC_PM25.rds")

# Subset and Calculate Totals
summary.baltimore <- subset(summary, fips == "24510")
baltimore.totals <- aggregate(summary.baltimore$Emissions, by=list(summary.baltimore$year), FUN = sum)
names(baltimore.totals) <- c("Years", "Emissions")

# Plot data
png("plot2.png")
barplot(baltimore.totals$Emissions,
      main = "Total emissions from PM2.5 in the Baltimore City per Year",
      names.arg = baltimore.totals$Years,
      xlab = "Years",
      ylab = "Emissions from PM2.5")
dev.off()
