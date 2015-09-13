setwd("~/Repos/datasciencecoursera//exploratory-data-analysis/assignment2/")

## Compare emissions from motor vehicle sources in Baltimore City with emissions
## from motor vehicle sources in Los Angeles County, California (fips == "06037").
## Which city has seen greater changes over time in motor vehicle emissions?

# Load data
scc <- readRDS("./data/Source_Classification_Code.rds")
summary <- readRDS("./data//summarySCC_PM25.rds")

# Subset
scc.motor.vehicle <- subset(scc, grepl("Vehicles",scc$EI.Sector))
summary.cities <- subset(summary, fips == "24510" | fips == "06037")
summary.cities.vehicles <- subset(summary.cities, 
                                  summary.cities$SCC %in% scc.motor.vehicle$SCC)

# Sumary
totals <- with(summary.cities.vehicles,
               aggregate(Emissions, by=list(fips,year), FUN = sum))
names(totals) <- c("City","Year","Emissions")
totals$City <- gsub("24510", "Baltimore", totals$City)
totals$City <- gsub("06037", "Los Angeles", totals$City)

# Plot
png("plot6.png", width = 700)
ggplot(totals, aes(Year,Emissions)) + 
    geom_point(aes(colour = City), size = 3) +
    geom_line(aes(color=City)) +
    labs(title = "Emissions from motor vehicle sources in Baltimore City vs Los Angeles County, California.") +
    theme_minimal()
dev.off()
