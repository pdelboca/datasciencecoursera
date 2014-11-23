setwd("~/Repos/datasciencecoursera//exploratory-data-analysis/assignment2/")
## Across the United States, how have emissions from coal combustion-related 
## sources changed from 1999–2008?

# Load data
scc <- readRDS("./data/Source_Classification_Code.rds")
summary <- readRDS("./data//summarySCC_PM25.rds")

# Subset
scc.coal <- subset(scc, grepl("Coal",scc$EI.Sector))
summary.coal <- subset(summary, SCC %in% scc.coal$SCC)
totals <- with(summary.coal, 
               aggregate(Emissions,by=list(year),FUN = sum))
names(totals) <- c("Year", "Emissions")

# Plot
png("plot4.png", width = 700)
ggplot(totals, aes(Year,Emissions)) + geom_line() + 
    labs(title = "Emissions from coal combustion-related sources across the United States",
         y = "Total PM2.5 Emissions")
dev.off()