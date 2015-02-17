## Project 2. plot 4 - Emissions from US coal combustion-related sources for 1999-2008
# Read data file (only this one needed for this plot)
NEI <- readRDS("summarySCC_PM25.rds")

# Read in the source listing
SCC <- readRDS("Source_Classification_Code.rds")

# Pick out only those that have "coal" in the EI.Sector column
coal_sources <- SCC[grep("coal", SCC$EI.Sector, ignore.case=TRUE), ]

# Simplify coal source codes to a vector
x <-as.character(coal_sources$SCC)

# Isolate coal emission readings
coal_emissions <- NEI[NEI$SCC %in% x, ]

# Since the instructions didn't specify to use ggplot, I'll reuse
# some code from plot 1...

# Get sums by year into a new smaller table
year_totals <- aggregate(coal_emissions$Emissions, by=list(coal_emissions$year), FUN=sum)

# Apply meaningful column names
colnames(year_totals) <- c("year", "tons")

# Open PNG plot device with desired name and image dimensions
png("plot4.png", width = 480, height = 480)

# Create plot
plot(year_totals$year, (year_totals$tons / 1000000), type="p", pch=19, xlab="Year",
     ylab="Total (million tons)", main="US Total Coal Emissions by Year",
     xaxp=c(1999, 2008, 3))

# Calculate linear model
model <- lm((year_totals$tons / 1000000) ~ year, year_totals )

# Apply model line to the plot
abline(model, lwd = 2, col="blue")

# Close the device
dev.off()

# Let the user know that the plot file has been generated
print("Plot file has been generated...")



