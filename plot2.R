## Project 2. plot 2 - Total emissions from 1999 to 2008 for Baltimore City, MD
# Read data file (only this one needed for this plot)
NEI <- readRDS("summarySCC_PM25.rds")

# Narrow to Baltimore City only
baltimore <- subset(NEI, fips == 24510)

# Get sums by year into a new smaller table
year_totals <- aggregate(baltimore$Emissions, by=list(baltimore$year), FUN=sum)

# Apply meaningful column names
colnames(year_totals) <- c("year", "tons")

# Open PNG plot device with desired name and image dimensions
png("plot2.png", width = 480, height = 480)

# Create plot
plot(year_totals$year, (year_totals$tons), type="p", pch=19, xlab="Year",
  ylab="Total (tons)", main="Baltimore City Total Emissions by Year",
  xaxp=c(1999, 2008, 3))

# Calculate linear model
model <- lm((year_totals$tons) ~ year, year_totals )

# Apply model line to the plot
abline(model, lwd = 2, col="blue")

# Close the device
dev.off()

# Let the user know that the plot file has been generated
print("Plot file has been generated...")
