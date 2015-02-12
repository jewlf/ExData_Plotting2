## Project 2. plot 1 - Total emissions from 1999 to 2008
# Read data file
NEI <- readRDS("summarySCC_PM25.rds")

# Get sums by year
year_totals <- aggregate(NEI$Emissions, by=list(NEI$year), FUN=sum)

# Apply meaningful column names
colnames(year_totals) <- c("year", "tons")

# Open PNG plot device with desired name and image dimensions
png("plot2.png", width = 480, height = 480)

# Create plot
plot(year_totals$year, (year_totals$tons / 1000000), type="p", pch=19, xlab="Year", ylab="Total (million tons)", main="Total Emissions by Year", xaxp=c(1999, 2008, 3))

# Calculate linear model
model <- lm((year_totals$tons / 1000000) ~ year, year_totals )

# Apply model line to plot
abline(model, lwd = 2, col="blue")

# Close the device
dev.off()

# Let the user know that the plot file has been generated
print("Plot file has been generated...")
