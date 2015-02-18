## Project 2. plot 5 - Emissions from motor vehicles 1999–2008 in Baltimore City
# Read data file (only this one needed for this plot)
NEI <- readRDS("summarySCC_PM25.rds")

# Read in the source listing
SCC <- readRDS("Source_Classification_Code.rds")

# Pick out only those that have "Highway vehiles gasoline" in the SCC.Level.Two column
motor_vehicle_sources <- SCC[grep("highway vehicles - gasoline", SCC$SCC.Level.Two, ignore.case=TRUE), ]

# Simplify motor vehicle sources codes to a vector
x <-as.character(motor_vehicle_sources$SCC)

# Isolate gasoline emission readings
motor_vehicle_emissions <- NEI[NEI$SCC %in% x, ]

# Narrow to Baltimore City only
baltimore_motor_vehicle_emissions <- subset(motor_vehicle_emissions, fips == 24510)

# Since the instructions didn't specify to use ggplot, I'll reuse
# some code from plot 1...

# Get sums by year into a new smaller table
year_totals <- aggregate(baltimore_motor_vehicle_emissions$Emissions, by=list(baltimore_motor_vehicle_emissions$year), FUN=sum)

# Apply meaningful column names
colnames(year_totals) <- c("year", "tons")

# Open PNG plot device with desired name and image dimensions
png("plot5.png", width = 480, height = 480)

# Create plot
plot(year_totals$year, (year_totals$tons / 1), type="p", pch=19, xlab="Year",
     ylab="Total (tons)", main="Baltimore City Total Motor Vehicle Emissions by Year",
     xaxp=c(1999, 2008, 3))

# Calculate linear model
model <- lm((year_totals$tons / 1) ~ year, year_totals )

# Apply model line to the plot
abline(model, lwd = 2, col="blue")

# Close the device
dev.off()

# Let the user know that the plot file has been generated
print("Plot file has been generated...")



