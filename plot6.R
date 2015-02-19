## Project 2. plot 6 - Which city has seen greater changes over time,
## in motor vehicle emissions: Baltimore City or Los Angles County?

# Read data file
NEI <- readRDS("summarySCC_PM25.rds")

# Read in the source listing
SCC <- readRDS("Source_Classification_Code.rds")

# Pick out only those that have "Highway vehiles gasoline" in the SCC.Level.Two column
motor_vehicle_sources <- SCC[grep("highway vehicles - gasoline", SCC$SCC.Level.Two, ignore.case=TRUE), ]

# Simplify motor vehicle sources codes to a vector
x <-as.character(motor_vehicle_sources$SCC)

# Isolate gasoline emission readings
motor_vehicle_emissions <- NEI[NEI$SCC %in% x, ]

# Get Baltimore City emissions
baltimore_motor_vehicle_emissions <- subset(motor_vehicle_emissions, fips == "24510")

# Get Los Angles County emissions
los_angles_motor_vehicle_emissions <- subset(motor_vehicle_emissions, fips == "06037")

# Get sums by year into a new smaller tables
baltimore_year_totals <- aggregate(baltimore_motor_vehicle_emissions$Emissions, by=list(baltimore_motor_vehicle_emissions$year), FUN=sum)
los_angles_year_totals <- aggregate(los_angles_motor_vehicle_emissions$Emissions, by=list(los_angles_motor_vehicle_emissions$year), FUN=sum)

# Apply meaningful column names
colnames(baltimore_year_totals) <- c("year", "tons")
colnames(los_angles_year_totals) <- c("year", "tons")

# To make a comparison, calculate percent change
baltimore_year_totals$percent <- round(baltimore_year_totals[, "tons"] / baltimore_year_totals[1, "tons"], digits = 3) * 100
los_angles_year_totals$percent <- round(los_angles_year_totals[, "tons"] / los_angles_year_totals[1, "tons"], digits = 3) * 100

# Append locale onto each table
baltimore_year_totals$locale <- "Baltimore"
los_angles_year_totals$locale <- "Los Angles"

# Add meaningful column names
colnames(baltimore_year_totals) <- c("year", "tons", "Percent","locale")
colnames(los_angles_year_totals) <- c("year", "tons", "Percent","locale")

# Combine into one table to make using ggplot simpler
year_percent <- rbind(baltimore_year_totals, los_angles_year_totals)

# Load ggplot2
# install.packages("ggplot2")       # Needed the first time
library(ggplot2)

# Create base
q <- ggplot(year_percent, aes(year, Percent))

#Build up the plot by adding layers
# NOTE: the + must be on the end of preceeding lines
p <- q + geom_point() +
  facet_grid(. ~ locale) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "PerCent Emissions by Year") +
  labs(x = "Year") +
  labs(y = "PerCent") +
  scale_x_continuous(breaks = c(1999, 2002, 2005, 2008)) +
  geom_text(aes(label=Percent),hjust=0.5, vjust=2)

# Increase the spacing between facet panels so that year labels don't overlap
library(grid)
r <- p + theme(panel.margin = unit(1, "lines"))

# Open PNG plot device with desired name and image dimensions
png("plot6.png", width = 900, height = 480)

# Plot
print(r)

# Close the device
dev.off()

# Let the user know that the plot file has been generated
print("Plot file has been generated...")


