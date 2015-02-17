## Project 2. plot 3 - Baltimore City Total Emossions by Type and Year
# Read data file (only this one needed for this plot)
NEI <- readRDS("summarySCC_PM25.rds")

# Narrow to Baltimore City only
baltimore <- subset(NEI, fips == 24510)

# Get sums by year, and by type, into a new smaller table
year_totals <- aggregate(baltimore$Emissions, by=list(baltimore$year), FUN=sum)
year_totals <- aggregate(baltimore$Emissions, by=list(baltimore$year, baltimore$type), FUN=sum)

# Apply meaningful column names
colnames(year_totals) <- c("year", "type", "tons")

# Load ggplot2
# install.packages("ggplot2")       # Needed the first time
library(ggplot2)

# Create base
q <- ggplot(year_totals, aes(year, tons))

#Build up the plot by adding layers
# NOTE: the + must be on the end of preceeding lines
p <- q + geom_point() +
  facet_grid(. ~ type) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Baltimore City Total Emissions by Type and Year") +
  labs(x = "Year") +
  labs(y = "Emissions (tons)") +
  scale_x_continuous(breaks = c(1999, 2002, 2005, 2008))

# Increate the spacing between facet panels so that year labels don't overlap
library(grid)
r <- p + theme(panel.margin = unit(1, "lines"))

# Open PNG plot device with desired name and image dimensions
png("plot3.png", width = 900, height = 480)

# Plot
print(r)

# Close the device
dev.off()

# Let the user know that the plot file has been generated
print("Plot file has been generated...")

