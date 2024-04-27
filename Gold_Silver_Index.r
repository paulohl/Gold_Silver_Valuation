# Install and load necessary packages
install.packages("readxl")
install.packages("ggplot2")
install.packages("xts")

library(readxl)
library(ggplot2)
library(xts)

# Read data from Excel file
silver_data <- read_excel("Investment_Historic.xlsx", sheet = "silver_price")

# Check the length of the data frame provided to ggplot()
# nrow(silver_data)
#
# If the lengths don't match, you may need to filter or subset your data
# For example, you can filter out rows with missing values in price
# silver_data <- silver_data[!is.na(price), ]

# Extract date and price columns
date <- as.POSIXct(silver_data$date)
price <- as.numeric(silver_data$SilverSpot)

# Remove rows with missing values: You can remove rows with missing values using the na.omit() function.
# silver_data <- na.omit(silver_data)
#
# Impute missing values: You can impute missing values using various methods such as mean imputation or 
# Interpolation.

price[is.na(price)] <- mean(price, na.rm = TRUE)

# Create a time series object with one observation per month
silver_xts <- xts(silver_data$SilverSpot, order.by = as.yearmon(silver_data$date))

# Calculate maximum, minimum, and mean values
max_value <- max(price)
min_value <- min(price)
mean_value <- mean(price)

# Find dates corresponding to maximum and minimum values
index_max <- which.max(price)
index_min <- which.min(price)

date_max <- index(silver_xts)[index_max]
date_min <- index(silver_xts)[index_min]

# Convert date_max and date_min to POSIXct format
date_max <- as.POSIXct(date_max)
date_min <- as.POSIXct(date_min)

# Create a ggplot object
p <- ggplot(silver_data, aes(x = date, y = price)) + 
	geom_line() + 
	labs(title="Historic Silver Prices", x="Date", y="Price (USD)")

# Add points for maximum and minimum values
p <- p + geom_point(data = data.frame(x = date_max, y = max_value),  
                    aes(x = x, y = y), color = "red", size = 3)
p <- p + geom_point(data = data.frame(x = date_min, y = min_value),  
                    aes(x = x, y = y), color = "blue", size = 3) 

# Add mean line
p <- p + geom_hline(yintercept = mean_value, 
                    linetype = "dashed", color = "green") 
# Display the plot
plot(p) 
