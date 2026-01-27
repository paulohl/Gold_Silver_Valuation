# Install and load necessary packages
install.packages("readxl")
install.packages("ggplot2")
install.packages("xts")

library(readxl)
library(ggplot2)
library(xts)

# Read data from Excel file
gold_data <- read_excel("Investment_Historic.xlsx", sheet = "gold_price")

# Extract date and price columns
date <- as.POSIXct(gold_data$date)
price <- as.numeric(gold_data$GoldSpot)

# Impute missing values using Interpolation.

price[is.na(price)] <- mean(price, na.rm = TRUE)

# Create a time series object with one observation per month
gold_xts <- xts(gold_data$GoldSpot, order.by = as.yearmon(gold_data$date))

# Calculate maximum, minimum, and mean values
max_value <- max(price)
min_value <- min(price)
mean_value <- mean(price)

# Find dates corresponding to maximum and minimum values
index_max <- which.max(price)
index_min <- which.min(price)

# Find dates corresponding to maximum and minimum values
date_max <- gold_xts[which.max(price)]
date_min <- gold_xts[which.min(price)]

date_max <- index(gold_xts)[index_max]
date_min <- index(gold_xts)[index_min]

# Convert date_max and date_min to POSIXct format
date_max <- as.POSIXct(date_max)
date_min <- as.POSIXct(date_min)

# Create a ggplot object
p <- ggplot(gold_data, aes(x = date, y = price)) + 
	geom_line() + 
	labs(title="Historic Gold Prices", x="Date", y="Price (USD)") 

# Add points for maximum and minimum values
p <- p + geom_point(data=data.frame(x=date_max, y=max_value),  
                    aes(x=x, y=y), color="red", size=3)
p <- p + geom_point(data=data.frame(x=date_min, y=min_value),  
                    aes(x=x, y=y), color="blue", size=3) 

# Add mean line
p <- p + geom_hline(yintercept=mean_value, 
                    linetype="dashed", color="green") 

# Display the plot
plot(p) 
