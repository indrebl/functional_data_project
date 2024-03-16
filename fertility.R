library(readxl)
library(tidyverse)
library(fda)
library(dplyr)
library(tidyr)

data <- read_excel("C:/Users/tucas/Downloads/undesa_pd_2019_world_fertility_dataset.xlsx", 
                   sheet = "FERTILITY INDICATORS", skip = 6)

data <- data[, 1:6]

# Filter out rows where the fourth column (Indicators) contains "TFR"
data_filtered <- filter(data, grepl("TFR", data[[4]]))

# Also we filter out all the useless variables

data_filtered <- data_filtered[, -c(2, 3, 4)]

# Now we only take 20 largest european countries and 20 largest asian countries

largest_european_countries <- c("Russian Federation", "Germany", "United Kingdom", "France",
                                "Italy", "Spain", "Ukraine", "Poland", "Romania", "Netherlands",
                                "Belgium", "Greece", "Czechia", "Portugal", "Sweden", "Hungary",
                                "Belarus", "Austria", "Switzerland", "Serbia")

largest_asian_countries <- c("China", "India", "Indonesia", "Pakistan", "Bangladesh", 
                             "Japan", "Philippines", "Vietnam", "Iran", "Turkey", 
                             "Thailand", "Myanmar", "South Korea", "Iraq", "Afghanistan", 
                             "Saudi Arabia", "Uzbekistan", "Malaysia", "Yemen", "Nepal")

country_data <- data_filtered %>%
  filter(`Country or Area` %in% c(largest_european_countries, largest_asian_countries))

# Add a new column for the continent
final_data <- country_data %>%
  mutate(Continent = ifelse(`Country or Area` %in% largest_european_countries, "Europe", "Asia"))

# Apply logarithmic transformation
final_data$ValueLog = log(final_data$Value)


# Draw the graph
ggplot(final_data, aes(x = Date, y = ValueLog, group = `Country or Area`, color = `Continent`)) +
  geom_line() + # Draw lines for each country
  scale_color_manual(values = c("Europe" = "blue", "Asia" = "red")) + # Set colors for continents
  labs(title = "Fertility Rates Over Time by Continent",
       x = "Date",
       y = "Fertility Rate",
       color = "Continent") +
  theme_minimal() + # Use a minimal theme
  theme(legend.position = "bottom") # Adjust legend position

######### Continue with smoothing

# Setup
range_of_years <- range(final_data$Date)
n_basis <- 4
bspline_basis <- create.bspline.basis(rangeval=range_of_years, nbasis=n_basis, norder=4)

# Initialize a list to store the smoothed functional data objects
smoothed_fds <- list()

# Loop through each country
unique_countries <- unique(final_data$`Country or Area`)
for (country in unique_countries) {
  # Extract data for the current country
  country_data <- filter(final_data, `Country or Area` == country)
  
  # Ensure the data is sorted by Date
  country_data <- country_data[order(country_data$Date), ]
  
  # Create a matrix of the values to be smoothed, with dates as argvals
  value_matrix <- matrix(country_data$ValueLog, ncol = 1)  # One column per country
  argvals <- country_data$Date
  
  # Perform the smoothing
  fd_obj <- smooth.basis(argvals=argvals, y=value_matrix, fdParobj=bspline_basis)
  
  # Store the smoothed functional data object for the country
  smoothed_fds[[country]] <- fd_obj$fd
}

#### Plotting

# Plot the smoothed function for Germany
plot(smoothed_fds[["Germany"]], xlab = "Year", ylab = "Log Fertility Rate", main = "Smoothed Fertility Rate - Germany")

# Step 1: Dynamically evaluate the fd objects at a sequence of points based on each country's data range
evaluated_curves <- list()  # Initialize an empty list to store the evaluated curves

for (country in names(smoothed_fds)) {
  # Determine the specific data range for each country
  country_data <- final_data[final_data$`Country or Area` == country, ]
  years_range <- range(country_data$Date)
  
  # Create a sequence of evaluation points for the current country's data range
  years_to_evaluate_country <- seq(from = years_range[1], to = years_range[2], length.out = 100)
  
  # Evaluate the fd object for the country over its specific range of years
  evaluated_curve <- eval.fd(years_to_evaluate_country, smoothed_fds[[country]])
  
  # Store the evaluated curve in a data frame format in the list
  evaluated_curves[[country]] <- data.frame(Country = country,
                                            Year = years_to_evaluate_country,
                                            LogFertilityRate = evaluated_curve)
}

# Step 2: Combine the evaluated curves for all countries into a single data frame
curves_df <- do.call(rbind, evaluated_curves)

# Add continent information to curves_df
curves_df$Continent <- ifelse(curves_df$Country %in% largest_european_countries, "Europe", "Asia")

# Step 3: Plot using ggplot2
ggplot(curves_df, aes(x = Year, y = LogFertilityRate, color = Continent, group = Country)) +
  geom_line() +
  scale_color_manual(values = c("Europe" = "blue", "Asia" = "red")) +
  labs(title = "Smoothed Fertility Rates Over Time by Continent",
       x = "Year",
       y = "Log Fertility Rate",
       color = "Continent") +
  theme_minimal() +
  theme(legend.position = "bottom")



















