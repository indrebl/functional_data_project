library(readxl)
library(tidyverse)
library(fda)

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




















