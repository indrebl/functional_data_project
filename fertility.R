library(readxl)
library(tidyverse)
library(fda)
library(dplyr)
library(tidyr)
library(zoo)

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
ggplot(filtered_data, aes(x = Date, y = ValueLog, group = `Country or Area`, color = `Continent`)) +
  geom_line() + # Draw lines for each country
  scale_color_manual(values = c("Europe" = "blue", "Asia" = "red")) + # Set colors for continents
  labs(title = "Fertility Rates Over Time by Continent",
       x = "Date",
       y = "Fertility Rate",
       color = "Continent") +
  theme_minimal() + # Use a minimal theme
  theme(legend.position = "bottom") # Adjust legend position

######### Continue with smoothing

# Get the range of years
range_of_dates <- range(final_data$Date)

# Set up the B-spline basis object
nbasis <-  5  # Can experiment, but this one seems to work the best
basis_obj <- create.bspline.basis(rangeval=range_of_dates, nbasis=nbasis, norder=4)

# Initialize an empty list for storing fd objects for each country
country_fds <- list()

for(country in unique(final_data$`Country or Area`)) {
  country_data <- subset(final_data, `Country or Area` == country)
  
  argvals <- country_data$Date
  y <- country_data$ValueLog
  
  fdParObj <- fdPar(basis_obj, Lfdobj=int2Lfd(2), lambda=1e-6)  # Lambda controls the smoothness
  smooth_res <- smooth.basis(argvals, y, fdParObj)
  
  # Store the fd object for each country
  country_fds[[country]] <- smooth_res$fd
}

# Combine the coefficients from each fd object into a single matrix
coefs <- do.call(cbind, lapply(country_fds, function(fd) fd$coefs))

# Create a new fd object using the combined coefficients and the common basis object
combined_fd <- fd(coefs, basis_obj)

plot(combined_fd)

# Mean and standard deviation

meanlogprec   = mean.fd(combined_fd)
stddevlogprec = std.fd(combined_fd)

lines(meanlogprec, lwd=4, lty=2, col=2)
lines(stddevlogprec, lwd=4, lty=2, col=4)

lines(meanlogprec-stddevlogprec, lwd=4, lty=2, col=6)
lines(meanlogprec+stddevlogprec, lwd=4, lty=2, col=6)

# Boxplot
boxplot(combined_fd)

# PCA

nharm = 4
pcalist = pca.fd(combined_fd, nharm, centerfns = TRUE)
plot(pcalist)
plot(pcalist$harmonics)

plotscores(pcalist, loc = 5)

# Clustering
library(funFEM)

res_w <- funFEM(combined_fd, K=2)

par(mfrow=c(1,2))
plot(combined_fd, col=res_w$cls, lwd=2, lty=1)
fdmeans_w <- combined_fd
fdmeans_w$coefs <- t(res_w$prms$my)
plot(fdmeans_w, col=1:max(res_w$cls), lwd=2)

