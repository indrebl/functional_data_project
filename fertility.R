library(readxl)
library(tidyverse)
library(fda)
library(dplyr)
library(tidyr)
library(zoo)

data <- read_excel("C:/Users/Indre/Downloads/undesa_pd_2019_world_fertility_dataset.xlsx",
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
final_data <- final_data %>%
  mutate(ValueLog = log(Value))

View(final_data)

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

# Get the range of years
range_of_dates <- range(final_data$Date)

# Set up the B-spline basis object
nbasis <- 5  # Can experiment, but this one seems to work the best
basis_obj <- create.bspline.basis(rangeval=range_of_dates, nbasis=nbasis, norder=4)

lambda_v =1e2 # Lambda controls the smoothness

# Initialize an empty list for storing fd objects for each country
country_fds <- list()

# Initialize empty lists for storing smoothed fd objects for European and Asian countries
europe_fds <- list()
asia_fds <- list()

# Loop through unique countries and smooth the data
for(country in unique(final_data$`Country or Area`)) {
  country_data <- subset(final_data, `Country or Area` == country)
  argvals <- country_data$Date
  y <- country_data$ValueLog

  # Smooth the data
  fdParObj <- fdPar(basis_obj, Lfdobj=int2Lfd(2), lambda= lambda_v)
  smooth_res <- smooth.basis(argvals, y, fdParObj)

  # Store the fd object based on continent
  if (country %in% largest_european_countries) {
    europe_fds[[country]] <- smooth_res$fd
  } else {
    asia_fds[[country]] <- smooth_res$fd
  }
}

# Combine the coefficients from each fd object into a single matrix for Europe
europe_coefs <- do.call(cbind, lapply(europe_fds, function(fd) fd$coefs))
europe_basis_obj <- create.bspline.basis(rangeval=range_of_dates, nbasis=nbasis, norder=4)
europe_fd <- fd(europe_coefs, europe_basis_obj)

# Combine the coefficients from each fd object into a single matrix for Asia
asia_coefs <- do.call(cbind, lapply(asia_fds, function(fd) fd$coefs))
asia_basis_obj <- create.bspline.basis(rangeval=range_of_dates, nbasis=nbasis, norder=4)
asia_fd <- fd(asia_coefs, asia_basis_obj)

# Combine the coefficients from European and Asian fd objects
combined_coefs <- cbind(europe_fd$coefs, asia_fd$coefs)

# Use the same basis object for the combined fd object
combined_fd <- fd(combined_coefs, basisobj = europe_fd$basisobj)

# Plot the combined functional data object
plot(combined_fd)

# Mean and standard deviation

meanlogprec   = mean.fd(combined_fd)
stddevlogprec = std.fd(combined_fd)

lines(meanlogprec, lwd=4, lty=2, col=2)
lines(stddevlogprec, lwd=4, lty=2, col=4)

lines(meanlogprec-stddevlogprec, lwd=4, lty=2, col=6)
lines(meanlogprec+stddevlogprec, lwd=4, lty=2, col=6)


# Calculate the covariance matrix for Asian country data
covariance_matrix_asia <- cov(asia_coefs)

# Plot the covariance surface
persp(covariance_matrix_asia,  theta=-45, phi=25, r=3, expand = 0.5,
      ticktype='detailed',
      xlab = "Time", ylab = "Time", zlab = "Covariance",
      main = "Covariance for Asian Country Data")

# Plot the covariance surface in 2d for Asian country data
contour(covariance_matrix_asia,
        ticktype = 'detailed',
        xlab = "Time", ylab = "Time",
        main = "Covariance for European Country Data")


# Calculate the covariance matrix for European country data
covariance_matrix_europe <- cov(europe_coefs)

# Plot the covariance surface
persp(covariance_matrix_europe, theta = -45, phi = 25, r = 3, expand = 0.5,
      ticktype = 'detailed',
      xlab = "Time", ylab = "Time", zlab = "Covariance",
      main = "Covariance for European Country Data")


# Plot the covariance surface in 2d for European country data
contour(covariance_matrix_europe,
        xlab = "Time", ylab = "Time",
        main = "Covariance for European Country Data")




# Boxplot
boxplot(combined_fd)
boxplot(asia_fd)
boxplot(europe_fd)

# PCA

nharm = 4
pcalist = pca.fd(combined_fd, nharm, centerfns = TRUE)
plot(pcalist)
plot(pcalist$harmonics)

plotscores(pcalist, loc = 5)


#### Rotation
varmx <- varmx.pca.fd(pcalist)
plot(varmx)

plot(varmx$harmonics)

plotscores(varmx, loc = 5)

#BY CONTINENT
# PCA asia

nharm = 4
pcalist = pca.fd(asia_fd, nharm, centerfns = TRUE)
plot(pcalist)
plot(pcalist$harmonics)

plotscores(pcalist, loc = 5)


#### Rotation asia
varmx_as <- varmx.pca.fd(pcalist)
plot(varmx_as)

plot(varmx_as$harmonics)

plotscores(varmx_as, loc = 5)

nharm = 4
pcalist = pca.fd(asia_fd, nharm, centerfns = TRUE)
plot(pcalist)
plot(pcalist$harmonics)

plotscores(pcalist, loc = 5)


# PCA europe

nharm = 4
pcalist = pca.fd(europe_fd, nharm, centerfns = TRUE)
plot(pcalist)
plot(pcalist$harmonics)

plotscores(pcalist, loc = 5)


#### Rotation europe
varmx_eu <- varmx.pca.fd(pcalist)
plot(varmx_eu)

plot(varmx_eu$harmonics)

plotscores(varmx_eu, loc = 5)

nharm = 4
pcalist = pca.fd(europe_fd, nharm, centerfns = TRUE)
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

