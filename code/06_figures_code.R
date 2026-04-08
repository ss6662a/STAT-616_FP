


# histogram of daily rides ----

par(mfrow = c(1, 1), bg = "azure3")
hist(model_data$rides, breaks = 40,
     col = "chocolate3",
     main = "Histogram of the Number of Rides per Day", col.main = "grey0",
     xlab = "Rides",
     ylab = "Frequency",
     border = NA)


# time series of total daily rides ----

total_rides_per_day <- aggregate(rides ~ date, data = model_data, FUN = sum)

par(mfrow = c(1, 1), bg = "azure3")
plot(total_rides_per_day$rides, type = "l")
lines(loess(total_rides_per_day$rides ~ total_rides_per_day$date))


# scatterplot of rides vs temperature ----



# table comparing Poisson to NB (AIC, dispersion, RMSE) ----



# residuals vs fitted for NB model ----



# interaction plot ----


# coefficient plot showing IRRs and CIs ----


# weekday effect plot ----



# most popular stations















