


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

ggplot(model_data, aes(x = tmean, y = rides)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "glm", method.args = list(family = "poisson")) +
  labs(
    title = "Daily Rides vs Temperature",
    x = "Mean temperature",
    y = "Daily rides"
  )

# table comparing Poisson to NB (AIC, dispersion, RMSE) ----

# Poisson predictions
model_data$pred_pois <- predict(pois_model, type = "response")

rmse_pois <- sqrt(mean((model_data$rides - model_data$pred_pois)^2))

# Negative Binomial predictions
model_data$pred_nb <- predict(nb_model, type = "response")

rmse_nb <- sqrt(mean((model_data$rides - model_data$pred_nb)^2))

rmse_pois
rmse_nb

#compare(rmse_pois vs rmse_nb)
model_compare <- tibble(
  Model = c("Poisson", "Negative Binomial"),
  AIC = c(AIC(pois_model), AIC(nb_model)),
  Dispersion = c(
    deviance(pois_model) / df.residual(pois_model),
    deviance(nb_model) / df.residual(nb_model)
  ),
  RMSE = c(rmse_pois, rmse_nb)
)
model_compare



# residuals vs fitted for NB model ----

model_data$nb_fitted <- fitted(nb_model)
model_data$nb_resid <- residuals(nb_model, type = "pearson")

ggplot(model_data, aes(x = nb_fitted, y = nb_resid)) +
  geom_point(alpha = 0.25) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Residuals vs Fitted Values (Negative Binomial)",
    x = "Fitted values",
    y = "Pearson residuals"
  )



# interaction plot ----

ggplot(model_data, aes(x = tmean, y = rides, color = weekday)) +
  geom_point(alpha = 0.15) +
  geom_smooth(method = "glm", method.args = list(family = "poisson"), se = FALSE) +
  labs(
    title = "Temperature and Weekday Interaction",
    x = "Mean temperature",
    y = "Daily rides",
    color = "Weekday"
  )


# coefficient plot showing IRRs and CIs ----

nb_coef <- broom::tidy(nb_model, exponentiate = TRUE, conf.int = TRUE) %>%
  filter(term != "(Intercept)")

ggplot(nb_coef, aes(x = reorder(term, estimate), y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  coord_flip() +
  labs(
    title = "Incidence Rate Ratios from Negative Binomial Model",
    x = "Predictor",
    y = "IRR"
  )


# weekday effect plot ----

weekday_summary <- model_data %>%
  group_by(weekday) %>%
  summarise(
    avg_rides = mean(rides),
    .groups = "drop"
  )

ggplot(weekday_summary, aes(x = weekday, y = avg_rides)) +
  geom_col() +
  labs(
    title = "Average Daily Rides by Weekday",
    x = "Weekday",
    y = "Average daily rides"
  )



# most popular stations

top_stations <- model_data %>%
  group_by(start_station_name) %>%
  summarise(
    avg_rides = mean(rides),
    total_rides = sum(rides),
    .groups = "drop"
  ) %>%
  arrange(desc(total_rides)) %>%
  slice_head(n = 10)

top_stations

ggplot(top_stations, aes(x = reorder(start_station_name, total_rides), y = total_rides)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Most Popular Bikeshare Stations",
    x = "Station",
    y = "Total rides"
  )














