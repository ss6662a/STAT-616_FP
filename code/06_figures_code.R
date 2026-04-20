


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

par(bg = "azure3")
plot(total_rides_per_day$date, total_rides_per_day$rides,
     type = "l",
     col = "chocolate3",
     main = "Total Daily Bikeshare Rides (2025)",
     xlab = "Date",
     ylab = "Total Rides",
     xaxt = "n")
axis.Date(1, at = seq(min(total_rides_per_day$date), 
                      max(total_rides_per_day$date), 
                      by = "month"),
          format = "%b")
lines(total_rides_per_day$date,
      predict(loess(rides ~ as.numeric(date), data = total_rides_per_day, span = 0.2)),
      col = "grey20",
      lwd = 2,
      lty = 2)
legend("topleft",
       legend = c("Daily rides", "Trend"),
       col = c("chocolate3", "grey20"),
       lty = c(1, 2),
       lwd = c(1, 2),
       bg = "azure3")


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
model_data$pred_nb <- predict(nb_simp, type = "response")

rmse_nb <- sqrt(mean((model_data$rides - model_data$pred_nb)^2))

rmse_pois
rmse_nb

#compare(rmse_pois vs rmse_nb)
model_compare <- tibble(
  Model = c("Poisson", "Negative Binomial"),
  AIC = c(AIC(pois_model), AIC(nb_simp)),
  Dispersion = c(
    deviance(pois_model) / df.residual(pois_model),
    deviance(nb_simp) / df.residual(nb_simp)
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


pred_grid <- expand.grid(
  tmean = seq(20, 90, by = 1),
  income_10k = c(5, 10, 15),
  prcp = 0,
  snow = 0,
  awnd = mean(model_data$awnd),
  poverty_pct = mean(model_data$poverty_pct),
  weekday = factor("Wed", levels = levels(model_data$weekday), ordered = TRUE)
)

pred_grid$predicted <- predict(nb_simp, newdata = pred_grid, type = "response")

low <- pred_grid[pred_grid$income_10k == 5,]
mid <- pred_grid[pred_grid$income_10k == 10,]
high <- pred_grid[pred_grid$income_10k == 15,]

# plot
par(bg = "azure3", mar = c(5, 5, 4, 2))

plot(low$tmean, low$predicted,
     type = "l",
     col = "chocolate3",
     lwd = 2,
     ylim = c(0, max(pred_grid$predicted) * 1.1),
     main = "Predicted Daily Rides by Temperature and Neighborhood Income",
     xlab = "Mean Temperature (°F)",
     ylab = "Predicted Rides per Station")

lines(mid$tmean,  mid$predicted,  col = "chartreuse3", lwd = 2)
lines(high$tmean, high$predicted, col = "mediumslateblue",  lwd = 2)

legend("topleft",
       legend = c("$50k", "$100k", "$150k"),
       col = c("chocolate3", "chartreuse3", "mediumslateblue"),
       lwd = 2,
       title = "Median Income",
       bg = "azure3")

# ----
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

nb_coef <- broom::tidy(nb_simp, exponentiate = TRUE, conf.int = TRUE) %>%
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














