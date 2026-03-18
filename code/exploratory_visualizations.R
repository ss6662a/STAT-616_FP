

# visualization ----


daily_total <- model_data %>%
  group_by(date) %>%
  summarise(total_rides = sum(rides), .groups = "drop")

ggplot(daily_total, aes(x = date, y = total_rides)) +
  geom_line() +
  labs(
    title = "Total Daily Bikeshare Rides Over Time",
    x = "Date",
    y = "Total rides"
  )


# rides distribution
ggplot(model_data, aes(x = rides)) +
  geom_histogram(bins = 40) + ## 40 bins is too much, no?
  labs(
    title = "Distribution of Daily Rides",
    x = "Rides",
    y = "Count"
  )

# temperature vs rides
ggplot(model_data, aes(x = tmean, y = rides)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "glm", method.args = list(family = "poisson")) +
  labs(
    title = "Temperature vs Rides",
    x = "Mean Temperature",
    y = "Rides"
  )

# rain vs rides
ggplot(model_data, aes(x = prcp, y = rides)) +
  geom_point(alpha = 0.2) +
  labs(
    title = "Rain vs Rides",
    x = "Precipitation",
    y = "Rides"
  )

# income vs rides
ggplot(model_data, aes(x = income_10k, y = rides)) +
  geom_point(alpha = 0.2) +
  labs(
    title = "Income vs Rides",
    x = "Median Income (10,000 dollars)",
    y = "Rides"
  )

# weekday vs rides
ggplot(model_data, aes(x = weekday, y = rides)) +
  geom_boxplot() +
  labs(
    title = "Rides by Weekday",
    x = "Weekday",
    y = "Rides"
  )