


####### my part

# regression-ready data ----
model_data <- full_data %>%
  drop_na(rides, tmean, prcp, snow, awnd, est_med_income, est_pct_poverty, est_total_pop) %>%
  mutate(
    weekday = lubridate::wday(date, label = TRUE),
    income_10k = est_med_income / 10000,
    poverty_pct = 100 * est_pct_poverty / est_total_pop
  )

## I don't think we should just drop the NAs...but good for quick modeling
## fixed NAs, so this should no longer be needed

# check data
glimpse(model_data)
summary(model_data)

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

# poisson regression ----
pois_model <- glm(
  rides ~ tmean + prcp + snow + awnd + income_10k + poverty_pct + weekday,
  family = poisson(link = "log"),
  data = model_data
)

summary(pois_model)

# easy interpretation table
exp(coef(pois_model))

# overdispersion check
deviance(pois_model) / df.residual(pois_model)


################################ another part
# Negative Binomial model ----
###################################




nb_model <- glm.nb(
  rides ~ tmean + prcp + snow + awnd + income_10k + poverty_pct + weekday,
  data = model_data
)

deviance(nb_model) / df.residual(nb_model)
AIC(pois_model, nb_model)



#######################
#Prediction evaluation (train/test)
######################################


set.seed(123)

train_index <- sample(1:nrow(model_data), 0.8 * nrow(model_data))

train_data <- model_data[train_index, ]
test_data  <- model_data[-train_index, ]



nb_model2 <- glm.nb(
  rides ~ tmean + prcp + snow + awnd + income_10k + poverty_pct + weekday,
  data = train_data
)
test_data$pred_nb <- predict(nb_model2, newdata = test_data, type = "response")
rmse_nb <- sqrt(mean((test_data$rides - test_data$pred_nb)^2))

rmse_nb
ggplot(test_data, aes(x = pred_nb, y = rides)) +
  geom_point(alpha = 0.3) +
  labs(
    title = "Observed vs Predicted Rides (Negative Binomial)",
    x = "Predicted rides",
    y = "Observed rides"
  )
mean(model_data$rides)



pois_model2 <- glm(
  rides ~ tmean + prcp + snow + awnd + income_10k + poverty_pct + weekday,
  family = poisson(link = "log"),
  data = train_data
)

pois_pred <- predict(pois_model2, newdata = test_data, type = "response")
rmse_pois <- sqrt(mean((test_data$rides - pois_pred)^2))

############################
#Compare both model
#############################
rmse_nb
rmse_pois


###################
#obs vs pred
#################
ggplot(test_data, aes(x = pred_nb, y = rides)) +
  geom_point(alpha = 0.3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(
    title = "Observed vs Predicted Rides (Negative Binomial)",
    x = "Predicted rides",
    y = "Observed rides"
  )



