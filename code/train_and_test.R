
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



