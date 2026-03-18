


####### my part

# regression-ready data ----
model_data <- ready_data %>%
  mutate(
    weekday = wday(date, label = TRUE),
    income_10k = est_med_income / 10000,
    poverty_pct = 100 * est_pct_poverty / est_total_pop
  )

## I don't think we should just drop the NAs...but good for quick modeling
## fixed NAs, so this should no longer be needed

# check data
glimpse(model_data)
summary(model_data)


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


################################ 
# Negative Binomial model ----
###################################


nb_model <- glm.nb(
  rides ~ tmean + prcp + snow + awnd + income_10k + poverty_pct + weekday,
  data = model_data
)

deviance(nb_model) / df.residual(nb_model)
AIC(pois_model, nb_model)


