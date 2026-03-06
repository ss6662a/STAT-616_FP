


# load data ----


bike <- read_csv(here("data", "daily_rides.csv"))
weather <- read_csv(here("data", "weather_clean.csv"))
income <- st_read(here("data", "dc_income.geojson"))



# dealing with missing values ----

colSums(is.na(bike)) # 0 without ebikes
colSums(is.na(dca_weather)) # much better
colSums(is.na(income)) # 5 missing estimates for median income, probably okay to drop

## weather NAs ----

# probably should just take station or 
# want to deal with by averaging temp for that week
# I don't know if we have the data for that tough and the small area of DC makes me lean toward using one good station instead











