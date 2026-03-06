


# load data ----


bike <- read_csv(here("data", "daily_rides.csv"))
weather <- read_csv(here("data", "dca_weather.csv"))
income <- st_read(here("data", "dc_income.geojson"))

head(bike)
head(weather)
head(income)


# dealing with missing values ----

colSums(is.na(bike)) # 0 without ebikes
colSums(is.na(weather)) # much better after switching to dca station
colSums(is.na(income)) # 5 missing estimates for median income, probably okay to drop


# joining data ----

# create an sf object from bike lat long data
bike_stations_sf <- bike %>% 
  distinct(start_station_id, start_station_name, start_lat, start_lng) %>% 
  st_as_sf(coords = c("start_lng", "start_lat"), crs = 4326)

bike_stations_income <- st_join(bike_stations_sf, income) %>% 
  st_drop_geometry() %>% 
  select(start_station_id, GEOID, est_med_income, est_pct_poverty, est_total_pop) %>% 
  group_by(start_station_id) %>% # points on boarders duplicate, 26 times, so just keeping first instance
  slice(1) %>%
  ungroup()


full_data <- bike %>% 
  left_join(bike_stations_income, by = "start_station_id") %>% 
  left_join(weather %>%  select(-station), by = "date")
 
# need to deal with missing values still

colSums(full_data)





