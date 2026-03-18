

# Bikeshare Data ----

base_url <- "https://s3.amazonaws.com/capitalbikeshare-data/"

months <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
file_names <- paste0("2025", months, "-capitalbikeshare-tripdata.zip")

raw_dir <- here("data", "raw_bike") # may need to change bc I have a .Rproj, which might mess with here()
dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE) # need bc git doesn't have that folder

walk(file_names, function(f) {
  url <- paste0(base_url, f)
  dest <- file.path(raw_dir, f)
  download.file(url, dest, mode = "wb")
  unzip(dest, exdir = raw_dir)
}) # downloads csv bikeshare data


bike_raw <- list.files(raw_dir, pattern = "\\.csv$", full.names = TRUE) %>% 
  map_dfr(read_csv, show_col_types = FALSE)

bike_raw %>% 
  filter(is.na(start_station_id)) %>% 
  count(rideable_type)
# all the missing values were ebikes that are not picked up or dropped off at specific loc
# decided it was not important to our analysis

daily_rides <- bike_raw %>% 
  mutate(date = as.Date(started_at)) %>% 
  filter(rideable_type == "classic_bike") %>% # filtering out ebikes
  group_by(start_station_id, 
           start_station_name,
           start_lat, 
           start_lng, 
           date) %>% 
  summarise(rides = n(), .groups = "drop")


# add the days with no rides back to the data set

all_stations <- daily_rides %>% 
  distinct(start_station_id, start_station_name, start_lat, start_lng)

all_dates <- tibble(
  date = seq(as.Date("2025-01-01"), as.Date("2025-12-31"), by = "day")
)

daily_rides2 <- all_stations %>% 
  cross_join(all_dates) %>% 
  left_join(daily_rides,
            by = c("start_station_id", "start_station_name", "start_lat", "start_lng", "date")
            ) %>% 
  mutate(rides = replace_na(rides, 0)) # all dates with no data should be 0


# daily_rides should be the file we call later
write_csv(daily_rides2, here("data", "daily_rides.csv"))
# also saving raw file just in case. It is about a gigabyte though, so avoid saving if you don't need
# write_csv(bike_raw, here("data", "bike_raw.csv"))









