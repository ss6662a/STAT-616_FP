

# Bikeshare Data ----

base_url <- "https://s3.amazonaws.com/capitalbikeshare-data/"

months <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
file_names <- paste0("2025", months, "-capitalbikeshare-tripdata.zip")

raw_dir <- here("STAT-616_FP", "data", "raw_bike") # may need to change bc I have a .Rproj, which might mess with here()
dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE) # need bc git doesn't have that folder

walk(file_names, function(f) {
  url <- paste0(base_url, f)
  dest <- file.path(raw_dir, f)
  download.file(url, dest, mode = "wb")
  unzip(dest, exdir = raw_dir)
}) # downloads csv bikeshare data


bike_raw <- list.files(raw_dir, pattern = "\\.csv$", full.names = TRUE) %>% 
  map_dfr(read_csv, show_col_types = FALSE)


daily_rides <- bike_raw %>% 
  mutate(date = as.Date(started_at)) %>% 
  group_by(start_station_id, start_station_name,
           start_lat, start_lng, date) %>% 
  summarise(rides = n(), .groups = "drop")


# daily_rides should be the file we call later
write_csv(bike_raw, here("STAT-616_FP", "data", "bike_raw.csv"))
write_csv(daily_rides, here("STAT-616_FP", "data", "daily_rides.csv"))










