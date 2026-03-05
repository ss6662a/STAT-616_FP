

# NOAA Weather Query ----


noaa_token = Sys.getenv("NOAA_TOKEN")

pull_noaa_month <- function(year, month, token) {
  start <- sprintf("%04d-%02d-01", year, month)
  end   <- as.character(ceiling_date(as.Date(start), "month") - days(1))
  
  message("Pulling ", start, " to ", end)
  
  request("https://www.ncdc.noaa.gov/cdo-web/api/v2/data") %>% 
    req_headers(token = token) %>% 
    req_url_query(
      datasetid  = "GHCND",
      datatypeid = "TMAX,TMIN,PRCP,SNOW,AWND",
      locationid = "FIPS:11",
      startdate  = start,
      enddate    = end,
      limit      = 1000,
      units      = "standard"
    ) |>
    req_perform() %>% 
    resp_body_json() %>% 
    pluck("results") %>% 
    map_dfr(as_tibble)
}

weather_raw <- map_dfr(1:12, ~pull_noaa_month(2025, .x, noaa_token))

write_csv(weather_raw, here("data", "weather_raw.csv"))

# station lat and long data ----

stations_raw <- request("https://www.ncdc.noaa.gov/cdo-web/api/v2/stations") %>% 
  req_headers(token = noaa_token) %>% 
  req_url_query(
    datasetid  = "GHCND",
    locationid = "FIPS:11",
    startdate  = "2025-01-01",
    enddate    = "2025-12-31",
    limit      = 50
  ) |>
  req_perform() %>% 
  resp_body_json() %>% 
  pluck("results") %>% 
  map_dfr(as_tibble) %>% 
  select(id, name, latitude, longitude) %>% 
  rename(station_id = "id")

write_csv(stations_raw, here("data", "stations_raw.csv"))


# cleaning ----

weather <- weather_raw %>% 
  mutate(
    date = as_date(date)
  ) %>% 
  select(-attributes) %>% 
  rename(station_id = "station") %>%
  pivot_wider(
   names_from = datatype,
   values_from = value
  ) %>% 
  left_join(stations_raw, by = "station_id") %>% 
  janitor::clean_names() %>% 
  mutate(
    tmean = (tmax + tmin) / 2, .after = tmax
  )

write_csv(weather, here("data", "weather_clean.csv"))

