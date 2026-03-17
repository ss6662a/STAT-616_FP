

# NOAA Weather Query ----


noaa_token = Sys.getenv("NOAA_TOKEN")

## DCA weather data ----

# Reagan station has more complete data
# in an area as small as DC, this may work to show weather in all of DC
# the DC stations had many missing values that would be difficult to deal with properly


pull_noaa_month_dca <- function(year, month, token) {
  start <- sprintf("%04d-%02d-01", year, month)
  end   <- as.character(ceiling_date(as.Date(start), "month") - days(1))
  
  message("Pulling ", start, " to ", end)
  
  request("https://www.ncdc.noaa.gov/cdo-web/api/v2/data") %>% 
    req_headers(token = token) %>% 
    req_url_query(
      datasetid = "GHCND",
      datatypeid = "TMAX,TMIN,PRCP,SNOW,AWND",
      stationid = "GHCND:USW00013743", # Reagan weather station
      startdate = start,
      enddate = end,
      limit = 1000,
      units = "standard"
    ) |>
    req_perform() %>% 
    resp_body_json() %>% 
    pluck("results") %>% 
    map_dfr(as_tibble)
}

dca_raw <- map_dfr(1:12, ~{
  Sys.sleep(2) # hitting rate limits, so added delay
  pull_noaa_month_dca(2025, .x, noaa_token)
  })

## cleaning ----

dca_weather <- dca_raw %>% 
  mutate(
    date = as_date(date)
  ) %>% 
  select(-attributes) %>% 
  pivot_wider(
    names_from = datatype,
    values_from = value
  ) %>% 
  janitor::clean_names() %>% 
  mutate(
    tmean = (tmax + tmin) / 2, .after = tmax
  )

write_csv(dca_weather, here("data", "dca_weather.csv"))








