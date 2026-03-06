

# getting DC income data ----

api_key <- Sys.getenv("CENSUS_API_KEY")

# https://censusreporter.org for finding variables

income <- get_acs(
  geography = "tract",
  variables = c(
    median_income    = "B19013_001",  # median household income
    pct_poverty      = "B17001_002",  # pop  below poverty line
    total_population = "B01001_001"   # total population
  ),
  state    = "DC",
  year     = 2023, # latest year available
  geometry = TRUE
) %>% 
  st_transform(4326) # EPSG code

# clean ----

income_clean <- income %>% 
  st_drop_geometry() %>% # can't pivot with geometery
  select(GEOID, variable, estimate, moe) %>% # might drop moe later
  pivot_wider(
    names_from = variable,
    values_from = c(estimate, moe)
  ) %>% 
  left_join(
    income %>% select(GEOID, geometry) %>% distinct(GEOID, .keep_all = TRUE),
    by = "GEOID"
  ) %>% 
  st_as_sf()
  


st_write(income_clean, here("data", "dc_income.geojson"))


