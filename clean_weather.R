library(tidyverse)


# read and parse weather data text file in data folder
weather_raw1 <- read_tsv("data/EWR_weather_2025-07-14.tsv",skip = 1) |> 
  mutate(date = as.Date("2025-07-14")) |>
  select(date, everything())

weather_raw2 <- read_tsv("data/EWR_weather_2025-07-15.tsv",skip = 1) |> 
  mutate(date = as.Date("2025-07-15")) |>
  select(date, everything())
# combine both days of weather data
weather_raw <- bind_rows(weather_raw1, weather_raw2) |> 
  mutate(datetime = as.POSIXct(paste(date, Time), format = "%Y-%m-%d %H:%M"),tz = "US/Eastern") %>%
  select(datetime, everything(), -date, -Time)


weather_clean <- weather_raw %>%
  separate_wider_regex(cols = where(~ any(str_detect(.x, "\\d+ (mph|in|°F|\\%)"))),
                       patterns = c(value = "^\\d+(?:\\.\\d+)?", unit = ".*"),
                       names_sep = "_") |> 
  #convert all values to numeric
  mutate(across(ends_with("_value"), as.numeric)) |> 
  # rename columns to remove _value and _unit suffixes
  rename_with(~str_remove(.,"_value")) |> 
  rename_with(tolower)

# save as csv file in data folder
write_csv(weather_clean, "data/EWR_weather_2025-07-14.csv")
