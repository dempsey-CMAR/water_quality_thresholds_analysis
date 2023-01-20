
library(dplyr)
library(here)
library(lubridate)
library(sensorstrings)
library(strings)
library(readr)

source(here("functions/summarise_grouped_data.R"))

dat_all <- import_strings_data(input_path = here("data-raw")) %>%
  select(COUNTY, WATERBODY, STATION, TIMESTAMP, DEPTH, VARIABLE, VALUE, UNITS)

all <- dat_all %>%
  summarise_grouped_data("all_data", VARIABLE, UNITS)

county <- dat_all %>%
  summarise_grouped_data("county", COUNTY, VARIABLE, UNITS)

county_depth <- dat_all %>%
  mutate(DEPTH = round(as.numeric(DEPTH))) %>%
  summarise_grouped_data("county_depth", COUNTY, DEPTH, VARIABLE, UNITS)

county_month <- dat_all %>%
  mutate(MONTH = month(TIMESTAMP)) %>%
  summarise_grouped_data("county_month", COUNTY, MONTH, VARIABLE, UNITS)

county_month_year <- dat_all %>%
  mutate(MONTH = month(TIMESTAMP), YEAR = year(TIMESTAMP)) %>%
  summarise_grouped_data("county_month_year", COUNTY, YEAR, MONTH, VARIABLE, UNITS)

all_month <- dat_all %>%
  mutate(MONTH = month(TIMESTAMP)) %>%
  summarise_grouped_data("all_month", MONTH, VARIABLE, UNITS)

all_station <- dat_all %>%
  summarise_grouped_data("all_station", COUNTY, STATION, VARIABLE, UNITS)

all_depth <-  dat_all %>%
  mutate(DEPTH = round(as.numeric(DEPTH))) %>%
  summarise_grouped_data("all_depth", DEPTH, VARIABLE, UNITS)

all_month_year <- dat_all %>%
  mutate(MONTH = month(TIMESTAMP), YEAR = year(TIMESTAMP)) %>%
  summarise_grouped_data("all_month_year", YEAR, MONTH, VARIABLE, UNITS)



dat_out <- bind_rows(
  all, all_month, all_month_year, all_station, all_depth,
  county, county_depth, county_month, county_month_year
) %>%
  rename(
    variable = VARIABLE,
    units = UNITS,
    county = COUNTY,
    station = STATION,
    depth = DEPTH,
    year = YEAR,
    month = MONTH
  )

write_csv(dat_out, here("data/summary.csv"))


# write_csv(dat_summary, here("data/all_summary.csv"))
#
# write_csv(dat_county, here("data/county_summary.csv"))
#
# write_csv(dat_county_depth, here("data/county_depth_summary.csv"))
#
# write_csv(dat_county_month, here("data/county_month_summary.csv"))
#
# write_csv(dat_all_month, here("data/all_month_summary.csv"))







