# April 24, 2023
# Updated July 25, 2023
# Updated August 30, 2023 to only include dissolved oxygen - percent saturation
# data (because filters for other variables not applied)

# Imports processed Water Quality observations, then filters for
# dissolved oxygen - percent saturation with depths <= 6 m
# Excludes freshwater stations and other outlier stations
## Piper Lake, Hourglass Lake, 0193, Sissiboo

# Exports the mean, standard deviation, and number of observations
# from different groupings
#### All data
#### county
#### county + depth
#### county + month
#### county + month + depth
#### county + month + year
#### month
#### station
#### depth
#### year
#### month + year

# set up ------------------------------------------------------------------
library(dplyr)
library(here)
library(lubridate)
library(sensorstrings)
library(strings)
library(readr)

source(here("functions/summarise_grouped_data.R"))

dat_all <- import_strings_data(input_path = here("data-raw")) %>%
  select(COUNTY, WATERBODY, STATION, TIMESTAMP, DEPTH, VARIABLE, VALUE, UNITS) %>%
  mutate(
    DEPTH = round(as.numeric(DEPTH)),
    MONTH = month(TIMESTAMP),
    YEAR = year(TIMESTAMP)
  ) %>%
  filter(
    (VARIABLE == "Dissolved Oxygen" & UNITS == "percent saturation"),
    DEPTH <= 6, # don't put this above because DEPTH is a factor
    !(STATION %in% c("Piper Lake", "Hourglass Lake", "0193", "Sissiboo"))
  )

# summarize data ----------------------------------------------------------
all <- dat_all %>%
  summarise_grouped_data("all_data", VARIABLE, UNITS)

county <- dat_all %>%
  summarise_grouped_data("county", COUNTY, VARIABLE, UNITS)

county_depth <- dat_all %>%
  summarise_grouped_data("county_depth", COUNTY, DEPTH, VARIABLE, UNITS)

county_month <- dat_all %>%
  summarise_grouped_data("county_month", COUNTY, MONTH, VARIABLE, UNITS)

county_month_year <- dat_all %>%
  summarise_grouped_data("county_month_year", COUNTY, YEAR, MONTH, VARIABLE, UNITS)

county_month_depth <- dat_all %>%
  summarise_grouped_data("county_month_depth", COUNTY, MONTH, DEPTH, VARIABLE, UNITS)


all_month <- dat_all %>%
  summarise_grouped_data("all_month", MONTH, VARIABLE, UNITS)

all_station <- dat_all %>%
  summarise_grouped_data("all_station", COUNTY, STATION, VARIABLE, UNITS)

all_depth <-  dat_all %>%
  summarise_grouped_data("all_depth", DEPTH, VARIABLE, UNITS)

all_month_year <- dat_all %>%
  summarise_grouped_data("all_month_year", YEAR, MONTH, VARIABLE, UNITS)

# export summarized data --------------------------------------------------
dat_out <- bind_rows(
  all, all_month, all_month_year, all_station, all_depth,
  county, county_depth, county_month, county_month_year, county_month_depth
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

write_csv(dat_out, here("data/3_summary_filtered_data_shallow_do_percent_sat.csv"))



