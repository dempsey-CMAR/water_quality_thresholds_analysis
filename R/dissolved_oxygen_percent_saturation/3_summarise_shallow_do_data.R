# Febraury 21, 2023

# Imports processed **Dissolved Oxygen - percent saturation** observations,
# then filters to exclude
# freshwater stations and other outliers
# ("Piper Lake", "Hourglass Lake", "0193", "Sissiboo")
# Filter to only include depth <= 6  m
# Exports the mean, standard deviation, and number of observations
# from different groupings
#### All data
#### county
#### county + month
#### county + month + year
#### month
#### station
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
  filter(
    VARIABLE == "Dissolved Oxygen", UNITS == "percent saturation",
    !(STATION %in% c("Piper Lake", "Hourglass Lake", "0193", "Sissiboo")),
  ) %>%
  mutate(
    DEPTH = round(as.numeric(DEPTH)),
    MONTH = month(TIMESTAMP),
    YEAR = year(TIMESTAMP)
  ) %>%
  filter(
    !(COUNTY == "Inverness" & DEPTH %in% c(8, 18, 28, 36)) &
      !(COUNTY == "Guysborough" & DEPTH == 60),
    DEPTH <= 6
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

write_csv(dat_out, here("data/summary_filtered_shallow_do_data.csv"))



