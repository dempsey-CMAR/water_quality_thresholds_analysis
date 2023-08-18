# February 1, 2023
# Updated August 18, 2023

# Imports processed Water Quality observations, then filters to exclude
# freshwater stations and other outliers
# ("Piper Lake", "Hourglass Lake", "0193", "Sissiboo", several depths at Inverness
# stations 0814x East, 0814x West, Aberdeen, and Deep Basin )

# applied additional trim to Dissolved Oxygen - mg/L data

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

source(here("functions/remove_do_correction.R"))
source(here("functions/summarise_grouped_data.R"))

dat_raw <- import_strings_data(input_path = here("data-raw"))

dat_all <- dat_raw %>%
  # remove "Corrected" DO mg/L here and add the uncorrected obs below
  filter(!(VARIABLE == "Dissolved Oxygen" & UNITS == "mg/L")) %>%
  # divides out salinity correction factor from dissolved oxygen (mg/L) observations
  # does not modify other observations
  bind_rows(remove_do_correction(dat_raw)) %>%
  select(COUNTY, WATERBODY, STATION, TIMESTAMP, DEPTH, VARIABLE, VALUE, UNITS) %>%
  mutate(
    DEPTH = round(as.numeric(DEPTH)),
    MONTH = month(TIMESTAMP),
    YEAR = year(TIMESTAMP)
  ) %>%
  # don't move this up because filter uses rounded depths
  filter(
    # Freshwater stations
    !(STATION %in% c("Piper Lake", "Hourglass Lake", "0193", "Sissiboo")),

    # Temperature
    !(COUNTY == "Inverness" & DEPTH %in% c(18, 23, 26, 28, 36, 40) &
        VARIABLE == "Temperature"),

    # Dissolved Oxygen - % saturation
    !(COUNTY == "Inverness" & DEPTH %in% c(8, 18, 28, 36) &
        VARIABLE == "Dissolved Oxygen"),
    !(COUNTY == "Guysborough" & DEPTH == 60 & VARIABLE == "Dissolved Oxygen"),

    # Dissolved Oxygen - mg/L (extra trim for 2022)
    !(STATION %in% c("Shut-In Island", "Birchy Head", "Upper Blandford") &
        UNITS == "mg/L" &
        TIMESTAMP > as_datetime("2022-08-15")),
    !(STATION == "Flat Island"  &
        UNITS == "mg/L" &
        TIMESTAMP > as_datetime("2022-07-01")),
    !(STATION == "Little Rafuse Island" &
        UNITS == "mg/L" &
        TIMESTAMP > as_datetime("2022-06-10"))


    # !(STATION == "Birchy Head" & UNITS == "mg/L" &
    #     TIMESTAMP > as_datetime("2022-08-15")),
    # !(STATION == "Flat Island"  & UNITS == "mg/L" &
    #     TIMESTAMP > as_datetime("2022-07-01")) |
    #   !(STATION == "Little Rafuse Island" &  UNITS == "mg/L" &
    #       TIMESTAMP > as_datetime("2022-06-10")) |
    #   !(STATION == "Upper Blandford" &  UNITS == "mg/L" &
    #       TIMESTAMP > as_datetime("2022-08-15"))
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

write_csv(dat_out, here("data/2_summary_filtered_data.csv"))



