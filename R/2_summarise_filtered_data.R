# February 1, 2023
# Updated August 18, 2023 - do mg/L trim
# Updated August 29, 2023 - salinity trim

# Imports processed Water Quality observations

# Filters to exclude freshwater stations and other outliers for each variable.
## Freshwater stations: Piper Lake, Hourglass Lake, 0193, Sissiboo

# Applies additional trim to Dissolved Oxygen - mg/L data
# Applies additional trim to Salinity data

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
library(tidyr)
library(readr)

source(here("functions/filter_out_suspect_obs.R"))
source(here("functions/remove_do_correction.R"))
source(here("functions/summarise_grouped_data.R"))


dat_raw <- import_strings_data(input_path = here("data-raw"))

dat_all <- dat_raw %>%
  # remove "Corrected" DO mg/L here and add the uncorrected obs below
  filter(!(VARIABLE == "Dissolved Oxygen" & UNITS == "mg/L")) %>%
  # divide out salinity correction factor from dissolved oxygen (mg/L) observations
  bind_rows(remove_do_correction(dat_raw)) %>%
  # apply additional QC to do concentration and salinity observations
  # filter must be applied AFTER the do corrections
  filter_out_suspect_obs() %>%
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
    !(COUNTY == "Guysborough" & DEPTH == 60 & VARIABLE == "Dissolved Oxygen")
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


#
# x <- dat_all %>%
#   filter(VARIABLE == "Salinity") %>%
#   mutate(YEAR = year(TIMESTAMP)) %>%
#   filter(YEAR == 2021)
#  ggplot(x, aes(TIMESTAMP, VALUE)) + geom_point() + facet_wrap(~STATION)
