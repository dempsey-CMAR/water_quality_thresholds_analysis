# July 13, 2023

# Imports processed Water Quality observations, then filters to exclude
# freshwater stations and other outliers ("Piper Lake", "Hourglass Lake",
# "0193", "Sissiboo", several depths at Inverness stations 0814x East, 0814x
# West, Aberdeen, and Deep Basin )

# Additionally removes counties with suspect DO data based on the
# 2.0_do_distribution.html

# Exports the mean, standard deviation, and number of observations
# from different groupings
#
# Excludes several counties of DO data, based on the rate of change analysis
#
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

exclude <- c("Antigonish", "Colchester", "Digby", "Inverness", "Pictou", "Queens")

dat_all <- import_strings_data(input_path = here("data-raw")) %>%
  select(COUNTY, WATERBODY, STATION, TIMESTAMP, DEPTH, VARIABLE, VALUE, UNITS) %>%
  filter(
    !(COUNTY == "Guysborough" & DEPTH == 60 & VARIABLE == "Dissolved Oxygen"),

    !(STATION %in% c("Piper Lake", "Hourglass Lake", "0193", "Sissiboo")),
    !(COUNTY %in% exclude & VARIABLE == "Dissolved Oxygen")
    # !(STATION == "Ram Island" & TIMESTAMP > as_datetime("2021-10-10") &
    #     TIMESTAMP < as_datetime("2021-11-15") & VARIABLE == "Dissolved Oxygen")
  ) %>%
  mutate(
    DEPTH = round(as.numeric(DEPTH)),
    MONTH = month(TIMESTAMP),
    YEAR = year(TIMESTAMP)
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

write_csv(dat_out, here("data/4_summary_filtered_data_additional_do_qc.csv"))



