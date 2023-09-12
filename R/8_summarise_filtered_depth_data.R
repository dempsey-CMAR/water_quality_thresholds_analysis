# set up ------------------------------------------------------------------
library(dplyr)
library(here)
library(lubridate)
library(sensorstrings)
library(strings)
library(readr)
library(tidyr)

source(here("functions/summarise_grouped_data.R"))

# all data
dat_all <- list.files(
  here("reprocessed-data-raw"), full.names = TRUE, pattern = ".rds"
) %>%
  purrr::map(readRDS) %>%
  list_rbind()  %>%
  select(
    -c(latitude, longitude, lease),
    -contains("temperature"), -contains("dissolved_oxygen"),
    -contains("salinity")
  ) %>%
  filter(!is.na(sensor_depth_measured_m)) %>%
  ss_pivot_longer() %>%
  rename(VARIABLE = variable, VALUE = value) %>%
  filter(
    !(station == "Olding Island" &
        deployment_range == "2022-Jun-03 to 2022-Sep-30" &
        sensor_depth_at_low_tide_m  == 3),
    # freshwater stations
    !(station %in% c("Piper Lake", "Hourglass Lake", "0193", "Sissiboo")),
    # suspect range
    !(station == "Long Beach" &
        deployment_range == "2020-Jul-16 to 2020-Nov-29" &
        sensor_depth_at_low_tide_m == 5),
    !(station == "Sandy Cove" &
        deployment_range == "2020-Jul-16 to 2020-Nov-30" &
        sensor_depth_at_low_tide_m  == 5),
    !(station == "Tickle Island 1" &
        deployment_range == "2020-Oct-21 to 2021-Aug-25" &
        sensor_depth_at_low_tide_m  == 5)
  )

# summarise data ----------------------------------------------------------
all <- dat_all %>%
  summarise_grouped_data("all_data")

county <- dat_all %>%
  summarise_grouped_data("county", county)

county_depth <- dat_all %>%
  mutate(sensor_depth_at_low_tide_m = round(as.numeric(sensor_depth_at_low_tide_m))) %>%
  summarise_grouped_data("county_depth", county, sensor_depth_at_low_tide_m)

county_month <- dat_all %>%
  mutate(month = month(timestamp_utc)) %>%
  summarise_grouped_data("county_month", county, month)

county_month_year <- dat_all %>%
  mutate(month = month(timestamp_utc), year = year(timestamp_utc)) %>%
  summarise_grouped_data("county_month_year", county, year, month)

all_month <- dat_all %>%
  mutate(month = month(timestamp_utc)) %>%
  summarise_grouped_data("all_month", month)

all_station <- dat_all %>%
  summarise_grouped_data("all_station", county, station)

all_depth <-  dat_all %>%
  mutate(sensor_depth_at_low_tide_m = round(as.numeric(sensor_depth_at_low_tide_m))) %>%
  summarise_grouped_data("all_depth", sensor_depth_at_low_tide_m)

all_month_year <- dat_all %>%
  mutate(month = month(timestamp_utc), year = year(timestamp_utc)) %>%
  summarise_grouped_data("all_month_year", year, month)

# export summarized data --------------------------------------------------
dat_out <- bind_rows(
  all, all_month, all_month_year, all_station, all_depth,
  county, county_depth, county_month, county_month_year
)

write_csv(dat_out, here("data/8_summary_filtered_data_depth.csv"))
