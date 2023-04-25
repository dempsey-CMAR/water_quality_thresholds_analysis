# April 24, 2023
#
# This script exports temperature thresholds for QC tests

#   Based on exploratory analysis of temperature data:
#   - Preliminary QC was applied to the data.
# - Obvious outliers removed.
# - Fresh water and other “outlier” stations not considered.
# - Piper Lake, Hourglass Lake, 0193 (Denas Pond), Sissiboo
#
# - Temperature thresholds were defined bu **county** but NOT depth.

library(dplyr)
library(here)
library(lubridate)
library(readr)
library(tidyr)

# import data -------------------------------------------------------------

dat <- read_csv(
  here("data/summary_filtered_data.csv"), show_col_types = FALSE
) %>%
  filter(variable == "Temperature")

# Gross Range  ------------------------------------------------------------

### Sensor Thresholds
sensor_thresh <- read_csv(
  here("data/sensors.csv"), show_col_types = FALSE
) %>%
  filter(variable == "temperature_degree_c") %>%
  select(variable, sensor_type, sensor_min, sensor_max) %>%
  mutate(county = NA_character_)

### User Thresholds (based on monthly climatology)
user_thresh <- dat %>%
  filter(group == "county_month") %>%
  rename(mean_month = mean) %>%
  group_by(county) %>%
  summarise(
    mean = round(mean(mean_month), digits = 3),
    stdev = round(sd(mean_month), digits = 3)
  ) %>%
  mutate(
    variable = "temperature_degree_c",
    user_min = mean - 3 * stdev,
    user_max = mean + 3 * stdev,
    sensor_type = NA_character_
  ) %>%
  select(variable, sensor_type, county, user_min, user_max)

# gross range
grossrange <- sensor_thresh %>%
  bind_rows(user_thresh) %>%
  pivot_longer(
    cols = c("sensor_min", "sensor_max", "user_min", "user_max"),
    names_to = "threshold", values_to = "threshold_value"
  ) %>%
  filter(!is.na(threshold_value)) %>%
  mutate(qc_test = "grossrange")

# Climatology -------------------------------------------------------------
climatology <- dat %>%
  filter(group == "county_month") %>%
  mutate(
    qc_test = "climatology",
    variable = "temperature_degree_c",
    month = as.numeric(month),
    season_min = round((mean - 3 * stdev), digits = 3),
    season_max = round((mean + 3 * stdev), digits = 3)
  ) %>%
  select(qc_test, variable, county, month, season_min, season_max) %>%
  pivot_longer(
    cols = c("season_min", "season_max"),
    names_to = "threshold", values_to = "threshold_value"
  )


# Combine and export ------------------------------------------------------
threshold_tables <- grossrange %>%
  bind_rows(climatology) %>%
  select(qc_test, variable, county, sensor_type, month, threshold, threshold_value)

write_csv(threshold_tables, here("output/threshold_tables.csv"))

write_csv(
  threshold_tables,
  "C:/Users/Danielle Dempsey/Desktop/RProjects/Packages/qaqcmar/data-raw/threshold_tables.csv"
)

