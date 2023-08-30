# August 30, 2023

# This script calculates and exports gross range, climatology, and
# rolling sd thresholds for:
## dissolved_oxygen_mg_per_l
## dissolved_oxygen_percent_saturation
## temperature_degree_c

# Thresholds are calculated from historical data
## Datasets sent to the Open Data Portal in December 2022.

# These datasets have been separated by variable:
## do_mg_per_l_rolling_sd_prelim_qc.rds
## do_rolling_sd_prelim_qc.rds
## temp_rolling_sd_prelim_qc.rds.

# These data files include the measured observations *and* the associated
# rolling standard deviation.
# These files are used because it takes a long time (15 mins +) to calculate
# the rolling standard deviation.
# These files include *all* observations sent to the Open Data Portal, and must
# be filtered before calculating the thresholds.
## Depth *must* be rounded to apply filter for temperature and
## dissolved oxygen (percent saturation)

# To regenerate the data files, see
## dissolved_oxygen_mg_per_l/3.0_export_do_mg_per_l_rolling_sd_prelim_qc.R
## dissolved_oxygen_percent_saturation/3.0_export_do_rolling_sd_prelim_qc.R
## temperature_degree_c/3.0_export_temp_rolling_sd_prelim_qc.R

# Dissolved oxygen thresholds are based on pooled data (not by county).
# Temperature thresholds are calculated for each county.

library(data.table)
library(dplyr)
library(here)
library(lubridate)
library(readr)
library(sensorstrings)
library(qaqcmar)

# for dissolved oxygen - mg/L and salinity observations
source(here("functions/filter_out_suspect_obs.R"))

# Dissolved Oxygen - mg/L -----------------------------------
# thresholds based on pooled data (NOT by county)
# salinity factor has already been removed
# to filter data need to convert to old format and then back
dat_do_mg_per_l <- readRDS(here("data/do_mg_per_l_rolling_sd_prelim_qc.rds")) %>%
  select(
    -c(int_sample, n_sample, rolling_sd_flag_dissolved_oxygen_uncorrected_mg_per_l)
  ) %>%
  ss_pivot_longer() %>%
  select(
    COUNTY = county,
    STATION = station,
    DEPLOYMENT_PERIOD = deployment_range,
    TIMESTAMP = timestamp_utc,
    everything()) %>%
  mutate(VARIABLE = "Dissolved Oxygen", UNITS = "mg/L") %>%
  filter_out_suspect_obs() %>%
  select(-c(VARIABLE, UNITS)) %>%
  rename(
    county = COUNTY,
    station = STATION,
    deployment_range = DEPLOYMENT_PERIOD,
    timestamp_utc = TIMESTAMP
  ) %>%
  ss_pivot_wider() %>%
  mutate(month = month(timestamp_utc))

# climatology thresholds
climatology_do_mg_per_l <- qc_calculate_climatology_thresholds(
  dat_do_mg_per_l, var = value_dissolved_oxygen_uncorrected_mg_per_l
)

# gross range user thresholds
user_do_mg_per_l <- qc_calculate_user_thresholds(
  dat_do_mg_per_l, var = value_dissolved_oxygen_uncorrected_mg_per_l
)

# rolling standard deviation thresholds (NAs are removed in the function)
rolling_sd_do_mg_per_l <- qc_calculate_rolling_sd_thresholds(
  dat_do_mg_per_l,
  var = value_dissolved_oxygen_uncorrected_mg_per_l, prob = 0.95
)


# Dissolved Oxygen - percent saturation -----------------------------------
# thresholds based on pooled data (NOT by county)

# filtered data
dat_do <- readRDS(here("data/do_rolling_sd_prelim_qc.rds")) %>%
  select(
    -c(int_sample, n_sample, rolling_sd_flag_dissolved_oxygen_percent_saturation)
  ) %>%
  mutate(month = month(timestamp_utc)) %>%
  filter(
    !(station %in% c("Piper Lake", "Hourglass Lake", "0193", "Sissiboo")),
    !(county == "Inverness" & sensor_depth_at_low_tide_m %in% c(8, 18, 28, 36)),
    !(county == "Guysborough" & sensor_depth_at_low_tide_m == 60)
  )

# climatology thresholds
climatology_do <- qc_calculate_climatology_thresholds(
  dat_do, var = value_dissolved_oxygen_percent_saturation
)

# gross range user thresholds
user_do <- qc_calculate_user_thresholds(
  dat_do, var = value_dissolved_oxygen_percent_saturation
)

# rolling standard deviation thresholds
rolling_sd_do <- qc_calculate_rolling_sd_thresholds(
  dat_do, var = value_dissolved_oxygen_percent_saturation, prob = 0.95
)

# temperature (group by county) -------------------------------------------------------------

dat_temp <- readRDS(here("data/temp_rolling_sd_prelim_qc.rds")) %>%
  select(
    -c(sensor_type, int_sample, n_sample, rolling_sd_flag_temperature_degree_c)
  ) %>%
  mutate(depth = factor(sensor_depth_at_low_tide_m)) %>%
  filter(
    !(station %in% c("Piper Lake", "Hourglass Lake", "0193", "Sissiboo")),
    !(county == "Inverness" & depth %in% c(18, 23, 26, 28, 36, 40))
  )

# climatology thresholds
climatology_temp <- dat_temp %>%
  qc_calculate_climatology_thresholds(
    # grouped by county here
    var = value_temperature_degree_c, county) %>%
  ungroup()

# gross range user thresholds
user_temp <- dat_temp %>%
  group_by(county) %>%
  qc_calculate_user_thresholds(var = value_temperature_degree_c) %>%
  ungroup()

# rolling standard deviation thresholds
rolling_sd_temp <- dat_temp %>%
  group_by(county) %>%
  qc_calculate_rolling_sd_thresholds(
    var = value_temperature_degree_c, prob = 0.997) %>%
  ungroup()

# export ------------------------------------------------------------------

thresholds_out <- bind_rows(
  climatology_do_mg_per_l, user_do_mg_per_l, rolling_sd_do_mg_per_l,
  climatology_do, user_do, rolling_sd_do,
  climatology_temp, user_temp, rolling_sd_temp
) %>%
  select(qc_test, variable, county, month, threshold, threshold_value)

fwrite(thresholds_out, file = here("output/5_thresholds.csv"))












