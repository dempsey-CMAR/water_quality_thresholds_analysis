# August 30, 2023

library(dplyr)
library(here)
library(lubridate)
library(purrr)
library(qaqcmar)
library(readr)
library(sensorstrings)
library(strings)
library(tidyr)

# all data
dat_raw <- list.files(
  here("reprocessed-data-raw"), full.names = TRUE, pattern = ".rds"
) %>%
  purrr::map(readRDS) %>%
  list_rbind() %>%
  select(
    -c(latitude, longitude, lease, string_configuration, sensor_type),
    -contains("temperature"), -contains("measured"),
    -contains("dissolved_oxygen")) %>%
  na.omit() %>%   # removes rows without salinity data
  mutate(sensor_depth_at_low_tide_m = round(as.numeric(sensor_depth_at_low_tide_m)))

# calculate rolling standard deviation ------------------------------------

dat_raw_qc <- dat_raw %>%
  qc_test_rolling_sd(keep_sd_cols = TRUE)

# check n_int and n_sample ------------------------------------------------
## for very large int_sample (e.g. data gaps):
### n_sample = (60 / int_sample) * period_hours = 0
## for int_sample = NA (first observation), n_sample_effective is set to 0
## for int_sample > min_int_sample, n_sample_effective is set to 0

n_sample <- dat_raw_qc %>%
  distinct(
    station,
    sensor_serial_number, sensor_depth_at_low_tide_m,
    int_sample, n_sample, n_sample_effective
  ) %>%
  arrange(desc(int_sample))

# when int_sample > 2 hours, N_sample_effective should = 0, and sd_roll = NA
# no large sample intervals for reprocessed data (because biofouling not trimmed)
large_int <- dat_raw_qc %>%
  filter(int_sample > 120) %>%  # 120 mins
  distinct(
    station,
    sensor_serial_number, sensor_depth_at_low_tide_m,
    int_sample, n_sample, n_sample_effective, sd_roll
  ) %>%
  arrange(desc(int_sample))

# check how often int_sample is really large
## mostly around 10 minute intervals, which is what we would expect
## very few large intervals
obs <- dat_raw_qc %>%
  group_by(int_sample) %>%
  summarise(n_obs = n()) %>%
  arrange(desc(int_sample))

# rexport ------------------------------
saveRDS(dat_raw_qc, here("data/sal_rolling_sd_reprocessed.rds"))

