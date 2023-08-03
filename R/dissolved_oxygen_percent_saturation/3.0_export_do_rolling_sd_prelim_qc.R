# July 28, 2023

# preliminary qc:
## Data sent to Open Data Portal (biofouling trimmed by humans)

library(dplyr)
library(here)
library(lubridate)
library(qaqcmar)
library(readr)
library(sensorstrings)
library(strings)
library(tidyr)

# all data - preliminary QC (leave Inverness in)
dat_raw <- import_strings_data(input_path = here("data-raw")) %>%
  filter(
    VARIABLE == "Dissolved Oxygen", UNITS == "percent saturation"
  ) %>%
  mutate(DEPTH = round(as.numeric(DEPTH))) %>%
  ss_reformat_old_data() %>%
  select(-c(waterbody, lease, latitude, longitude, string_configuration))

# calculate rolling standard deviation ------------------------------------
# this takes about 10 -15 mins to run
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

# export ------------------------------
 saveRDS(dat_raw_qc, here("data/do_rolling_sd_prelim_qc.rds"))


