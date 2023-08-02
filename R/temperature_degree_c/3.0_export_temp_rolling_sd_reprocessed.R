# August 2, 2023

# reprocessed data

library(dplyr)
library(here)
library(lubridate)
library(purrr)
library(qaqcmar)
library(readr)
library(sensorstrings)
library(strings)
library(tidyr)


dat_raw <- list.files(
  here("reprocessed-data-raw"), full.names = TRUE, pattern = ".rds"
) %>%
  purrr::map(readRDS) %>%
  list_rbind() %>%
  select(
    -c(latitude, longitude, lease, string_configuration, sensor_type),
    -contains("dissolved_oxygen"), -contains("salinity"), -contains("measured")
  ) %>%
  na.omit() %>%   # removes rows without temp data
  mutate(sensor_depth_at_low_tide_m = round(as.numeric(sensor_depth_at_low_tide_m)))


# calculate rolling standard deviation ------------------------------------
# this takes a while to run
dat_raw_qc1 <- dat_raw %>%
  filter(county == "Guysborough") %>%
  qc_test_rolling_sd(keep_sd_cols = TRUE)

dat_raw_qc2 <- dat_raw %>%
  filter(county == "Lunenburg") %>%
  qc_test_rolling_sd(keep_sd_cols = TRUE)

# remaining counties
dat_raw_qc3 <- dat_raw %>%
  filter((!county %in% c("Guysborough", "Lunenburg"))) %>%
  qc_test_rolling_sd(keep_sd_cols = TRUE)

#rm(dat_raw1, dat_raw2, dat_raw3)

dat_raw_qc <- bind_rows(dat_raw_qc1, dat_raw_qc2, dat_raw_qc3)

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
# there are some large intervals for vemco data
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
saveRDS(dat_raw_qc, here("data/temp_rolling_sd_reprocessed.rds"))


