# July 28, 2023

# preliminary qc:
## Data sent to Open Data Portal

library(dplyr)
library(here)
library(lubridate)
library(qaqcmar)
library(readr)
library(sensorstrings)
library(strings)
library(tidyr)

# Guysborough
dat_raw1 <- import_strings_data(
  input_path = here("data-raw"),
  county = "Guysborough"
) %>%
  filter(VARIABLE == "Temperature") %>%
  mutate(DEPTH = round(as.numeric(DEPTH))) %>%
  ss_reformat_old_data() %>%
  select(-c(waterbody, lease, latitude, longitude, string_configuration))

# Lunenburg
dat_raw2 <- import_strings_data(
  input_path = here("data-raw"),
  county = "Lunenburg"
) %>%
  filter(VARIABLE == "Temperature") %>%
  mutate(DEPTH = round(as.numeric(DEPTH))) %>%
  ss_reformat_old_data() %>%
  select(-c(waterbody, lease, latitude, longitude, string_configuration))

# remaining counties
counties <- c("Annapolis", "Antigonish", "Colchester",
              "Digby", "Inverness",
              "Halifax", "Pictou", "Queens",
              "Richmond", "Shelburne", "Victoria", "Yarmouth")

dat_raw3 <- import_strings_data(
    input_path = here("data-raw"),
    county = counties
  ) %>%
  filter(VARIABLE == "Temperature") %>%
  mutate(DEPTH = round(as.numeric(DEPTH))) %>%
  ss_reformat_old_data() %>%
  select(-c(waterbody, lease, latitude, longitude, string_configuration))


# calculate rolling standard deviation ------------------------------------
# this takes a while to run
dat_raw_qc1 <- dat_raw1 %>%
  qc_test_rolling_sd(keep_sd_cols = TRUE)

dat_raw_qc2 <- dat_raw2 %>%
  qc_test_rolling_sd(keep_sd_cols = TRUE)

dat_raw_qc3 <- dat_raw3 %>%
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
saveRDS(dat_raw_qc, here("data/temp_rolling_sd_prelim_qc.rds"))


