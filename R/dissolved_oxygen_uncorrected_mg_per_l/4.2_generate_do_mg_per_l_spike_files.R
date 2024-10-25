# November 9, 2023

# This script generates an html file showing all dissolved oxygen (concentration)
# observations and spike flags

library(dplyr)
library(here)
library(lubridate)
library(readr)
library(rmarkdown)
library(sensorstrings)
library(stringr)
library(tidyr)

source(here("functions/filter_out_suspect_obs.R"))

thresh <- "q_999"

# calculate thresholds
quartile_table <- readRDS(here("data/do_mg_per_l_rolling_sd_prelim_qc.rds")) %>%
  select(
    -c(sensor_type, int_sample, n_sample, n_sample_effective,
       sd_roll, rolling_sd_flag_dissolved_oxygen_uncorrected_mg_per_l)
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
  group_by(county, station, deployment_range, sensor_serial_number, variable) %>%
  dplyr::arrange(timestamp_utc, .by_group = TRUE) %>%
  mutate(
    lag_value = lag(value),
    lead_value = lead(value),
    spike_ref = (lag_value + lead_value) / 2,
    spike_value = abs(value - spike_ref),
  ) %>%
 ungroup() %>%
  summarise(
    q_90 = round(quantile(spike_value, prob = 0.90, na.rm = TRUE), digits = 3),
    q_95 = round(quantile(spike_value, prob = 0.95, na.rm = TRUE), digits = 3),
    q_997 = round(quantile(spike_value, prob = 0.997, na.rm = TRUE), digits = 3),
    q_999 = round(quantile(spike_value, prob = 0.999, na.rm = TRUE), digits = 3)
  ) %>%
  mutate(variable = "dissolved_oxygen_uncorrected_mg_per_l")


# export html file for each county showing the flagged observations
sapply(thresh, function(x) {

  rmarkdown::render(
    input = here("R/dissolved_oxygen_mg_per_l/4.2_apply_do_mg_per_l_spike_flag.rmd"),
    output_file = here(paste0("output/do_mg_per_l_spike/", x, ".html")),
    params = list(thresh = x, quartiles = quartile_table))

})

