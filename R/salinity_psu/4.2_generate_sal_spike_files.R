# November 9, 2023

# This script generates an html file showing all salinity deployments
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

#thresh <- "q_997"

thresh <- "q_999"

# calculate thresholds
quartile_table <- readRDS(here("data/sal_rolling_sd_prelim_qc.rds")) %>%
  select(
    -c(sensor_type, int_sample, n_sample, n_sample_effective,
       sd_roll, rolling_sd_flag_salinity_psu)
  ) %>%
  ss_pivot_longer() %>%
  select(
    COUNTY = county,
    STATION = station,
    DEPLOYMENT_PERIOD = deployment_range,
    TIMESTAMP = timestamp_utc,
    everything()) %>%
  mutate(VARIABLE = "Salinity", UNITS = "PSU") %>%
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

    county_sal = if_else(county == "Inverness", "Inverness", NA_character_)
  ) %>%
  ungroup() %>%
  group_by(county_sal) %>%
  summarise(
    q_90 = round(quantile(spike_value, prob = 0.90, na.rm = TRUE), digits = 3),
    q_95 = round(quantile(spike_value, prob = 0.95, na.rm = TRUE), digits = 3),
    q_997 = round(quantile(spike_value, prob = 0.997, na.rm = TRUE), digits = 3),
    q_999 = round(quantile(spike_value, prob = 0.999, na.rm = TRUE), digits = 3)
  ) %>%
  mutate(variable = "salinity_psu")

# # hard coded thresholds 99.7 quartile of 99.7 quartile
# quartile_table <- data.frame(
#   q_999 = c(0.45, 0.65),
#   q_997_q_50 = c(0.4, 0.65),
#   q_997_q_90 = c(0.65, 2.5),
#   q_997_q_95 = c(0.85, 4.95),
#   q_997_q_997 = c(1.23, 14.3),
#   county_sal = c("Inverness", NA_character_),
#   variable = c("salinity_psu", "salinity_psu")
# )

# export html file for each county showing the flagged observations
sapply(thresh, function(x) {

  rmarkdown::render(
    input = here("R/salinity_psu/4.2_apply_sal_spike_flag.rmd"),
    output_file = here(paste0("output/sal_spike/", x, ".html")),
    params = list(thresh = x, quartiles = quartile_table))

})

