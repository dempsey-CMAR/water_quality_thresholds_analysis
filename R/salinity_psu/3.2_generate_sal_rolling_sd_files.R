# August 31, 2023

# This script generates and html file for each county to highlight deployments
# dissolved oxygen observations flagged by the rolling standard deviation test

library(dplyr)
library(here)
library(lubridate)
library(readr)
library(rmarkdown)
library(sensorstrings)
library(stringr)
library(tidyr)

source(here("functions/filter_out_suspect_obs.R"))

thresh <- "q_997"

# calculate thresholds
quartile_table <- readRDS(here("data/sal_rolling_sd_prelim_qc.rds")) %>%
  select(-c(sensor_type, int_sample, n_sample, rolling_sd_flag_salinity_psu)) %>%
  mutate(
    depth = factor(sensor_depth_at_low_tide_m),
    year = factor(year(timestamp_utc)),
    month = month(timestamp_utc)
  ) %>%
  na.omit() %>%
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
  ss_pivot_wider() %>%
  mutate(
    county_sal_roll_sd = if_else(county == "Inverness", "Inverness", NA_character_)
  ) %>%
  group_by(county_sal_roll_sd) %>%
  summarise(
    q_90 = quantile(sd_roll, prob = 0.90),
    q_95 = quantile(sd_roll, prob = 0.95),
    q_97 = quantile(sd_roll, prob = 0.97),
    q_99 = quantile(sd_roll, prob = 0.99),
    q_997 = quantile(sd_roll, prob = 0.997)
  ) %>%
  mutate(variable = "salinity_psu")

# export html file for each county showing the flagged observations
sapply(thresh, function(x) {

  rmarkdown::render(
    input = here("R/salinity_psu/3.2_apply_sal_rolling_sd_flag.rmd"),
    output_file = here(paste0("output/sal_rolling_sd/", x, ".html")),
    params = list(thresh = x, quartiles = quartile_table))

})

