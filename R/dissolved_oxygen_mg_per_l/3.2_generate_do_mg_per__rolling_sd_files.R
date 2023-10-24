# August 21, 2023
# September 27 - modified to calculate the thresholds in this script
## (instead of copying from another file)

# This script generates an html file showing each dissolved oxygen - concentration
# deployment for the reprocessed data.
# Observations that fail the rolling standard deviation test are flagged

library(dplyr)
library(here)
library(rmarkdown)
library(stringr)
library(tidyr)

source(here("functions/filter_out_suspect_obs.R"))

# data - preliminary QC (to calculate thresholds)
thresh <- readRDS(here("data/do_mg_per_l_rolling_sd_prelim_qc.rds")) %>%
  select(
    -c(sensor_type, int_sample, n_sample,
       rolling_sd_flag_dissolved_oxygen_uncorrected_mg_per_l)) %>%
  mutate(
    depth = factor(sensor_depth_at_low_tide_m),
    year = factor(year(timestamp_utc)),
    month = month(timestamp_utc)
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
  )  %>%
  summarise(
    q_90 = quantile(sd_roll, probs = 0.90, na.rm = TRUE),
    q_95 = quantile(sd_roll, probs = 0.95, na.rm = TRUE),
    q_997 = quantile(sd_roll, probs = 0.997, na.rm = TRUE)
  )

# export html file for each county showing the flagged observations
sapply(thresh, function(x) {

  rmarkdown::render(
    input = here("R/dissolved_oxygen_mg_per_l/3.2_apply_do_mg_per_l_rolling_sd_flag.rmd"),
    output_file = here(paste0("output/do_mg_per_l_rolling_sd/", x, ".html")),
    params = list(thresh = x))

})

