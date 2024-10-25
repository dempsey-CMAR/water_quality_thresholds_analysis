# November 9, 2023

# This script generates an html file showing all dissolved oxygen (% saturation)
# deployments observations and spike flags

library(dplyr)
library(here)
library(lubridate)
library(readr)
library(rmarkdown)
library(sensorstrings)
library(stringr)
library(tidyr)

thresh <- "q_95"

# calculate thresholds
quartile_table <-  readRDS(here("data/do_rolling_sd_prelim_qc.rds"))  %>%
  select(
    -c(sensor_type, int_sample, n_sample, n_sample_effective,
       sd_roll, rolling_sd_flag_dissolved_oxygen_percent_saturation)
  ) %>%
  rename(value = value_dissolved_oxygen_percent_saturation,
         depth = sensor_depth_at_low_tide_m) %>%
  filter(
    !(station %in% c("Piper Lake", "Hourglass Lake", "0193", "Sissiboo")),
    !(county == "Inverness" & depth %in% c(8, 18, 28, 36)),
    !(county == "Guysborough" & depth == 60)
  ) %>%
  group_by(county, station, deployment_range, sensor_serial_number) %>%
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
  mutate(variable = "dissolved_oxygen_percent_saturation")

# export html file for each county showing the flagged observations
sapply(thresh, function(x) {

  rmarkdown::render(
    input = here("R/dissolved_oxygen_percent_saturation/4.2_apply_do_spike_flag.rmd"),
    output_file = here(paste0("output/do_spike_percent_saturation/", x, ".html")),
    params = list(thresh = x, quartiles = quartile_table))

})

