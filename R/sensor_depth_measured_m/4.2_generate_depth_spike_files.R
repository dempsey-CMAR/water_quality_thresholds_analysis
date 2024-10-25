# November 14, 2023

# This script generates an html file showing all depth deployment
# observations and spike flags

library(dplyr)
library(here)
library(lubridate)
library(readr)
library(rmarkdown)
library(sensorstrings)
library(stringr)
library(tidyr)

thresh <- "q_997"

# calculate thresholds
spike_table <- readRDS(here("data/depth_rolling_sd_reprocessed.rds")) %>%
  select(
    -c(int_sample, n_sample,
       n_sample_effective, sd_roll,
       rolling_sd_flag_sensor_depth_measured_m)) %>%
  rename(sensor_depth_at_low_tide_m = depth_log) %>%
  filter(
    !(station == "Olding Island" &
        deployment_range == "2022-Jun-03 to 2022-Sep-30" &
        sensor_depth_at_low_tide_m == 3),
    # freshwater stations
    !(station %in% c("Piper Lake", "Hourglass Lake", "0193", "Sissiboo")),
    # suspect range
    !(station == "Long Beach" &
        deployment_range == "2020-Jul-16 to 2020-Nov-29" & sensor_depth_at_low_tide_m == 5),
    !(station == "Sandy Cove" &
        deployment_range == "2020-Jul-16 to 2020-Nov-30" & sensor_depth_at_low_tide_m == 5),
    !(station == "Tickle Island 1" &
        deployment_range == "2020-Oct-21 to 2021-Aug-25" & sensor_depth_at_low_tide_m == 5)
  ) %>%
  rename(value = value_sensor_depth_measured_m) %>%
  group_by(county, station, deployment_range, sensor_serial_number) %>%
  dplyr::arrange(timestamp_utc, .by_group = TRUE) %>%
  mutate(
    lag_value = lag(value),
    lead_value = lead(value),
    spike_ref = (lag_value + lead_value) / 2,
    spike_value = abs(value - spike_ref)
  ) %>%
  ungroup() %>%
  group_by(county, sensor_type) %>%
  summarise(
    q_90 = round(quantile(spike_value, prob = 0.90, na.rm = TRUE), digits = 3),
    q_95 = round(quantile(spike_value, prob = 0.95, na.rm = TRUE), digits = 3),
    q_997 = round(quantile(spike_value, prob = 0.997, na.rm = TRUE), digits = 3),
    q_999 = round(quantile(spike_value, prob = 0.999, na.rm = TRUE), digits = 3)
  ) %>%
  mutate(variable = "sensor_depth_measured_m") select(county, variable, sensor_type, all_of(thresh)) %>%
  rename(spike_low = all_of(thresh)) %>%
  mutate(spike_high = 3 * spike_low)
attr(spike_table$spike_low, "names") <- NULL
attr(spike_table$spike_high, "names") <- NULL


# export html file showing the flagged observations
sapply(thresh, function(x) {

  rmarkdown::render(
    input = here("R/sensor_depth_measured_m/4.2_apply_depth_spike_flag.rmd"),
    output_file = here(paste0("output/depth_spike/", x, "_county_sensor.html")),
    params = list(thresh = x, quartiles = quartile_table))

})

