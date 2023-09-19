# Sept 13, 2023

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
rolling_sd_table <- readRDS(here("data/depth_rolling_sd_reprocessed.rds")) %>%
  select(-c(sensor_type, int_sample, n_sample, rolling_sd_flag_sensor_depth_measured_m)) %>%
  rename(sensor_depth_at_low_tide_m = depth_log) %>%
  ss_pivot_longer() %>%
  filter(
    !(station == "Olding Island" &
        deployment_range == "2022-Jun-03 to 2022-Sep-30" &
        sensor_depth_at_low_tide_m  == 3),
    # freshwater stations
    !(station %in% c("Piper Lake", "Hourglass Lake", "0193", "Sissiboo")),
    # suspect range
    !(station == "Long Beach" &
        deployment_range == "2020-Jul-16 to 2020-Nov-29" &
        sensor_depth_at_low_tide_m == 5),
    !(station == "Sandy Cove" &
        deployment_range == "2020-Jul-16 to 2020-Nov-30" &
        sensor_depth_at_low_tide_m  == 5),
    !(station == "Tickle Island 1" &
        deployment_range == "2020-Oct-21 to 2021-Aug-25" &
        sensor_depth_at_low_tide_m  == 5)
  ) %>%
  group_by(county, variable) %>%
  summarise(
    mean_var = mean(sd_roll, na.rm = TRUE),
    sd_var = sd(sd_roll, na.rm = TRUE),
    #stdev_max = mean_var + 2 * sd_var,
    stdev_max = quantile(sd_roll, probs = 0.95, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  select(variable, county, stdev_max)

# export html file
sapply(thresh, function(x) {

  rmarkdown::render(
    input = here("R/sensor_depth_measured_m/3.2_apply_depth_rolling_sd_flag.rmd"),
    output_file = here(paste0("output/depth_rolling_sd/", x, ".html")),
    params = list(rolling_sd_table = rolling_sd_table))

})

