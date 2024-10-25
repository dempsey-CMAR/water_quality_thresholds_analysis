# January 4, 2024

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


# calculate thresholds
quartile_table <-readRDS(here("data/temp_rolling_sd_prelim_qc.rds")) %>%
  select(
    -c(sensor_type, int_sample, n_sample, rolling_sd_flag_temperature_degree_c)
  ) %>%
  rename(value = value_temperature_degree_c) %>%
  filter(
    !(station %in% c("Piper Lake", "Hourglass Lake", "0193", "Sissiboo")),
    !(county == "Inverness" & sensor_depth_at_low_tide_m %in% c(18, 23, 26, 28, 36, 40))
  ) %>%
  group_by(county, station, deployment_range, sensor_serial_number) %>%
  dplyr::arrange(timestamp_utc, .by_group = TRUE) %>%
  mutate(
    lag_value = lag(value),
    lead_value = lead(value),
    spike_ref = (lag_value + lead_value) / 2,
    spike_value = abs(value - spike_ref)
  ) %>%
  ungroup() %>%
  summarise(
    q_90 = round(quantile(spike_value, prob = 0.90, na.rm = TRUE), digits = 3),
    q_95 = round(quantile(spike_value, prob = 0.95, na.rm = TRUE), digits = 3),
    q_997 = round(quantile(spike_value, prob = 0.997, na.rm = TRUE), digits = 3),
    q_999 = round(quantile(spike_value, prob = 0.999, na.rm = TRUE), digits = 3)
  ) %>%
  select(spike_low = q_999) %>%
  mutate(
    variable = "temperature_degree_c",
    spike_high = 3 * spike_low
  ) %>%
  select(variable, spike_high, spike_low)

counties <- c("Annapolis", "Antigonish", "Cape Breton", "Colchester", "Digby",
              "Guysborough", "Halifax", "Inverness", "Lunenburg", "Pictou",
              "Queens", "Richmond", "Shelburne", "Victoria", "Yarmouth")

# export html file showing the flagged observations
sapply(counties, function(x) {

  rmarkdown::render(
    input = here("R/temperature_degree_c/4.5_temp_spike_vs_rolling_sd.qmd"),
    output_file = here(paste0("output/temp_spike/", x, ".html")),
    params = list(county = x, spike_table = quartile_table))

})

