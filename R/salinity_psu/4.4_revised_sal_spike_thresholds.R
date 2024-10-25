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


# calculate thresholds
spike_table <- readRDS(here("data/sal_rolling_sd_prelim_qc.rds")) %>%
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
    q_997 = round(quantile(spike_value, prob = 0.997, na.rm = TRUE), digits = 3)
  ) %>%
  mutate(variable = "salinity_psu") %>%
  rename(spike_low = q_997) %>%
  mutate(spike_high = 3 * spike_low)


# reprocessed data (no preliminary QC)
dat_raw <- readRDS(here("data/sal_rolling_sd_reprocessed.rds")) %>%
  select(
    -c(int_sample, n_sample, n_sample_effective, sd_roll, rolling_sd_flag_salinity_psu)
    ) %>%
  mutate(
    county_sal = if_else(county == "Inverness", "Inverness", NA_character_)
  )

counties <- unique(dat_raw$county)

dat_qc <- NULL

for(i in seq_along(counties)) {

  county_i <- counties[i]

  dat_qc[[i]] <- dat_raw %>%
    filter(county == county_i) %>%
    qc_test_spike(
      spike_table = spike_table, join_column = "county_sal",
      keep_spike_cols = TRUE
    )
}

dat_qc <- dat_qc %>%
  map_df(bind_rows)

dat_spike <- dat_qc %>%
  filter(spike_flag_salinity_psu %in% c("3", "4"))

dat_spike %>%
  plot_histogram(hist_col = spike_value, binwidth = 0.1) +
  facet_wrap(~county_sal, scales = "free")

dat_spike %>%
  filter(is.na(county_sal)) %>%
  plot_histogram(hist_col = spike_value, binwidth = 0.5)

dat_spike %>%
  group_by(county_sal) %>%
  summarise(
    q_50 = round(quantile(spike_value, prob = 0.50, na.rm = TRUE), digits = 3),
    q_90 = round(quantile(spike_value, prob = 0.90, na.rm = TRUE), digits = 3),
    q_95 = round(quantile(spike_value, prob = 0.95, na.rm = TRUE), digits = 3),
    q_997 = round(quantile(spike_value, prob = 0.997, na.rm = TRUE), digits = 3)
  )


dat_spike4 <- dat_qc %>%
  filter(spike_flag_salinity_psu == "4")

dat_spike4 %>%
  group_by(county_sal) %>%
  summarise(
    q_90 = round(quantile(spike_value, prob = 0.90, na.rm = TRUE), digits = 3),
    q_95 = round(quantile(spike_value, prob = 0.95, na.rm = TRUE), digits = 3),
    q_997 = round(quantile(spike_value, prob = 0.997, na.rm = TRUE), digits = 3)
  )

dat_spike4 %>%
  filter(is.na(county_sal)) %>%
  plot_histogram(hist_col = spike_value, binwidth = 0.1)

