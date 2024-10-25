
library(dplyr)
library(here)
library(lubridate)
library(readr)
library(rmarkdown)
library(sensorstrings)
library(stringr)
library(tidyr)
library(qaqcmar)
library(plotly)
library(purrr)

dat <- readRDS(here("data/depth_rolling_sd_reprocessed.rds")) %>%
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
  )


spike_table <- dat %>%
  qc_calculate_spike_thresholds(
    stat = "quartile", prob = 0.997, county, sensor_type,
    var = value_sensor_depth_measured_m)


counties <- unique(dat$county)

dat_qc <- NULL

for(i in seq_along(counties)) {

  county_i <- counties[i]

  spike_table_i <- spike_table %>%
    filter(county == county_i) %>%
    pivot_wider(names_from = "threshold", values_from = "threshold_value")

  dat_qc[[i]] <- dat %>%
    filter(county == county_i) %>%
    qc_test_spike(
      spike_table = spike_table_i,
      join_column = c("county", "sensor_type")
    )
}

dat_qc <- dat_qc %>%
  map_df(bind_rows)

rafuse <- dat_qc %>%
  filter(
    station == "Little Rafuse Island",
    deployment_range == "2021-Nov-21 to 2022-May-24")  %>%
  qc_pivot_longer(qc_tests = "spike") %>%
  ss_convert_depth_to_ordered_factor()


p <- qc_plot_flags(rafuse, qc_tests = "spike")
p$sensor_depth_measured_m$spike
p



