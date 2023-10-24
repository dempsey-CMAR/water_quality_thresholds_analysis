# October 24, 2023

# This script calculates and exports gross range, climatology, and
# rolling sd thresholds for:
## dissolved_oxygen_mg_per_l
## dissolved_oxygen_percent_saturation
## salinity_psu
## sensor_depth_measured_m
## temperature_degree_c

# This script also calculates depth crosscheck thresholds for
## sensor_depth_at_low_tide_m (compared to sensor_depth_measured)

# Thresholds are calculated from historical data
## Datasets sent to the Open Data Portal in December 2022.

# These datasets have been separated by variable:
## do_mg_per_l_rolling_sd_prelim_qc.rds
## do_rolling_sd_prelim_qc.rds
## sal_rolling_sd_prelim_qc.rds.
## depth_rolling_sd_reprocessed.rds (no prelim_qc depth data because it was not extracted before 2022)
## temp_rolling_sd_prelim_qc.rds.

# These data files include the measured observations *and* the associated
# rolling standard deviation.
# These files are used because it takes a long time (15 mins +) to calculate
# the rolling standard deviation.
# These files include *all* observations sent to the Open Data Portal, and must
# be filtered before calculating the thresholds.
## Depth *must* be rounded to apply filter for temperature and
## dissolved oxygen (percent saturation)

# To regenerate the data files, see
## dissolved_oxygen_mg_per_l/3.0_export_do_mg_per_l_rolling_sd_prelim_qc.R
## dissolved_oxygen_percent_saturation/3.0_export_do_rolling_sd_prelim_qc.R
## salinity_psu/3.0_export_sal_rolling_sd_prelim_qc.R
## sensor_depth_measured_m/3.0_export_depth_rolling_sd_reprocessed.R
## temperature_degree_c/3.0_export_temp_rolling_sd_prelim_qc.R

# Dissolved oxygen thresholds are based on pooled data (not by county).
# Temperature thresholds are calculated for each county.

library(data.table)
library(dplyr)
library(here)
library(lubridate)
library(qaqcmar)
library(readr)
library(sensorstrings)
library(tidyr)

# for dissolved oxygen - mg/L and salinity observations
source(here("functions/filter_out_suspect_obs.R"))


# Grossrange sensor thresholds --------------------------------------------
# compiled from sensor manuals

grossrange <- read_csv(
  here("data/grossrange_thresholds.csv"),
  show_col_types = FALSE) %>%
  pivot_longer(
    cols = c(sensor_min, sensor_max),
    names_to = "threshold", values_to = "threshold_value"
  ) %>%
  mutate(county = NA, month = NA, qc_test = "grossrange")

# Dissolved Oxygen - mg/L -----------------------------------
# thresholds based on pooled data (NOT by county)
# salinity factor has already been removed
# to filter data need to convert to old format and then back
dat_do_mg_per_l <- readRDS(here("data/do_mg_per_l_rolling_sd_prelim_qc.rds")) %>%
  select(
    -c(int_sample, n_sample, rolling_sd_flag_dissolved_oxygen_uncorrected_mg_per_l)
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
  ss_pivot_wider() %>%
  mutate(month = month(timestamp_utc))

# climatology thresholds
climatology_do_mg_per_l <- qc_calculate_climatology_thresholds(
  dat_do_mg_per_l, var = dissolved_oxygen_uncorrected_mg_per_l
)

# gross range user thresholds
user_do_mg_per_l <- qc_calculate_user_thresholds(
  dat_do_mg_per_l, var = dissolved_oxygen_uncorrected_mg_per_l
)

# rolling standard deviation thresholds (NAs are removed in the function)
rolling_sd_do_mg_per_l <- qc_calculate_rolling_sd_thresholds(
  dat_do_mg_per_l,
  stat = "quartile",
  var = dissolved_oxygen_uncorrected_mg_per_l, prob = 0.95
)


# Dissolved Oxygen - percent saturation -----------------------------------
# thresholds based on pooled data (NOT by county)

# filtered data
dat_do <- readRDS(here("data/do_rolling_sd_prelim_qc.rds")) %>%
  select(
    -c(int_sample, n_sample, rolling_sd_flag_dissolved_oxygen_percent_saturation)
  ) %>%
  rename(
    dissolved_oxygen_percent_saturation = value_dissolved_oxygen_percent_saturation) %>%
  mutate(month = month(timestamp_utc)) %>%
  filter(
    !(station %in% c("Piper Lake", "Hourglass Lake", "0193", "Sissiboo")),
    !(county == "Inverness" & sensor_depth_at_low_tide_m %in% c(8, 18, 28, 36)),
    !(county == "Guysborough" & sensor_depth_at_low_tide_m == 60)
  )

# climatology thresholds
climatology_do <- qc_calculate_climatology_thresholds(
  dat_do, var = dissolved_oxygen_percent_saturation
)

# gross range user thresholds
user_do <- qc_calculate_user_thresholds(
  dat_do, var = dissolved_oxygen_percent_saturation
)

# rolling standard deviation thresholds
rolling_sd_do <- qc_calculate_rolling_sd_thresholds(
  dat_do, stat = "quartile", prob = 0.95,
  var = dissolved_oxygen_percent_saturation,
)

# Estimated sensor depth --------------------------------------------------
dat_depth <- readRDS(here("data/depth_rolling_sd_reprocessed.rds")) %>%
  select(
    -c(sensor_type, int_sample, n_sample,
       rolling_sd_flag_sensor_depth_measured_m)) %>%
  ss_pivot_longer() %>%
  filter(
    # sensor read outside range
    !(station == "Olding Island" &
        deployment_range == "2022-Jun-03 to 2022-Sep-30" &
        depth_log == 3),
    # freshwater stations
    !(station %in% c("Piper Lake", "Hourglass Lake", "0193", "Sissiboo")),
    # suspect range
    !(station == "Long Beach" &
        deployment_range == "2020-Jul-16 to 2020-Nov-29" & depth_log == 5),
    !(station == "Sandy Cove" &
        deployment_range == "2020-Jul-16 to 2020-Nov-30" & depth_log == 5),
    !(station == "Tickle Island 1" &
        deployment_range == "2020-Oct-21 to 2021-Aug-25" & depth_log == 5)
  )

depth_crosscheck <- dat_depth %>%
  filter(
    !(station == "Madeline Point 1" &
        deployment_range == "2020-Feb-26 to 2020-Oct-20" &
        depth_log == 22)
  ) %>%
  rename(sensor_depth_at_low_tide_m = depth_log) %>%
  qc_calculate_depth_crosscheck_thresholds()


# Measured sensor depth ---------------------------------------------------

climatology_depth <- expand.grid(
  month = 1:12,
  county = NA_character_,
  threshold = c("season_min", "season_max")) %>%
  mutate(
    qc_test = "climatology",
    variable = "sensor_depth_measured_m",
    threshold_value = NA
  )

user_depth <- expand.grid(
  county = NA_character_,
  threshold = c("season_min", "season_max")) %>%
  mutate(
    qc_test = "grossrange",
    variable = "sensor_depth_measured_m",
    threshold_value = NA
  )

rolling_sd_depth <- dat_depth %>%
  group_by(county) %>%
  qc_calculate_rolling_sd_thresholds(
    var = "sensor_depth_measured_m",
    stat = "mean_sd", n_sd = 3
  )

# Salinity (Inverness is separate) ----------------------------------------
dat_sal <- readRDS(here("data/sal_rolling_sd_prelim_qc.rds")) %>%
  select(-c(sensor_type, int_sample, n_sample, rolling_sd_flag_salinity_psu)) %>%
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
  mutate(county_sal = if_else(county == "Inverness", "Inverness", NA_character_))

# climatology thresholds - not enough data to calculate
climatology_sal <- expand.grid(
  month = 1:12,
  county = c("Inverness",  NA_character_),
  threshold = c("season_min", "season_max")) %>%
  mutate(
    qc_test = "climatology",
    variable = "salinity_psu",
    threshold_value = NA
  )

# gross range user thresholds
user_sal <- dat_sal %>%
  group_by(county_sal) %>%
  qc_calculate_user_thresholds(var = salinity_psu) %>%
  ungroup() %>%
  rename(county = county_sal)

# rolling standard deviation thresholds
rolling_sd_sal <- dat_sal %>%
  group_by(county_sal) %>%
  qc_calculate_rolling_sd_thresholds(
    var = salinity_psu, stat = "quartile", prob = 0.95) %>%
  ungroup() %>%
  rename(county = county_sal)

# Sensor Depth (group by county) ------------------------------------------

dat_depth <- readRDS(here("data/depth_rolling_sd_reprocessed.rds")) %>%
  select(-c(sensor_type, int_sample, n_sample,
            rolling_sd_flag_sensor_depth_measured_m)) %>%
  ss_pivot_longer() %>%
  filter(
    # measured outside of gross range
    !(station == "Olding Island" &
        deployment_range == "2022-Jun-03 to 2022-Sep-30" &
        depth_log  == 3),
    # freshwater stations
    !(station %in% c("Piper Lake", "Hourglass Lake", "0193", "Sissiboo")),
    # suspect range
    !(station == "Long Beach" &
        deployment_range == "2020-Jul-16 to 2020-Nov-29" & depth_log == 5),
    !(station == "Sandy Cove" &
        deployment_range == "2020-Jul-16 to 2020-Nov-30" & depth_log == 5),
    !(station == "Tickle Island 1" &
        deployment_range == "2020-Oct-21 to 2021-Aug-25" & depth_log == 5)
    )


# depth depends on where sensor is on string, not historical data
user_depth <- expand.grid(
  county = c(unique(dat_depth$county)),
  threshold = c("user_min", "user_max")) %>%
  mutate(
    qc_test = "grossrange",
    variable = "sensor_depth_measured_m",
    threshold_value = NA
  )

# no monthly climatology cycle
climatology_depth <- expand.grid(
  month = 1:12,
  county = c(unique(dat_depth$county)),
  threshold = c("season_min", "season_max")) %>%
  mutate(
    qc_test = "climatology",
    variable = "sensor_depth_measured_m",
    threshold_value = NA
  )

# rolling standard deviation thresholds
rolling_sd_depth <- dat_depth %>%
  group_by(county) %>%
  qc_calculate_rolling_sd_thresholds(
    var = sensor_depth_measured, stat = "mean_sd", n_sd = 3) %>%
  ungroup()

# Temperature (group by county) -------------------------------------------------------------

dat_temp <- readRDS(here("data/temp_rolling_sd_prelim_qc.rds")) %>%
  select(
    -c(sensor_type, int_sample, n_sample, rolling_sd_flag_temperature_degree_c)
  ) %>%
  mutate(depth = factor(sensor_depth_at_low_tide_m)) %>%
  filter(
    !(station %in% c("Piper Lake", "Hourglass Lake", "0193", "Sissiboo")),
    !(county == "Inverness" & depth %in% c(18, 23, 26, 28, 36, 40))
  )

# climatology thresholds
climatology_temp <- dat_temp %>%
  # grouped by county here
  qc_calculate_climatology_thresholds(var = value_temperature_degree_c, county) %>%
  ungroup()

# gross range user thresholds
user_temp <- dat_temp %>%
  group_by(county) %>%
  qc_calculate_user_thresholds(var = value_temperature_degree_c) %>%
  ungroup()

# rolling standard deviation thresholds
rolling_sd_temp <- dat_temp %>%
  group_by(county) %>%
  qc_calculate_rolling_sd_thresholds(
    var = value_temperature_degree_c, stat = "quartile", prob = 0.997) %>%
  ungroup()


# spike -------------------------------------------------------------------

spike <- data.frame(
  variable = c("temperature_degree_c",
               "dissolved_oxygen_percent_saturation",
               "dissolved_oxygen_mg_per_l",
               "salinity_psu"),
  qc_test= "spike",
  spike_high = 9,
  spike_low = 4
) %>%
  pivot_longer(
    cols = c("spike_high", "spike_low"),
    values_to = "threshold_value",
    names_to = "threshold")


# export ------------------------------------------------------------------

thresholds <- bind_rows(
  grossrange,
  depth_crosscheck,
  climatology_do_mg_per_l, user_do_mg_per_l, rolling_sd_do_mg_per_l,
  climatology_do, user_do, rolling_sd_do,
  climatology_sal, user_sal, rolling_sd_sal,
  climatology_depth, user_depth, rolling_sd_depth,
  climatology_temp, user_temp, rolling_sd_temp,
  spike
) %>%
  select(qc_test, variable, county, month, sensor_type, threshold, threshold_value)


fwrite(thresholds, file = here("output/5_thresholds.csv"), na = "NA")

# export directly to qaqcmar
save(
  thresholds,
  file = "C:/Users/Danielle Dempsey/Desktop/RProjects/Packages/qaqcmar/data/thresholds.rda")

