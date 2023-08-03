
# use rolling_sd data so don't have to re-calculate rolling sd (takes a while)
# should  be able to calculate from the rolling_sd files (preliminary qc)
# NOT already filtered
# be careful applying filters - depth must be rounded!

library(dplyr)
library(lubridate)
library(sensorstrings)

# Dissolved Oxygen - percent saturation -----------------------------------
# thresholds based on pooled data (NOT by county)

# filtered data
dat_do <- readRDS(here("data/do_rolling_sd_prelim_qc.rds")) %>%
  select(
    -c(int_sample, n_sample, rolling_sd_flag_dissolved_oxygen_percent_saturation)
  ) %>%
  mutate(month = month(timestamp_utc)) %>%
  filter(
    !(station %in% c("Piper Lake", "Hourglass Lake", "0193", "Sissiboo")),
    !(county == "Inverness" & sensor_depth_at_low_tide_m %in% c(8, 18, 28, 36)),
    !(county == "Guysborough" & sensor_depth_at_low_tide_m == 60)
  )

# user thresholds
user_do <- qc_calculate_user_thresholds(
  dat_do, var = value_dissolved_oxygen_percent_saturation
)

# climatology thresholds
climatology_do <- qc_calculate_climatology_thresholds(
  dat_do, var = value_dissolved_oxygen_percent_saturation
)

# rolling standard deviation thresholds
rolling_sd_do <- qc_calculate_rolling_sd_thresholds(
  dat_do, var = value_dissolved_oxygen_percent_saturation, prob = 0.95
)



















