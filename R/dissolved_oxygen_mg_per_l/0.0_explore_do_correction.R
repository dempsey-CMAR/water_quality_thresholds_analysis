# August 17, 2023

# The DO concentration data sent to the Open Data Portal on December 2022
# was corrected for Salinity using strings::do_salinity_correction()
# DO_corrected = DO_raw * F_s, where F_s is a salinity correction factor between
# 0 and 1

# The newly processed data will NOT include a salinity correction factor.
# So to calculate thresholds based on historical data, we need to remove F_s.
# DO_raw = DO_corrected / F_s

# This script re-calculates F_s to calculate DO_raw
# Results were spot-checked to verify
# Code was turned into a function for easier re-use

# Discovered some serial number discrepancies - see z
# Many serial numbers in released data are likely wrong (but depths are correct)

library(dplyr)
library(here)
library(stringr)
library(strings)
library(tidyr)

sal <- 30.6 # salinity value used to calculate salinity correction factor in old compile scripts

# read in all data
dat_all <- import_strings_data(input_path = here("data-raw"))

# pivot wider so corresponding temperature and DO measurements are in the same row
dat <- dat_all %>%
  filter(UNITS %in% c("mg/L", "degrees Celsius")) %>%
  select(-UNITS) %>%
  pivot_wider(names_from = "VARIABLE", values_from = "VALUE") %>%
  filter(!is.na(`Dissolved Oxygen`))

# calculate and apply salinity correction factor
dat_corr <- dat %>%
  DO_salinity_correction(Sal = sal) %>%
  mutate(
    dissolved_oxygen_uncorrected_mg_per_l = `Dissolved Oxygen` / F_s,
    dissolved_oxygen_uncorrected_mg_per_l =
      round(dissolved_oxygen_uncorrected_mg_per_l, digits = 2))

# pivot longer again so can add back to data (just do; do not add temperature)
dat_long <- dat_corr %>%
  select(-c(`Dissolved Oxygen`, Temperature, Salinity, F_s)) %>%
  rename(`Dissolved Oxygen` = dissolved_oxygen_uncorrected_mg_per_l) %>%
  pivot_longer(
    cols = "Dissolved Oxygen", names_to = "VARIABLE", values_to = "VALUE") %>%
  mutate(UNITS = "mg/L")

# check for nas
dat_na <- dat_do %>%
  filter(is.na(VALUE))
# One row with NA for "Shut-In Island 2021-May-21 to 2021-Nov-26"
# Re-visited the original Trim script and found the trim date for
# dissolved oxygen was BEFORE the trim date for temperature,
# i.e., there is no corresponding temperature observation for the first
# DO obs, and so F_s cannot be calculated.


# verify that uncorrected data matches the raw values in corresponding HOBO
# files (spot check)
y <- dat_corr %>%
  distinct(STATION, DEPLOYMENT_PERIOD, SENSOR, DEPTH)

i <- 6

dat_i <- dat_corr %>%
  filter(STATION == y$STATION[i], DEPLOYMENT_PERIOD == y$DEPLOYMENT_PERIOD[i])

# sensors that probably have the wrong serial number
z <- y %>%
  filter(!str_detect(SENSOR, "208"))





