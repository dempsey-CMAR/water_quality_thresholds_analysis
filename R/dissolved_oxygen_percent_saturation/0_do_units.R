
library(dplyr)
library(here)
library(readr)
library(strings)

# all data
dat_all <- import_strings_data(input_path = here("data-raw")) %>%
  select(COUNTY, WATERBODY, STATION, TIMESTAMP, DEPTH, VARIABLE, VALUE, UNITS) %>%
  filter(VARIABLE == "Dissolved Oxygen")

do_units <- dat_all %>%
  group_by(STATION) %>%
  distinct(UNITS) %>%
  mutate(
    n_units = n(),
    do_units = if_else(n_units == 1, UNITS, "percent saturation & mg/L")
  ) %>%
  select(STATION, do_units) %>%
  ungroup() %>%
  distinct()


write_csv(do_units, here("data/do_units.csv"))
