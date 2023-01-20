
library(dplyr)
library(here)
library(lubridate)
library(sensorstrings)
library(strings)
library(readr)

dat_all <- import_strings_data(input_path = here("data-raw")) %>%
  select(COUNTY, WATERBODY, STATION, TIMESTAMP, DEPTH, VARIABLE, VALUE, UNITS)

all <- dat_all %>%
  group_by(VARIABLE, UNITS) %>%
  summarise(
    mean = round(mean(VALUE), digits = 3),
    stdev = round(sd(VALUE), digits = 3),
    n = n()
  ) %>%
  mutate(group = "all_data") %>%
  ungroup()

county <- dat_all %>%
  group_by(COUNTY, VARIABLE, UNITS) %>%
  summarise(
    mean = round(mean(VALUE), digits = 3),
    stdev = round(sd(VALUE), digits = 3),
    n = n()
  ) %>%
  mutate(group = "county") %>%
  ungroup()

county_depth <- dat_all %>%
  mutate(DEPTH = round(as.numeric(DEPTH))) %>%
  group_by(COUNTY, DEPTH, VARIABLE, UNITS) %>%
  summarise(
    mean = round(mean(VALUE), digits = 3),
    stdev = round(sd(VALUE), digits = 3),
    n = n()
  ) %>%
  mutate(group = "county_depth") %>%
  ungroup()


county_month <- dat_all %>%
  mutate(MONTH = month(TIMESTAMP)) %>%
  group_by(COUNTY, MONTH, VARIABLE, UNITS) %>%
  summarise(
    mean = round(mean(VALUE), digits = 3),
    stdev = round(sd(VALUE), digits = 3),
    n = n()
  ) %>%
  mutate(group = "county_month") %>%
  ungroup()

all_month <- dat_all %>%
  mutate(MONTH = month(TIMESTAMP)) %>%
  group_by(MONTH, VARIABLE, UNITS) %>%
  summarise(
    mean = round(mean(VALUE), digits = 3),
    stdev = round(sd(VALUE), digits = 3),
    n = n()
  ) %>%
  mutate(group = "all_month") %>%
  ungroup()

all_station <- dat_all %>%
  group_by(COUNTY, STATION, VARIABLE, UNITS) %>%
  summarise(
    mean = round(mean(VALUE), digits = 3),
    stdev = round(sd(VALUE), digits = 3),
    n = n()
  ) %>%
  mutate(group = "all_station") %>%
  ungroup()

all_depth <-  dat_all %>%
  mutate(DEPTH = round(as.numeric(DEPTH))) %>%
  group_by(DEPTH, VARIABLE, UNITS) %>%
  summarise(
    mean = round(mean(VALUE), digits = 3),
    stdev = round(sd(VALUE), digits = 3),
    n = n()
  ) %>%
  mutate(group = "all_depth") %>%
  ungroup()


dat_out <- bind_rows(
  all, all_month, all_station, all_depth,
  county, county_depth, county_month
) %>%
  rename(
    variable = VARIABLE,
    units = UNITS,
    county = COUNTY,
    station = STATION,
    depth = DEPTH,
    month = MONTH
  )

write_csv(dat_out, here("data/summary.csv"))


# write_csv(dat_summary, here("data/all_summary.csv"))
#
# write_csv(dat_county, here("data/county_summary.csv"))
#
# write_csv(dat_county_depth, here("data/county_depth_summary.csv"))
#
# write_csv(dat_county_month, here("data/county_month_summary.csv"))
#
# write_csv(dat_all_month, here("data/all_month_summary.csv"))







