# August 28, 2023

# Applies additional QC (trim) to:

## dissolved oxygen - concentration data based on reviewing
### New trim dates are based on
### 0.1_do_mg_per_l_additional_qc.html and 2.1_do_distribution.html

## salinity data
### New trim dates are based on
### 1.1_explore_sal.html and 0.1_sal_additional_qc.html

# TRIM_DATE is the date of the last *GOOD* observation

# template for the trim file
# dat_raw <- import_strings_data(input_path = here("data-raw"))
#
# do <- dat_raw %>%
#   filter(VARIABLE == "Dissolved Oxygen", UNITS == "mg/L") %>%
#   distinct(VARIABLE, COUNTY, STATION, DEPLOYMENT_PERIOD) %>%
#   arrange(COUNTY, STATION)
#
# sal <- dat_raw %>%
#   filter(VARIABLE == "Salinity") %>%
#   distinct(VARIABLE, COUNTY, STATION, DEPLOYMENT_PERIOD) %>%
#   arrange(COUNTY, STATION)
#
# do_sal <- bind_rows(do, sal)
#
# write_csv(do_sal, here("data/do_sal_additional_qc.csv"))


filter_out_suspect_obs <- function(dat) {
  # dates for additional qc
  trim <- read_csv(
    here("data/do_sal_additional_qc.csv"), show_col_types = FALSE) %>%
    select(-NOTES)

  # data that does not require additional trim
  dat_hold <- dat %>%
    filter(
      !(VARIABLE == "Dissolved Oxygen" & UNITS == "mg/L") &
        VARIABLE != "Salinity"
    )

  # apply additional trim to do - concentration and salinity data
  dat_trim <- dat %>%
    filter(
      (VARIABLE == "Dissolved Oxygen" & UNITS == "mg/L") |
        VARIABLE == "Salinity"
    ) %>%
    left_join(
      trim,
      by = c("COUNTY", "STATION", "DEPLOYMENT_PERIOD", "VARIABLE")) %>%
    # remove deployments marked as "do not use"
    filter(TRIM_DATE != "DNU") %>%
    # the TRIM_DATE is the last *GOOD* observation; however, the filter
    # will exclude observations on this day, e.g.,
    # as_datetime("2023-08-30 08:13:00") <= as_datetime("2023-08-30")
    # is FALSE.
    # So add 1 day to TRIM_DATE so observations on the original date are kept
    mutate(
      TRIM_DATE = as_datetime(TRIM_DATE),
      TRIM_DATE = TRIM_DATE + days(1)
    ) %>%
    # trim suspect observations
    filter(TIMESTAMP <= TRIM_DATE) %>%
    select(-TRIM_DATE)

  dat_out <- bind_rows(dat_hold, dat_trim)

  if("WATERBODY" %in% colnames(dat_out)) {
    dat_out %>%
      arrange(COUNTY, WATERBODY, STATION, VARIABLE, TIMESTAMP)
  } else {
    dat_out %>%
      arrange(COUNTY, STATION, VARIABLE, TIMESTAMP)
  }
}
