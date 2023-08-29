# August 28, 2023

# apply additional QC (trim) to salinity data
# based on reviewing 1.1_explore_sal.html and 0.1_sal_additional_qc.html


# sal <- dat_raw %>%
#   filter(VARIABLE == "Salinity") %>%
#   distinct(COUNTY, STATION, DEPLOYMENT_PERIOD) %>%
#   arrange(COUNTY, STATION)
#
# write_csv(sal, here("data/salinity_additional_qc.csv"))
#

filter_out_sus_salinity_obs <- function(dat) {

  # dates for additional salinity qc
  sal_trim <- read_csv(
    here("data/salinity_additional_qc.csv"), show_col_types = FALSE) %>%
    select(-NOTES)

  dat %>%
    filter(VARIABLE == "Salinity") %>%
    left_join(sal_trim, by = c("COUNTY", "STATION", "DEPLOYMENT_PERIOD")) %>%
    # remove deployments marked as "do not use"
    filter(TRIM_DATE != "DNU") %>%
    mutate(TRIM_DATE = as_datetime(TRIM_DATE)) %>%
    # trim suspect observations
    filter(TIMESTAMP <= TRIM_DATE)

  #sal <- dat_raw %>%
  #filter(VARIABLE == "Salinity")

}
