# August 17, 2023

# Removes salinity correction factor (F_s) applied to the DO concentration data
# in the 2022 Open Data Portal release

# The DO concentration data sent to the Open Data Portal on December 2022
# was corrected for Salinity using strings::do_salinity_correction()
# DO_corrected = DO_raw * F_s, where F_s is a salinity correction factor between
# 0 and 1

# The newly processed data will NOT include a salinity correction factor.
# So to calculate thresholds based on historical data, we need to remove the
# correction factor
# DO_raw = DO_corrected / F_s

# param dat Tody data in OLD format
# param sal Salinity value used to calculate salinity correction factor. Default is
# sal = 30.6, which was used in the old compile scripts.


remove_do_correction <- function(dat, sal = 30.6, keep_temp = FALSE) {

  dat <- dat %>%
    filter(UNITS %in% c("mg/L", "degrees Celsius")) %>%
    # remove UNITS so that corresponding temp and DO will be in same row
    select(-UNITS) %>%
    pivot_wider(names_from = "VARIABLE", values_from = "VALUE") %>%
    # remove rows with only Temperature data
    filter(!is.na(`Dissolved Oxygen`))

  temp_na <- dat %>%
    filter(is.na(Temperature))

  if(nrow(temp_na) > 0) {
    dat <- dat %>%
      filter(!is.na(Temperature))

    warning(
      paste0(nrow(temp_na), " dissolved oxygen observation(s) did not have corresponding temperature value
            and were removed from analysis: ",
             paste(temp_na$STATION, temp_na$DEPLOYMENT_PERIOD, collapse = "\n"))
    )

  }

  dat <- dat %>%
    DO_salinity_correction(Sal = sal) %>%
    mutate(
      # remove correction factor and round
      dissolved_oxygen_uncorrected_mg_per_l = `Dissolved Oxygen` / F_s,
      dissolved_oxygen_uncorrected_mg_per_l =
        round(dissolved_oxygen_uncorrected_mg_per_l, digits = 2))  #%>%

  if(isFALSE(keep_temp)) {
    dat <- dat %>%
      select(-c(`Dissolved Oxygen`, Temperature, Salinity, F_s)) %>%
      # re-format into old style
      rename(`Dissolved Oxygen` = dissolved_oxygen_uncorrected_mg_per_l) %>%
      pivot_longer(
        cols = "Dissolved Oxygen", names_to = "VARIABLE", values_to = "VALUE") %>%
      mutate(UNITS = "mg/L")
  }

  if(isTRUE(keep_temp)) {
    dat <- dat %>%
      select(-c(`Dissolved Oxygen`, Salinity, F_s)) %>%
      # re-format into old style
      rename(`Dissolved Oxygen` = dissolved_oxygen_uncorrected_mg_per_l) %>%
      pivot_longer(
        cols = c("Dissolved Oxygen", "Temperature"),
        names_to = "VARIABLE", values_to = "VALUE") %>%
      mutate(
        UNITS = if_else(VARIABLE == "Dissolved Oxygen", "mg/L", "degrees Celsius")
      )
  }

  dat

}


