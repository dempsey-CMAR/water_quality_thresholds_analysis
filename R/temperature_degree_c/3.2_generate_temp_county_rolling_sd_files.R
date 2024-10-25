# July 28, 2023

# This script generates and html file for each county to highlight deployments
# dissolved oxygen observations flagged by the grossrange test

library(dplyr)
library(here)
library(rmarkdown)
library(stringr)
library(tidyr)


counties <- c("annapolis", "antigonish", "colchester", "cape breton",
              "digby", "guysborough",
              "halifax", "inverness", "lunenburg", "pictou", "queens",
              "richmond", "shelburne", "victoria", "yarmouth")

#counties <- "victoria"

# calculate thresholds
# quartile_table <-readRDS(here("data/temp_rolling_sd_prelim_qc.rds")) %>%
#   select(
#     -c(sensor_type, int_sample, n_sample, rolling_sd_flag_temperature_degree_c)
#   ) %>%
#   rename(value = value_temperature_degree_c) %>%
#   filter(
#     !(station %in% c("Piper Lake", "Hourglass Lake", "0193", "Sissiboo")),
#     !(county == "Inverness" & sensor_depth_at_low_tide_m %in% c(18, 23, 26, 28, 36, 40))
#   ) %>%
#   group_by(county) %>%
#   qc_calculate_rolling_sd_thresholds(
#     var = value_temperature_degree_c, stat = "quartile", prob = 0.999) %>%
#   ungroup()

#library(data.table)
#fwrite(quartile_table, here("output/temperature_rolling_sd_thresholds.csv"))

thresh <- "q_999"

# export html file for each county showing the flagged observations
sapply(counties, function(x) {

  rmarkdown::render(
    input = here("R/temperature_degree_c/3.2_apply_temp_county_rolling_sd_flag.rmd"),
    output_file = here(paste0("output/temp_rolling_sd_q_999/", x, "_temp_rolling_sd.html")),
    params = list(county = x, thresh = thresh))

})

