# August 21, 2023

# This script generates and html file for each county to highlight deployments
# dissolved oxygen observations flagged by the rolling standard deviation test

library(dplyr)
library(here)
library(rmarkdown)
library(stringr)
library(tidyr)

# **these are old. Update if re-running
thresh <- c(0.51, 0.71,	0.86,	1.11, 1.38) # #q90, q95, q97, q99, q99.7 of filtered data

thresh <- 0.51

# export html file for each county showing the flagged observations
sapply(thresh, function(x) {

  rmarkdown::render(
    input = here("R/dissolved_oxygen_mg_per_l/3.2_apply_do_mg_per_l_rolling_sd_flag.rmd"),
    output_file = here(paste0("output/do_mg_per_l_rolling_sd/", x, ".html")),
    params = list(thresh = x))

})

