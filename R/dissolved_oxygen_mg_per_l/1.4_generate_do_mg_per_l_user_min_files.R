# Aug 21, 2023

# This script generates and html file for each county to highlight deployments
# dissolved oxygen observations flagged by the rolling standard deviation test

library(dplyr)
library(here)
library(rmarkdown)
library(stringr)
library(tidyr)

thresh <- 8.346

# export html file for each county showing the flagged observations

rmarkdown::render(
  input = here("R/dissolved_oxygen_mg_per_l/1.3_apply_do_mg_per_l_user_flag.rmd"),
  output_file = here("output/do_mg_per_l_user_min_filtered_data.html"),
  params = list(thresh = thresh))



