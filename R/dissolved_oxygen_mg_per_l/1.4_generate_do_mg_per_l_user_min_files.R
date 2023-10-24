# Aug 21, 2023
# September 27: updated to use most recent version of qaqcmar

# This script generates an html file showing all do - concentration deployments
# Observations *below* thresh are flagged



library(dplyr)
library(here)
library(rmarkdown)
library(stringr)
library(tidyr)

thresh <- 8.346


rmarkdown::render(
  input = here("R/dissolved_oxygen_mg_per_l/1.4_apply_do_mg_per_l_user_flag.rmd"),
  output_file = here("output/do_mg_per_l_user_min/do_mg_per_l_user_min_reprocessed_data.html"),
  params = list(thresh = thresh))



