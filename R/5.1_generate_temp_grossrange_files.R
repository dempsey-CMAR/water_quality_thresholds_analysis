# February 1, 2023

# This script generates and html file for each county to highlight deployments
# temperature observations flagged by the grossrange test

library(dplyr)
library(here)
library(rmarkdown)
library(stringr)
library(tidyr)

# format county to match the names of the rds files (separated into waterbody for some counties)
counties <- data.frame(counties = list.files(here("data-raw"))) %>%
  separate(
    counties,
    into = c("county", "waterbody", NA),
    sep = "_", fill = "left"
  ) %>%
  mutate(county = if_else(is.na(county), waterbody, county)) %>%
  distinct(county)

counties <- counties$county

# export html file for each county showing the flagged observations
sapply(counties, function(x) {

  rmarkdown::render(
    input = here("R/5.2_apply_temp_grossrange_flag.rmd"),
    output_file = here(paste0("output/temp_grossrange/", x, "_temp_grossrange.html")),
    params = list(county = x))

})



#county = paste(county, waterbody, sep = "_"))
#county = if_else(str_detect(county, "^_"), str_remove(county, "_"), county)
