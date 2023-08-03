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

counties <- "victoria"

thresh <- "q_99"

# export html file for each county showing the flagged observations
sapply(counties, function(x) {

  rmarkdown::render(
    input = here("R/temperature_degree_c/3.2_apply_temp_county_rolling_sd_flag.rmd"),
    output_file = here(paste0("output/temp_rolling_sd_", thresh, "/", x, "_temp_rolling_sd.html")),
    params = list(county = x, thresh = thresh))

})

