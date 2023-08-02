# July 28, 2023

# This script generates and html file for each county to highlight deployments
# dissolved oxygen observations flagged by the rolling standard deviation test

library(dplyr)
library(here)
library(rmarkdown)
library(stringr)
library(tidyr)


counties <- c("annapolis", "antigonish", "colchester",
              "digby", "guysborough", "inverness",
              "halifax", "lunenburg", "pictou", "queens",
              "richmond", "shelburne", "yarmouth")

counties <- "Lunenburg"

thresh <- 2.23

# export html file for each county showing the flagged observations
sapply(counties, function(x) {

  rmarkdown::render(
    input = here("R/dissolved_oxygen_percent_saturation/3.2_apply_do_rolling_sd_flag.rmd"),
    output_file = here(paste0("output/do_rolling_sd_", thresh, "/", x, "_do_rate_of_change.html")),
    params = list(county = x, thresh = thresh))

})

