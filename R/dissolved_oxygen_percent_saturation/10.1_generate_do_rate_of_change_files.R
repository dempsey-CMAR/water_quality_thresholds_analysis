# May 10, 2023

# This script generates and html file for each county to highlight deployments
# dissolved oxygen observations flagged by the grossrange test

library(dplyr)
library(here)
library(rmarkdown)
library(stringr)
library(tidyr)


counties <- c("annapolis", "antigonish", "colchester",
              "digby", "guysborough",
              "halifax", "lunenburg", "pictou", "queens",
              "richmond", "shelburne", "yarmouth")

#counties <- "Inverness"

thresh <- 3.49

# export html file for each county showing the flagged observations
sapply(counties, function(x) {

  rmarkdown::render(
    input = here("R/dissolved_oxygen_percent_saturation/10.1_apply_do_rate_of_change_flag.rmd"),
    output_file = here(paste0("output/do_rate_of_change_", thresh, "/", x, "_do_rate_of_change.html")),
    params = list(county = x, thresh = thresh))

})



#county = paste(county, waterbody, sep = "_"))
#county = if_else(str_detect(county, "^_"), str_remove(county, "_"), county)
