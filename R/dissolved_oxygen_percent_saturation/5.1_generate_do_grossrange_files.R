# February 1, 2023

# This script generates and html file for each county to highlight deployments
# dissolved oxygen observations flagged by the grossrange test

library(dplyr)
library(here)
library(rmarkdown)
library(stringr)
library(tidyr)


counties <- c("Annapolis", "Antigonish",
              "Colchester", "Digby", "Guysborough",
              "Halifax", "Inverness", "Lunenburg", "Pictou", "Queens",
              "Richmond", "Shelburne", "Yarmouth")

# export html file for each county showing the flagged observations
sapply(counties, function(x) {

  rmarkdown::render(
    input = here("R/5.2_apply_do_grossrange_flag.rmd"),
    output_file = here(paste0("output/do_grossrange/", x, "_do_grossrange.html")),
    params = list(county = x))

})



#county = paste(county, waterbody, sep = "_"))
#county = if_else(str_detect(county, "^_"), str_remove(county, "_"), county)
