# March 9, 2023

# This script generates and html file for each county to highlight deployments
# dissolved oxygen observations flagged by the climatology test

library(here)
library(rmarkdown)

counties <- c("Annapolis", "Antigonish",
              "Colchester", "Digby", "Guysborough",
              "Halifax", "Inverness", "Lunenburg", "Pictou", "Queens",
              "Richmond", "Shelburne", "Yarmouth")

# export html file for each county showing the flagged observations
sapply(counties, function(x) {

  rmarkdown::render(
    input = here("R/dissolved_oxygen_percent_saturation/5.4_apply_do_climatology_flag.rmd"),
    output_file = here(paste0("output/do_climatology/", x, "_do_climatology.html")),
    params = list(county = x))

})



#county = paste(county, waterbody, sep = "_"))
#county = if_else(str_detect(county, "^_"), str_remove(county, "_"), county)
