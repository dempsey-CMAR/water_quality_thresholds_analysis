library(dplyr)

summarise_grouped_data <- function(dat, group_name, ...) {

  dat %>%
    group_by(...) %>%
    summarise(
      mean = round(mean(VALUE), digits = 3),
      stdev = round(sd(VALUE), digits = 3),
      n = n()
    ) %>%
    mutate(group = group_name) %>%
    ungroup()
}
