# February 1, 2023

# calculates the mean, standard deviation, and number of rows for
# each group as specified in ...

# dat: dataframe with at least 1 column, named VALUE. Additional columns can be
## used for grouping the summary results.

# group_name: character string indicating the name of the group.

# ...: name(s) of grouping column(s)

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
