#' Proportion table
#'
#' Obtain the proportional distribution of a variable, optionally by grouping variables.
#' @param x A `tbl`.
#' @param var_name Variable in `x` of which we want the proportional distribution.
#' @param ... Optional grouping vars, comma separated, unquoted.
#' @param round Rounding of the proportions.
#' @return An object of the same class as `x`.
#' @details Original idea from https://stackoverflow.com/questions/24576515/
tt_prop_table <- function(x,
                          var_name,
                          ...,
                          round = 3) {
  valid_source(x)
  var_name_q <- enquo(var_name)
  x %>%
    group_by(...) %>%
    count(!!var_name_q) %>%
    mutate(prop = (n / sum(n)) %>% round(round)) %>%
    ungroup()
}
