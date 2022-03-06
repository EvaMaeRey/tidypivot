
#' library(tidyverse)
#' library(magrittr)
#' create_tidy_titanic() %>%
#' pivot_count(Sex, Survived)
pivot_count <- function(data, cols = NULL, rows = NULL, pivot = T){

  cols_quo <- enquo(cols)

  tidy <- data %>%
    group_by(across(c({{rows}}, {{cols}}))) %>%
    summarize(value = n()) %>%
    ungroup()

  if(rlang::quo_is_null(cols_quo) | pivot == F) return(tidy)

  tidy %>%
    pivot_wider(names_from = {{cols}})
}


