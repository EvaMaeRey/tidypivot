#' Title
#'
#' @param data
#' @param y
#' @param y0
#' @param y00
#' @param x
#' @param value
#' @param fun
#' @param pivot
#'
#' @return
#' @export
#'
#' @examples
#' library(tidyverse)
#' library(magrittr)
#' create_tidy_titanic() %>%
#' pivot_count(Sex, Survived)
pivot_count <- function(data, x = NULL, y = NULL, pivot = F){

  cols_quo <- enquo(x)

  tidy <- data %>%
    group_by(across(c({{y}}, {{x}}))) %>%
    summarize(value = n()) %>%
    ungroup()

  if(rlang::quo_is_null(cols_quo) | pivot == F) return(tidy)

  tidy %>%
    pivot_wider(names_from = {{x}})
}


