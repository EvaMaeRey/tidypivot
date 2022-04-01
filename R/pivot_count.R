#' Title
#'
#' @param data
#' @param cols
#' @param rows
#' @param pivot
#'
#' @return
#' @export
#'
#' @examples
#' library(magrittr)
#' create_tidy_titanic() %>%
#' pivot_count(Sex, Survived)
pivot_count <- function(data, cols = NULL, rows = NULL, pivot = T){

  cols_quo <- rlang::enquo(cols)

  tidy <- data %>%
    dplyr::group_by(dplyr::across(c({{rows}}, {{cols}})), .drop = FALSE) %>%
    dplyr::summarize(value = dplyr::n()) %>%
    dplyr::ungroup()

  if(rlang::quo_is_null(cols_quo) | pivot == F) return(tidy)

  tidy %>%
    tidyr::pivot_wider(names_from = {{cols}})
}


