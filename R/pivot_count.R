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
#'
#' create_tidy_titanic() %>%
#' pivot_count(c(Sex, Age), c(Class, Survived))
pivot_count <- function(data, rows = NULL, cols = NULL, pivot = T){

  cols_quo <- rlang::enquo(cols)

  tidy <- data %>%
    dplyr::group_by(dplyr::across(c({{rows}}, {{cols}})), .drop = FALSE) %>%
    dplyr::summarize(value = dplyr::n()) %>%
    # dplyr::mutate(value = tidyr::replace_na(.data$value, 0)) %>%
    dplyr::arrange(dplyr::across(c({{cols}}, {{rows}}))) %>%
    dplyr::ungroup()

  if(rlang::quo_is_null(cols_quo) | pivot == F) return(tidy)

  tidy %>%
    tidyr::pivot_wider(names_from = {{cols}}, values_fill = list(value = 0)) %>%
    dplyr::arrange(dplyr::across(c({{rows}})))
}

