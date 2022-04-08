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
pivot_count <- function(data, cols = NULL, rows = NULL, pivot = T){

  cols_quo <- rlang::enquo(cols)

  tidy <- data %>%
    dplyr::group_by(dplyr::across(c({{cols}}, {{rows}})), .drop = FALSE) %>%
    dplyr::summarize(value = dplyr::n()) %>%
    # tidyr::complete(dplyr::across(c({{cols}}, {{rows}}))) %>%
    dplyr::mutate(value = tidyr::replace_na(.data$value, 0)) %>%
    dplyr::arrange(dplyr::across(c({{rows}}, {{cols}}))) %>%
    dplyr::ungroup()

  # do not pivot if argument pivot false or no columns specified
  if(pivot == F | rlang::quo_is_null(cols_quo)){

    tidy

  # otherwise pivot by columns
  }else{

  tidy %>%
    tidyr::pivot_wider(names_from = {{cols}})

  }

}
