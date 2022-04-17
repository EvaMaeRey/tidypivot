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
pivot_count <- function(data, cols = NULL, rows = NULL, pivot = T, wt = NULL){

  cols_quo <- rlang::enquo(cols)
  cols_quo <- rlang::enquo(cols)

  grouped <- data %>%
    dplyr::group_by(dplyr::across(c({{cols}}, {{rows}})), .drop = FALSE)

  summarized <- grouped %>%
    dplyr::summarize(value = dplyr::n())

  arranged <- summarized# %>%
    # tidyr::complete(dplyr::across(c({{cols}}, {{rows}}))) %>%
    # dplyr::mutate(value = tidyr::replace_na(.data$value, 0)) %>%
    # dplyr::arrange(dplyr::across(c({{rows}}, {{cols}})))

  ungrouped <- arranged %>%
    dplyr::ungroup()

  tidy <- ungrouped


  # do not pivot if argument pivot false or no columns specified
  if(pivot == F | rlang::quo_is_null(cols_quo)){

    tidy %>%
      dplyr::rename(count = .data$value)

  # otherwise pivot by columns
  }else{

  tidy %>%
    tidyr::pivot_wider(names_from = {{cols}})

  }

  # browser()


}
