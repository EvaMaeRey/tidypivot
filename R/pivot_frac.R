#' Title
#'
#' @inheritParams pivot_count
#'
#' @return
#' @export
#'
#' @examples
#' tidy_titanic %>%
#'     pivot_frac(rows = survived, cols = sex, within = sex)
#' tidy_titanic %>%
#'     pivot_frac(rows = survived, cols = c(sex, age), within = c(sex, age))

pivot_frac <- function(data, rows = NULL, cols = NULL,
                       value = NULL,
                       within = NULL,  pivot = T,
                       percent = T, round = F){

  cols_quo <- rlang::enquo(cols)
  value_quo <- rlang::enquo(value)

  if(rlang::quo_is_null(value_quo)){
    data <- data %>% dplyr::mutate(value = 1)
  }else{
    data <- data %>%
      dplyr::mutate(value = fun({{value}}))
  }

  data %>%
    dplyr::group_by(across(c({{rows}}, {{cols}})), .drop = FALSE) %>%
    dplyr::summarize(value = fun(value)) %>%
    dplyr::group_by(across(c({{within}}))) %>%
    dplyr::mutate(prop = paste0(value, "/", sum(value))) %>%
    dplyr::select(-value) %>%
    dplyr::ungroup() ->
  tidy

  if(pivot){

    tidy %>%
      tidyr::pivot_wider(values_from = prop, names_from = {{cols}})

  }else{

    tidy
  }

}
