#' Title
#'
#' @param data A data frame, data frame extension (e.g. a tibble), or a lazy data frame (e.g. from dbplyr or dtplyr). See Methods, below, for more details.
#' @param cols a character vector of items
#' @param rows a character vector of items
#' @param pivot logical: should wide table be returned - col categories as columns (TRUE), or left long and tidy (FALSE)?
#'
#' @return
#' @export
#'
#' @examples
#' tidy_titanic %>% pivot_count(rows = sex)
#' tidy_titanic %>% pivot_count(cols = sex)
#' tidy_titanic %>% pivot_count(rows = survived, cols = sex)
#' tidy_titanic %>% pivot_count(rows = c(survived, sex), cols = age)
#' tidy_titanic %>% pivot_count(rows = c(survived), cols = c(age, sex))
#' tidy_titanic %>% pivot_count(cols = c(survived), rows = c(age, sex, class))
#' tidy_titanic %>% pivot_count(rows = c(survived, sex), cols = age, pivot = F)
#' flat_titanic %>% pivot_count(rows = sex, wt = freq)
pivot_count <- function(data, cols = NULL,
                        rows = NULL, pivot = T, wt = NULL){

  fun <- sum # this will be a variable in pivot_calc

  # allow for default behaviors under null
  cols_quo <- rlang::enquo(cols)
  wt_quo <- rlang::enquo(wt)

  # declare grouping
  grouped <- data %>%
    dplyr::group_by(dplyr::across(c({{cols}}, {{rows}})),
                    .drop = FALSE)

  # behavior if no wt
  if(rlang::quo_is_null(wt_quo)){
  summarized <- grouped %>%
    dplyr::mutate(value = 1) %>%
    dplyr::summarize(value = fun(value))

  # behavior with wt
  }else{
  summarized <- grouped %>%
    dplyr::summarise(value = fun({{wt}}))

  }

  # placeholder for arrangement
  arranged <- summarized

  # ungrouping, preserving unpivoted object
  tidy <- arranged %>%
    dplyr::ungroup()

  # do not pivot if argument pivot false or if no columns specified
  if(pivot == F | rlang::quo_is_null(cols_quo)){

    tidy %>%
      dplyr::rename(count = .data$value)

  # otherwise pivot by columns
  }else{

  tidy %>%
    tidyr::pivot_wider(names_from = {{cols}})

  }

}
