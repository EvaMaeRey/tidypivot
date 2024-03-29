#' Title
#'
#' @inheritParams pivot_count
#'
#' @return
#' @export
#'
#' @examples
#' flat_titanic %>% pivot_calc(rows = sex, value = freq)
#' flat_titanic %>% pivot_calc(rows = sex, fun = mean, value = freq)
pivot_calc <- function(data, rows = NULL, cols = NULL,
                       value = NULL,
                       fun = sum,
                       pivot = T
){

    cols_quo <- rlang::enquo(cols)
    value_quo <- rlang::enquo(value)

    grouped <- data %>%
      dplyr::group_by(dplyr::across(c({{cols}}, {{rows}})),
                      .drop = FALSE)

    if(rlang::quo_is_null(value_quo)){
      summarized <- grouped %>%
        dplyr::summarize(value = dplyr::n())
    }else{
      summarized <- grouped %>%
        dplyr::summarise(value = fun({{value}}))

    }

    arranged <- summarized

    ungrouped <- arranged %>%
      dplyr::ungroup()

    tidy <- ungrouped

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
