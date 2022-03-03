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
pivot_count <- function(data, y, y0, y00, x = NULL,
                        value = NULL,
                        fun = sum,
                        pivot = T #ifelse(is.null(x),F,T)
){

  y00 <- enquo(y00)
  y0 <- enquo(y0)
  y <- enquo(y)
  x <- enquo(x)

  data <- data %>% dplyr::mutate(count = 1)

  tidy <- data %>%
    dplyr::group_by(!!y00, !!y0, !!y, !!x) %>%
    dplyr::summarize(value = fun(.data$count)) %>%
    dplyr::ungroup()

  if(pivot){#or x is null
    tidy %>%
      tidyr::pivot_wider(names_from = !!x,
                         values_from = value)
  }else{
    tidy
  }

}
