#' Title
#'
#' @param data
#' @param y
#' @param x
#' @param pivot
#'
#' @return
#' @export
#'
#' @examples
pivot_calc <- function(data, y, y0, y00, x = NULL,
                       value,
                       fun = sum,
                       pivot = T #ifelse(is.null(x),F,T)
){

  y00 <- enquo(y00)
  y0 <- enquo(y0)
  y <- enquo(y)
  x <- enquo(x)
  value <- enquo(value)

  tidy <- data %>%
    dplyr::group_by(!!y00, !!y0, !!y, !!x) %>%
    dplyr::summarize(value = fun(!!value)) %>%
    dplyr::ungroup()

  if(pivot){#or x is null
    tidy %>%
      tidyr::pivot_wider(names_from = !!x)
  }else{
    tidy
  }

}
