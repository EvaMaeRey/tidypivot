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
pivot_calc <- function(data, rows = NULL, cols = NULL,
                       value = NULL,
                       fun = sum,
                       pivot = T #ifelse(is.null(x),F,T)
){
#
#   y00 <- enquo(y00)
#   y0 <- enquo(y0)
#   y <- enquo(y)
#   x <- enquo(cols)
#   value <- enquo(value)

  tidy <- data %>%
    dplyr::group_by(across(c({{cols}}, {{rows}})), .drop = FALSE) %>%
    dplyr::summarize(value = fun({{value}})) %>%
    dplyr::ungroup()

  if(pivot){#or x is null
    tidy %>%
      tidyr::pivot_wider(names_from = {{cols}})
  }else{
    tidy
  }

}
