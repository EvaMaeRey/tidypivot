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
      tidyr::pivot_wider(names_from = !!x)
  }else{
    tidy
  }

}


# Titanic %>%
#   data.frame() %>%
#   uncount(weights = Freq) ->
#   tidy_titanic ; tidy_titanic %>% head()
#
# pivot_table <- function(data, rows = NULL, cols = NULL){
#
#   cols_quo <- enquo(cols)
#
#   tidy <- data %>%
#     group_by(across(c({{rows}}, {{cols}}))) %>%
#     summarize(value = n()) %>%
#     ungroup()
#
#   if(rlang::quo_is_null(cols_quo)) return(tidy)
#
#   tidy %>%
#     pivot_wider(names_from = {{cols}})
# }
#
# tidy_titanic %>%
#   pivot_table(cols = c(Class, Survived), rows = Sex)
#
# tidy_titanic %>%
#   pivot_table(cols = c(Class, Survived))
#
# tidy_titanic %>%
#   pivot_table(rows = c(Class, Survived))
