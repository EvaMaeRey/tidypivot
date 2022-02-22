pivot_prop <- function(data, y, y0, y00, x = NULL, value = NULL, fun = sum, within, within2,  pivot = !is.null(x), percent = T, round = F){

  y00 <- enquo(y00)
  y0 <- enquo(y0)
  x <- enquo(x)
  y <- enquo(y)
  within <- enquo(within)
  within2 <- enquo(within2)


  if(is.null(value)){
    data <- data %>% dplyr::mutate(value = 1)
  }else{
    value <- enquo(value)
  }

  data %>%
    dplyr::group_by(!!y00, !!y0, !!y, !!x) %>%
    dplyr::summarize(value = fun(value)) %>%
    dplyr::group_by(!!within, !!within2) %>%
    dplyr::mutate(prop = (value/sum(value)*ifelse(percent, 100, 1)) %>% round(1)) %>%
    dplyr::select(-value) %>%
    dplyr::ungroup() ->
    tidy

  if(pivot){

    tidy %>%
      tidyr::pivot_wider(values_from = prop, names_from = !!x)

  }else{

    tidy
  }

}
