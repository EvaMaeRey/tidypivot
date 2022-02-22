pivot_count <- function(data, y, y0, y00, x = NULL,
                        value = NULL,
                        fun = sum,
                        pivot = T #ifelse(is.null(x),F,T)
){

  y00 <- enquo(y00)
  y0 <- enquo(y0)
  y <- enquo(y)
  x <- enquo(x)

  data <- data %>% mutate(count = 1)

  tidy <- data %>%
    group_by(!!y00, !!y0, !!y, !!x) %>%
    summarize(value = fun(count)) %>%
    ungroup()

  if(pivot){#or x is null
    tidy %>%
      pivot_wider(names_from = !!x)
  }else{
    tidy
  }

}
