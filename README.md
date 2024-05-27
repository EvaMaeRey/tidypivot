
  - [{tidypivot} allows you to create tables by describing
    them](#tidypivot-allows-you-to-create-tables-by-describing-them)
      - [example](#example)
          - [Internals](#internals)
  - [examples/derivitive](#examplesderivitive)
  - [Piping](#piping)

<!-- README.md is generated from README.Rmd. Please edit that file -->

# {tidypivot} allows you to create tables by describing them

note: see original discussion here:
<https://evamaerey.github.io/mytidytuesday/2022-02-14-tables/tables.html>
and thoughtful contributions from @shannonpileggi and @brshallow
<https://github.com/EvaMaeRey/mytidytuesday/issues/3>

## example

``` r
library(tidyverse)
library(tidypivot)
options(scipen = 10)

ggplot2::diamonds %>% 
  pivot_helper(rows = cut, 
     cols = color)
#> # A tibble: 5 × 8
#>   cut           D     E     F     G     H     I     J
#>   <ord>     <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 Fair        163   224   312   314   303   175   119
#> 2 Good        662   933   909   871   702   522   307
#> 3 Very Good  1513  2400  2164  2299  1824  1204   678
#> 4 Premium    1603  2337  2331  2924  2360  1428   808
#> 5 Ideal      2834  3903  3826  4884  3115  2093   896

tidytitanic::tidy_titanic %>% 
  pivot_helper(rows = sex,
               cols = survived,
               prop = T,
               within = sex)
#> # A tibble: 2 × 3
#>   sex       No   Yes
#>   <fct>  <dbl> <dbl>
#> 1 Male   0.788 0.212
#> 2 Female 0.268 0.732
```

### Internals

  - group by rows and columns
  - value in data to consider (1 if not specified)
  - wt, weight the value (1 if not specified)
  - fun - do an operation (on value) within group

<!-- end list -->

``` r
#' A function that pretty much let's you do anything in the tidypivot space, is carefully crafted, and is adapted to make the other functions easy to use
#'
# #' @inheritParams pivot_count
#'
#' @return
#' @export
#'
#' @examples
#' tidy_titanic %>% pivotr(rows = sex, cols = survived, fun = length) # pivot_count
#' flat_titanic %>% pivotr(rows = sex, value = freq, fun = mean) # pivot_calc
#' flat_titanic %>% pivotr(rows = sex, value = freq, fun = sum) # pivot_count (weighted sum)
#' nar <- function(x) return(NA)
#' flat_titanic %>% pivotr(rows = sex, cols = survived, fun = nar); #pivot_null
#' sample1 <- function(x) sample(x, 1)
#' flat_titanic %>% pivotr(rows = sex, cols = survived, fun = sample1, value = freq); #pivot_sample1
#' samplen <- function(x, n) paste(sample(x, 5, replace = F), collapse = ", ")
#' # flat_titanic %>% pivotr(rows = sex, cols = survived, fun = samplen, value = freq); #pivot_samplen
#' paste_collapse <- function(x) paste (x, collapse = ", ")
#' flat_titanic %>% pivotr(rows = sex, fun = paste_collapse, value = freq) # pivot_list
#' flat_titanic %>% pivotr(rows = sex, value = freq, prop = TRUE) # pivot_prop
#' flat_titanic %>% pivotr(rows = sex, cols = survived, value = freq, prop = TRUE)
#' flat_titanic %>% pivotr(rows = sex, cols = survived, value = freq, prop = TRUE, within = sex)
pivotr <- function(data,
                       rows = NULL,
                       cols = NULL,
                       value = NULL,
                       wt = NULL,
                       
                       fun = NULL,
                       
                       prop = FALSE,
                       percent = FALSE,
                       round = NULL,
                       
                       within = NULL,
                       withinfun = NULL,
                       
                       pivot = NULL,
                       wrap = NULL,
                       totals_within = NULL
){

  
    cols_quo          <- rlang::enquo(cols)
    value_quo         <- rlang::enquo(value)
    wt_quo            <- rlang::enquo(wt)
    within_quo        <- rlang::enquo(within)
    totals_within_quo <- rlang::enquo(totals_within)

    if(is.null(prop)) {prop <- FALSE}
    if(is.null(pivot)){pivot <- TRUE}
    if(is.null(wrap)) {wrap <- FALSE}

    if(is.null(fun))  {fun <- sum}

    ## adding a value as 1 if there is none
    
    if(rlang::quo_is_null(value_quo) ){

      data <- data %>%
        dplyr::mutate(value = 1)
      
    }else{
      
      data <- data %>% 
          dplyr::mutate(value = {{value}})
        
    }
    
    #### weighting ####
    
    if(!rlang::quo_is_null(wt_quo) ){
     
      data <- data %>%
        dplyr::mutate(value = value * {{wt}}) 
   }
    
    
    ### grouping by tabulation vars col and row
    grouped <- data %>%
      dplyr::group_by(dplyr::across(c({{cols}}, {{rows}})),
                      .drop = FALSE)
  
    ### summarizing ####
    
    summarized <- grouped %>%
        dplyr::mutate(value = 1) %>%
        dplyr::summarise(value = fun(value))

    # proportion case or percent
    if(prop|percent){
      
      mult <- ifelse(percent, 100, 1)
      if(is.null(round)){round <- ifelse(percent, 1, 3)}

      # prop is across all data
        if(rlang::quo_is_null(within_quo) ){

            summarized <- summarized %>%
              dplyr::ungroup() %>%
              dplyr::mutate(value = round(value*mult/sum(value), round))

        # prop is within categories specified by within variable
        }else{

              summarized <- summarized %>%
                dplyr::ungroup() %>%
                dplyr::group_by(dplyr::across(c({{within}})),
                                .drop = FALSE) %>%
                dplyr::mutate(value = round(value*mult/sum(value), round))

        }
    }

    arranged <- summarized

    ungrouped <- arranged %>%
      dplyr::ungroup()

    tidy <- ungrouped

    # do not pivot if argument pivot false or if no columns specified
    if(pivot == F | rlang::quo_is_null(cols_quo)){

      tidy
      # tidy %>%
      #   dplyr::rename(count = .data$value)

      # otherwise pivot by columns
    }else{

      tidy %>%
        tidyr::pivot_wider(names_from = {{cols}})

    }

  }
```

# examples/derivitive

``` r

tidy_titanic %>% pivotr(rows = sex, cols = survived, fun = length) # pivot_count
#> # A tibble: 2 × 3
#>   sex       No   Yes
#>   <fct>  <int> <int>
#> 1 Male    1364   367
#> 2 Female   126   344
flat_titanic %>% pivotr(rows = sex, value = freq, fun = mean) # pivot_calc
#> # A tibble: 2 × 2
#>   sex    value
#>   <fct>  <dbl>
#> 1 Male       1
#> 2 Female     1
flat_titanic %>% pivotr(rows = sex, value = freq, fun = sum) # pivot_count (weighted sum)
#> # A tibble: 2 × 2
#>   sex    value
#>   <fct>  <dbl>
#> 1 Male      16
#> 2 Female    16
nar <- function(x) return(NA)
flat_titanic %>% pivotr(rows = sex, cols = survived, fun = nar); #pivot_null
#> # A tibble: 2 × 3
#>   sex    No    Yes  
#>   <fct>  <lgl> <lgl>
#> 1 Male   NA    NA   
#> 2 Female NA    NA
sample1 <- function(x) sample(x, 1)
flat_titanic %>% pivotr(rows = sex, cols = survived, fun = sample1, value = freq); #pivot_sam
#> # A tibble: 2 × 3
#>   sex       No   Yes
#>   <fct>  <dbl> <dbl>
#> 1 Male       1     1
#> 2 Female     1     1
samplen <- function(x, n) paste(sample(x, 5, replace = F), collapse = ", ")
# flat_titanic %>% pivotr(rows = sex, cols = survived, fun = samplen, value = freq); #pivot_s
paste_collapse <- function(x) paste (x, collapse = ", ")
flat_titanic %>% pivotr(rows = sex, fun = paste_collapse, value = freq) # pivot_list
#> # A tibble: 2 × 2
#>   sex    value                                         
#>   <fct>  <chr>                                         
#> 1 Male   1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
#> 2 Female 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
flat_titanic %>% pivotr(rows = sex, value = freq, prop = TRUE) # pivot_prop
#> # A tibble: 2 × 2
#>   sex    value
#>   <fct>  <dbl>
#> 1 Male     0.5
#> 2 Female   0.5
flat_titanic %>% pivotr(rows = sex, cols = survived, value = freq, prop = TRUE)
#> # A tibble: 2 × 3
#>   sex       No   Yes
#>   <fct>  <dbl> <dbl>
#> 1 Male    0.25  0.25
#> 2 Female  0.25  0.25
flat_titanic %>% pivotr(rows = sex, cols = survived, value = freq, prop = TRUE, within = sex)
#> # A tibble: 2 × 3
#>   sex       No   Yes
#>   <fct>  <dbl> <dbl>
#> 1 Male     0.5   0.5
#> 2 Female   0.5   0.5
```

-----

``` r
# library(R6)

# library(tidytitanic)
# pivot_helper(data = passengers, rows = sex, cols = survived)

Tidypivot <- R6::R6Class("Tidypivot",
                     public = list(

                       # objects
                       data = NULL,
                       rows = NULL,
                       cols = NULL,
                       fun = NULL,
                       value = NULL,
                       wt = NULL,
                       within = NULL,
                       withinfun = NULL,
                       pivot = NULL,
                       wrap = NULL,
                       out = NULL
                       ,

                       # functions
                       update = function(data = NULL, rows = NULL, cols = NULL,
                                         fun = NULL, value = NULL, wt = NULL, pivot_logical = NULL){ # a method

                         # updating

                         if(!is.null(data)){self$data <- data}
                         if(!is.null(rows)){self$rows <- rows}
                         if(!is.null(cols)){self$cols <- cols}
                         if(!is.null(fun)){self$fun <- fun}
                         if(!is.null(wt)){self$wt <- wt}
                         if(!is.null(value)){self$value <- value}
                         if(!is.null(pivot_logical)){self$pivot <- pivot_logical}

                         # displaying
                         self$out <- 'tidypivot::pivot_helper(data, rows, cols, fun, value, wt, pivot_logical)'

                         if(!is.null(self$data)) {self$out <- self$out %>% stringr::str_replace("data",  self$data)  }
                         if(!is.null(self$rows)) {self$out <- self$out %>% stringr::str_replace("rows",  self$rows)  }
                         if(!is.null(self$cols)) {self$out <- self$out %>% stringr::str_replace("cols",  self$cols)  }
                         if(!is.null(self$fun))  {self$out <- self$out %>% stringr::str_replace("fun",   self$fun)   }
                         if(!is.null(self$value)){self$out <- self$out %>% stringr::str_replace("value", self$value) }
                         if(!is.null(self$wt))   {self$out <- self$out %>% stringr::str_replace("wt",    self$wt)    }
                         if(!is.null(self$pivot)){self$out <- self$out %>% stringr::str_replace("pivot_logical", self$pivot) }

                         if(is.null(self$data)) {self$out <- self$out %>% stringr::str_replace("data",  "NULL")}
                         if(is.null(self$rows)) {self$out <- self$out %>% stringr::str_replace("rows",  "NULL")}
                         if(is.null(self$cols)) {self$out <- self$out %>% stringr::str_replace("cols",  "NULL")}
                         if(is.null(self$fun))  {self$out <- self$out %>% stringr::str_replace("fun",   "NULL")}
                         if(is.null(self$value)){self$out <- self$out %>% stringr::str_replace("value", "NULL")}
                         if(is.null(self$wt))   {self$out <- self$out %>% stringr::str_replace("wt",    "NULL")}
                         if(is.null(self$pivot)){self$out <- self$out %>% stringr::str_replace("pivot_logical", "NULL")}

                         invisible(self)          #returns

                       },

                       print = function() {  # print method; default is to print everything

                         # str(self)
                         self$out

                       }
                     )
)


#' Title
#'
#' @param data
#' @param rows
#' @param cols
#' @param fun
#' @param value
#' @param wt
#' @param pivot_logical
#'
#' @return
#' @export
#'
#' @examples
#' tp_init(data = "tidytitanic::tidy_titanic")
tp_init <- function(data = NULL, quietly = FALSE){

  if(exists("my_tp")){rm(my_tp)}

  # my_tp <- Tidypivot$new()
  my_tp <<- Tidypivot$new()


  my_tp$update(data)

  if(!quietly){
  eval(parse(text =  my_tp$out))
  }

}


tp_init_pipe <- function(data = NULL, quietly = FALSE){

  # if(exists("my_tp")){rm(my_tp)}

  my_tp_pipe <- Tidypivot$new()
  # my_tp <<- Tidypivot$new()


  my_tp_pipe$update(data)

  if(!quietly){
    eval(parse(text =  my_tp_pipe$out))
  }

}
#
#
#' Title
#'
#' @param nothing
#' @param data
#' @param rows
#' @param cols
#' @param fun
#' @param value
#' @param wt
#' @param pivot_logical
#'
#' @return
#' @export
#'
#' @examples
#'tp_init(data = "tidytitanic::tidy_titanic")
#'   tp_add(rows = "sex")
#'   tp_add(cols = "class")
#'
#'tp_init(data = "tidytitanic::tidy_titanic") |>
#'   tp_add(rows = "sex") |>
#'   tp_add(cols = "class")
#'
tp_add <- function(nothing = NULL, data = NULL, rows = NULL, cols = NULL,
                                   fun = NULL, value = NULL, wt = NULL,
                   pivot_logical = NULL, quietly = FALSE){



  my_tp$update(data,
               rows,
               cols,
               fun,
               value,
               wt,
               pivot_logical)

  if(!quietly){
  eval(parse(text =  my_tp$out))
  }

}


tp_add_pipe <- function(nothing = NULL, data = NULL, rows = NULL, cols = NULL,
                   fun = NULL, value = NULL, wt = NULL,
                   pivot_logical = NULL, quietly = FALSE){



  my_tp_pipe$update(data,
               rows,
               cols,
               fun,
               value,
               wt,
               pivot_logical)

  if(!quietly){
    eval(parse(text =  my_tp_pipe$out))
  }

}

#
#
#' Title
#'
#' @param vars
#'
#' @return
#' @export
#'
#' @examples
#'   tp_init(data = "tidytitanic::tidy_titanic")
#'
#' set_data(data = tidytitanic::tidy_titanic) %>%
#'  set_rows(age)
set_rows <- function(first, vars){

  var_names = deparse(substitute(vars))

  var_names
  tp_add(rows = deparse(substitute(vars)))

}


#
#
#' Title
#'
#' @param vars
#'
#' @return
#' @export
#'
#' @examples
#' tp_init(data = "tidytitanic::tidy_titanic")
#'
#' set_data(data = tidytitanic::tidy_titanic) |>
#' set_rows(age) |>
#' set_cols(c(survived, sex))
set_cols <- function(first, vars){

  var_names = deparse(substitute(vars))

  var_names
  tp_add(cols = var_names)

}


#' Title
#'
#' @param vars
#'
#' @return
#' @export
#'
#' @examples
#' set_data(tidytitanic::tidy_titanic)
#'
set_data <- function(data){

  my_tp <<- Tidypivot$new()
  return(my_tp)

  my_tp$update(data = data,
               rows,
               cols,
               fun,
               value,
               wt,
               pivot_logical)


  eval(parse(text =  my_tp$out))


  # tp_add(data = deparse(substitute(data)))

}
```

# Piping

``` r
tp_init(data = "tidytitanic::tidy_titanic")
#> # A tibble: 1 × 1
#>   value
#>   <dbl>
#> 1  2201

set_data(tidytitanic::tidy_titanic) |> 
   set_rows(sex) |>
   set_cols(survived) |>
   set_cols(c(survived, age))
#> # A tibble: 1 × 4
#>   No_Child No_Adult Yes_Child Yes_Adult
#>      <dbl>    <dbl>     <dbl>     <dbl>
#> 1       52     1438        57       654
```

``` r
knitr::knit_exit()
```
