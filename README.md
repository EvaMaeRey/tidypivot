
- [{tidypivot} allows you to create tables by describing them (like
  ggplot2 plot
  description/declaration)](#tidypivot-allows-you-to-create-tables-by-describing-them-like-ggplot2-plot-descriptiondeclaration)
- [declarative table creation with
  ggplot2](#declarative-table-creation-with-ggplot2)
  - [Status quo table creation: Harder than it should
    be?](#status-quo-table-creation-harder-than-it-should-be)
    - [pivotr function: toward declarative table
      generation](#pivotr-function-toward-declarative-table-generation)
- [examples/derivative](#examplesderivative)
- [filling cells with examples from
  data.](#filling-cells-with-examples-from-data)
  - [proportions helpers](#proportions-helpers)
- [toward a piped workflow](#toward-a-piped-workflow)

<!-- README.md is generated from README.Rmd. Please edit that file -->

# {tidypivot} allows you to create tables by describing them (like ggplot2 plot description/declaration)

note: see original discussion here:
<https://evamaerey.github.io/mytidytuesday/2022-02-14-tables/tables.html>
and thoughtful contributions from @shannonpileggi and @brshallow
<https://github.com/EvaMaeRey/mytidytuesday/issues/3>

> And, you know, I’d get a dataset. And, *in my head I could very
> clearly kind of picture*, I want to put this on the x-axis. Let’s put
> this on the y-axis, draw a line, put some points here, break it up by
> this variable. And then, like, getting that vision out of my head, and
> into reality, it’s just really, really hard. Just, like, felt harder
> than it should be. Like, there’s a lot of custom programming involved,
> where I just felt, like, to me, I just wanted to say, like, you know,
> *this is what I’m thinking, this is how I’m picturing this plot. Like
> you’re the computer ‘Go and do it’.* … and I’d also been reading about
> the Grammar of Graphics by Leland Wilkinson, I got to meet him a
> couple of times and … I was, like, this book has been, like, written
> for me. -
> <https://www.trifacta.com/podcast/tidy-data-with-hadley-wickham/>

# declarative table creation with ggplot2

``` r
library(ggplot2)
StatSum$default_aes <- aes(label = after_stat(n))

# I want to put this on the x-axis (cols)
tidytitanic::tidy_titanic |>
  ggplot(
      # I want to put this on the x-axis (cols)
  aes(x = sex, 
      # I want to put this on the y- axis (rows)
      y = survived)
  )
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />

``` r

# grouping and computation happen in one step, filling in 'table'
last_plot() + 
  stat_sum(geom = "text")
```

<img src="man/figures/README-unnamed-chunk-2-2.png" width="100%" />

## Status quo table creation: Harder than it should be?

- 1.  grouping
- 2.  compute
- 3.  pivot

### pivotr function: toward declarative table generation

Under the hood:

- group by rows and columns
- value in data to consider (1 if not specified)
- wt, weight the value (1 if not specified)
- fun - do an operation (on value) within group

But API:

- describe layout of table and compute

``` r
#' Title
#'
#' @param data 
#' @param rows 
#' @param cols 
#' @param value 
#' @param wt 
#' @param fun 
#' @param prop 
#' @param percent 
#' @param round 
#' @param within 
#' @param withinfun 
#' @param pivot 
#' @param wrap 
#' @param totals_within 
#'
#' @return
#' @export
#'
#' @examples
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

      data <- data |>
        dplyr::mutate(value = 1)
      
    }else{
      
      data <- data |> 
          dplyr::mutate(value = {{value}})
        
    }
    
    #### weighting ####
    
    if(!rlang::quo_is_null(wt_quo) ){
     
      data <- data |>
        dplyr::mutate(value = value * {{wt}}) 
   }
    
    
    ### grouping by tabulation vars col and row
    grouped <- data |>
      dplyr::group_by(dplyr::across(c({{cols}}, {{rows}})),
                      .drop = FALSE)
  
    ### summarizing ####
    
    summarized <- grouped |>
        dplyr::summarise(value = fun(value))

    # proportion case or percent
    if(prop|percent){
      
      mult <- ifelse(percent, 100, 1)
      if(is.null(round)){round <- ifelse(percent, 1, 3)}

      # prop is across all data
        if(rlang::quo_is_null(within_quo) ){

            summarized <- summarized |>
              dplyr::ungroup() |>
              dplyr::mutate(value = round(value*mult/sum(value), round))

        # prop is within categories specified by within variable
        }else{

              summarized <- summarized |>
                dplyr::ungroup() |>
                dplyr::group_by(dplyr::across(c({{within}})),
                                .drop = FALSE) |>
                dplyr::mutate(value = round(value*mult/sum(value), round))

        }
    }

    arranged <- summarized

    ungrouped <- arranged |>
      dplyr::ungroup()

    tidy <- ungrouped

    # do not pivot if argument pivot false or if no columns specified
    if(pivot == F | rlang::quo_is_null(cols_quo)){

      tidy
      # tidy |>
      #   dplyr::rename(count = .data$value)

      # otherwise pivot by columns
    }else{

      tidy |>
        tidyr::pivot_wider(names_from = {{cols}})

    }

  }
```

``` r
# data_slice <- function(data, filter = NULL){
#   
#   value_quo <- rlang::enquo(value)
#   
#   if()
#   data %>% 
#     filter(filter)
#   
# }

data_define_value <- function(data, value = NULL, wt = NULL){
  
    value_quo <- rlang::enquo(value)
    wt_quo    <- rlang::enquo(wt)

      if(rlang::quo_is_null(value_quo) ){

      ## adding a value as 1 if there is none
      data <- data |>
        dplyr::mutate(value = 1)
      
    }else{
      
      data <- data |> 
          dplyr::mutate(value = {{value}})
        
    }
    
    #### weighting ####
    
    if(!rlang::quo_is_null(wt_quo) ){
     
      data <- data |>
        dplyr::mutate(value = .data$value * {{wt}}) 
   }
    
    data
  
}


data_to_grouped <- function(data, cols, rows){
  
    ### grouping by tabulation vars col and row
    data |>
      dplyr::group_by(dplyr::across(c({{cols}}, {{rows}})),
                      .drop = FALSE)
  
  
}


data_grouped_to_summarized <- function(data, fun = NULL){
  
      if(is.null(fun))  {fun <- sum}

    ## adding a value as 1 if there is none
  
    ### summarizing ####
    
    data |>
        dplyr::summarise(value = fun(.data$value))
  
  
}


data_summarized_to_proportioned <- function(data, prop = F, percent = F, within = NULL, round = 2){ 
    # proportion case or percent
  
    within_quo        <- rlang::enquo(within)
    # totals_within_quo <- rlang::enquo(totals_within)

    if(is.null(prop)) {prop <- FALSE}

    if(prop|percent){
      
      mult <- ifelse(percent, 100, 1)
      if(is.null(round)){round <- ifelse(percent, 1, 3)}

      # prop is across all data
        if(rlang::quo_is_null(within_quo) ){

            data <- data |>
              dplyr::ungroup() |>
              dplyr::mutate(value = round(.data$value*mult/sum(.data$value), round))

        # prop is within categories specified by within variable
        }else{

              data <- data |>
                dplyr::ungroup() |>
                dplyr::group_by(dplyr::across(c({{within}})),
                                .drop = FALSE) |>
                dplyr::mutate(value = round(.data$value*mult/sum(.data$value), round))

        }
    }
  
  data

}


data_proportioned_to_pivoted <- function(data, pivot = T, cols = NULL){
  
    cols_quo  <- rlang::enquo(cols)
    if(is.null(pivot)){pivot <- TRUE}

    tidy <- data |>
      dplyr::ungroup()

    # do not pivot if argument pivot false or if no columns specified
    if(pivot == F | rlang::quo_is_null(cols_quo)){

      tidy

      # otherwise pivot by columns
    }else{

      tidy |>
        tidyr::pivot_wider(names_from = {{cols}})

    }

  }
```

``` r
tidytitanic::flat_titanic |> 
  data_define_value(value = freq) |> 
  data_to_grouped(rows = survived, cols = sex) |>
  data_grouped_to_summarized() |>
  data_summarized_to_proportioned(percent = T, within = survived) |>
  data_proportioned_to_pivoted(cols = sex)
#> # A tibble: 2 × 3
#>   survived  Male Female
#>   <fct>    <dbl>  <dbl>
#> 1 No        91.5   8.46
#> 2 Yes       51.6  48.4
```

``` r
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

                   pivot = NULL
){

  
  data |> 
  data_define_value(value = {{value}}, wt = {{wt}}) |> 
  data_to_grouped(rows = {{rows}}, cols = {{cols}}) |>
  data_grouped_to_summarized(fun = fun) |>
  data_summarized_to_proportioned(prop = prop, percent = percent, within = {{within}}, round = round) |>
  data_proportioned_to_pivoted(pivot = pivot, cols = {{cols}})
  
}
```

``` r

tidytitanic::flat_titanic |> 
  pivotr(value = freq, rows = survived, cols = sex, percent = T, within = survived)
#> # A tibble: 2 × 3
#>   survived  Male Female
#>   <fct>    <dbl>  <dbl>
#> 1 No        91.5    8.5
#> 2 Yes       51.6   48.4
```

``` r
library(tidytitanic)

tidy_titanic |> pivotr()
#> # A tibble: 1 × 1
#>   value
#>   <dbl>
#> 1  2201
```

``` r

tidy_titanic |> pivotr(rows = sex, cols = survived)
#> # A tibble: 2 × 3
#>   sex       No   Yes
#>   <fct>  <dbl> <dbl>
#> 1 Male    1364   367
#> 2 Female   126   344
```

``` r

tidy_titanic |> pivotr(rows = c(sex, age), cols = survived)
#> # A tibble: 4 × 4
#>   sex    age      No   Yes
#>   <fct>  <fct> <dbl> <dbl>
#> 1 Male   Child    35    29
#> 2 Male   Adult  1329   338
#> 3 Female Child    17    28
#> 4 Female Adult   109   316
```

``` r

tidy_titanic |> pivotr(rows = sex, cols = survived, pivot = F)
#> # A tibble: 4 × 3
#>   survived sex    value
#>   <fct>    <fct>  <dbl>
#> 1 No       Male    1364
#> 2 No       Female   126
#> 3 Yes      Male     367
#> 4 Yes      Female   344
```

``` r

flat_titanic |> pivotr(rows = sex, value = freq, prop = TRUE)
#> # A tibble: 2 × 2
#>   sex    value
#>   <fct>  <dbl>
#> 1 Male   0.786
#> 2 Female 0.214
```

``` r

flat_titanic |> pivotr(rows = sex, cols = survived, value = freq, prop = TRUE)
#> # A tibble: 2 × 3
#>   sex       No   Yes
#>   <fct>  <dbl> <dbl>
#> 1 Male   0.62  0.167
#> 2 Female 0.057 0.156
```

``` r

flat_titanic |> pivotr(rows = sex, cols = survived, value = freq, prop = TRUE, within = sex)
#> # A tibble: 2 × 3
#>   sex       No   Yes
#>   <fct>  <dbl> <dbl>
#> 1 Male   0.788 0.212
#> 2 Female 0.268 0.732
```

# examples/derivative

Here are some examples where you might have derivative functions

``` r
pivot_count <- function(...){

  # maybe a wt version...
  pivotr(fun = length, ...) 

  }

pivot_average <- function(...){
  
  mean_na_rm <- function(x){mean(x, na.rm = T)}
  
  pivotr(fun = mean_na_rm, ...) 
  
}

pivot_sum <- function(...){
  
  pivotr(fun = sum, ...)
  
}

pivot_empty <- function(...){
  
nar <- function(x) return(NA)

  pivotr(fun = nar, ...)

}
```

``` r
library(magrittr)
library(tidytitanic)

passengers <- readr::read_csv("https://raw.githubusercontent.com/clauswilke/dviz.supp/master/data-raw/titanic/Titanic.csv")

head(passengers)
#> # A tibble: 6 × 7
#>    ...1 Name                                 PClass   Age Sex   Survived SexCode
#>   <dbl> <chr>                                <chr>  <dbl> <chr>    <dbl>   <dbl>
#> 1     1 Allen, Miss Elisabeth Walton         1st    29    fema…        1       1
#> 2     2 Allison, Miss Helen Loraine          1st     2    fema…        0       1
#> 3     3 Allison, Mr Hudson Joshua Creighton  1st    30    male         0       0
#> 4     4 Allison, Mrs Hudson JC (Bessie Wald… 1st    25    fema…        0       1
#> 5     5 Allison, Master Hudson Trevor        1st     0.92 male         1       0
#> 6     6 Anderson, Mr Harry                   1st    47    male         1       0
```

``` r

tidy_titanic |> pivot_count(rows = sex)
#> # A tibble: 2 × 2
#>   sex    value
#>   <fct>  <int>
#> 1 Male    1731
#> 2 Female   470
```

``` r
tidy_titanic |> pivot_count(rows = sex, col = survived)
#> # A tibble: 2 × 3
#>   sex       No   Yes
#>   <fct>  <int> <int>
#> 1 Male    1364   367
#> 2 Female   126   344
```

``` r
flat_titanic |> pivot_sum(rows = survived, value = freq)
#> # A tibble: 2 × 2
#>   survived value
#>   <fct>    <dbl>
#> 1 No        1490
#> 2 Yes        711
```

``` r
flat_titanic |> pivot_sum(rows = sex,  cols = survived, value = freq)
#> # A tibble: 2 × 3
#>   sex       No   Yes
#>   <fct>  <dbl> <dbl>
#> 1 Male    1364   367
#> 2 Female   126   344
```

``` r

flat_titanic |> pivot_average(rows = sex,  cols = survived, value = freq)
#> # A tibble: 2 × 3
#>   sex       No   Yes
#>   <fct>  <dbl> <dbl>
#> 1 Male   170.   45.9
#> 2 Female  15.8  43
```

``` r
flat_titanic |> pivot_empty(rows = survived, cols = age)
#> # A tibble: 2 × 3
#>   survived Child Adult
#>   <fct>    <lgl> <lgl>
#> 1 No       NA    NA   
#> 2 Yes      NA    NA
```

``` r

passengers |> pivot_average(rows = c(Sex, PClass), cols = Survived, value = Age)
#> # A tibble: 7 × 4
#>   Sex    PClass   `0`   `1`
#>   <chr>  <chr>  <dbl> <dbl>
#> 1 female 1st     35.2  37.9
#> 2 female 2nd     31.4  26.9
#> 3 female 3rd     22.8  22.7
#> 4 male   *      NaN    NA  
#> 5 male   1st     44.8  34.3
#> 6 male   2nd     31.7  14.8
#> 7 male   3rd     27.1  22.1
```

# filling cells with examples from data.

``` r
pivot_example <- function(...){

  sample1 <- function(x) sample(x, 1)
  pivotr(fun = sample1, ...)

}


pivot_samplen <- function(..., n = 3, sep = "; "){

  samplen <- function(x) paste(sample(x, n, replace = F), collapse = sep)

  pivotr(fun = samplen, ...) 

}

pivot_list <- function(..., sep = "; "){

  paste_collapse <- function(x) paste (x, collapse = sep)
  pivotr(fun = paste_collapse, ...) 
  
}
```

``` r
flat_titanic |> pivot_example(rows = sex, value = freq)
#> # A tibble: 2 × 2
#>   sex    value
#>   <fct>  <dbl>
#> 1 Male     387
#> 2 Female     3
```

``` r

flat_titanic |> pivot_samplen(rows = sex, value = freq)
#> # A tibble: 2 × 2
#>   sex    value    
#>   <fct>  <chr>    
#> 1 Male   5; 35; 57
#> 2 Female 17; 0; 20
```

``` r

flat_titanic |> pivot_list(rows = sex, cols = survived, value = freq)
#> # A tibble: 2 × 3
#>   sex    No                              Yes                          
#>   <fct>  <chr>                           <chr>                        
#> 1 Male   0; 0; 35; 0; 118; 154; 387; 670 5; 11; 13; 0; 57; 14; 75; 192
#> 2 Female 0; 0; 17; 0; 4; 13; 89; 3       1; 13; 14; 0; 140; 80; 76; 20
```

``` r

set.seed(12345)
passengers |> pivot_example(rows = Survived, cols = Sex, value = Name)
#> # A tibble: 2 × 3
#>   Survived female                     male                          
#>      <dbl> <chr>                      <chr>                         
#> 1        0 Solvang, Mrs Lena Jacobsen Meyer, Mr August              
#> 2        1 Gibson, Miss Dorothy       Williams, Mr Richard Norris II
```

``` r
passengers |> pivot_samplen(rows = Survived, cols = Sex, value = Name, n = 2, sep = "; ") 
#> # A tibble: 2 × 3
#>   Survived female                                                          male 
#>      <dbl> <chr>                                                           <chr>
#> 1        0 McGowan, Miss Katherine; Klasen, Miss Gertrud Emilia            Smar…
#> 2        1 Ware, Mrs John James (Florence Louise Long); Dyker, Mrs Adolf … Mock…
```

``` r

passengers |> pivot_samplen(rows = Survived, cols = Sex, value = Age, n = 7) 
#> # A tibble: 2 × 3
#>   Survived female                    male                      
#>      <dbl> <chr>                     <chr>                     
#> 1        0 NA; 44; 20; NA; 18; 2; NA NA; NA; 28; NA; 19; NA; NA
#> 2        1 22; 5; 59; 12; 13; 26; NA 32; 9; 35; 60; NA; NA; NA
```

``` r

passengers |> dplyr::sample_n(20) |> pivot_list(rows = Sex, cols = Survived, value = Age)
#> # A tibble: 2 × 3
#>   Sex    `0`                                                `1`       
#>   <chr>  <chr>                                              <chr>     
#> 1 female NA; 30                                             NA; 45; 22
#> 2 male   NA; 24; 26; 29; 21; 29; 19; 46; 54; NA; 21; 22; NA 19; 2
```

## proportions helpers

``` r
library(tidytitanic)
# pivot_prop
flat_titanic |> pivotr(rows = sex, 
                       value = freq, 
                       prop = TRUE) # pivot_prop
#> # A tibble: 2 × 2
#>   sex    value
#>   <fct>  <dbl>
#> 1 Male   0.786
#> 2 Female 0.214
```

``` r

flat_titanic |> 
  pivotr(rows = sex, cols = survived, 
         value = freq, prop = TRUE)
#> # A tibble: 2 × 3
#>   sex       No   Yes
#>   <fct>  <dbl> <dbl>
#> 1 Male   0.62  0.167
#> 2 Female 0.057 0.156
```

``` r

flat_titanic |> 
  pivotr(rows = sex, cols = survived, 
         value = freq, prop = TRUE, within = sex)
#> # A tibble: 2 × 3
#>   sex       No   Yes
#>   <fct>  <dbl> <dbl>
#> 1 Male   0.788 0.212
#> 2 Female 0.268 0.732
```

``` r

# pivot_percent
flat_titanic |> 
  pivotr(rows = sex, cols = survived, 
         value = freq, percent = TRUE, within = sex)
#> # A tibble: 2 × 3
#>   sex       No   Yes
#>   <fct>  <dbl> <dbl>
#> 1 Male    78.8  21.2
#> 2 Female  26.8  73.2
```

# toward a piped workflow

<https://evamaerey.github.io/mytidytuesday/2024-07-02-s3-tables/s3-tables-tidypivot.html>

``` r
new_tidypivot <- function(data = data.frame(),
                          rows = NULL,
                          columns = NULL,
                          value = NULL,
                          wt = NULL) {

  # table specification components !
  tp <- list(
    data = data,
    rows = rows,
    columns = columns,
    value = value,
    wt = wt
    # more 'slots' to be added
  )

  # declare class 'tidypivot'
  class(tp) <- "tidypivot"

  # Return the created object
  invisible(tp)

}


return_specified_table = function(tp){

      out <- 'tidypivot::pivot_helper(thedata, rows, cols, value, wt, fun)'

      str_replace_or_null <- function(x, pattern, replacement){
        
        if(is.null(replacement)){x |> str_replace(pattern, 'NULL')}
        else{x |> str_replace(pattern, replacement)}
        
      }
      
      out <- str_replace_or_null(out, "rows",  tp$rows)
      out <- str_replace_or_null(out, "cols",  tp$cols)
      out <- str_replace_or_null(out, "value",  tp$value)
      out <- str_replace_or_null(out, "wt",  tp$wt)
      out <- str_replace_or_null(out, "fun",  tp$fun)

      
      eval(parse(text = out))         
      
}


print.tidypivot <- function(tp){
  
  print(return_specified_table(tp))
  invisible(tp)
  
}

#' @export
ggtable <- function(data){
  
  thedata <<- data # don't love this

  tp <- new_tidypivot(deparse(substitute(thedata)))
  
  last_tp <<- tp
  
  tp

}

#' @export
set_rows <- function(tp, rows = NULL){
  
  tp$rows <- deparse(substitute(rows))
  
  last_tp <<- tp
  
  tp

  
}

#' @export
set_cols <- function(tp, cols = NULL){
  
tp$cols <- deparse(substitute(cols))

  tp
  
  last_tp <<- tp
  
  tp
  
}

#' @export
set_fun <- function(tp, fun = sum){

tp$fun <- deparse(substitute(fun))

  last_tp <<- tp

  tp
  
}


#' @export
set_value <- function(tp, value = NULL){
  
tp$value <- deparse(substitute(value))

  last_tp <<- tp
  
  tp
  
}


#' @export
set_wt <- function(tp, wt = NULL){
  
tp$wt <- deparse(substitute(wt))

  last_tp <<- tp
  
  tp
  
}


#' @export
set_weight <- function(tp, weight = NULL){
  
tp$weight <- deparse(substitute(weight))

  print(tp)
  
  last_tp <<- tp
  
}


#' @export
unpivot <- function(tp){
  
  tp$pivot <- FALSE

  print(tp)
  
  last_tp <<- tp
  
  
}



#' @export
last_table <- function(){
  
  last_tp
  
}
```

``` r
# knitrExtra::chunk_names_get()

knitrExtra::chunk_to_dir("helpers")
knitrExtra::chunk_to_dir("pivotr")
knitrExtra::chunk_to_dir("piping")
```

``` r
library(tidyverse)
library(tidypivot)
ext_exports <- read_csv("https://raw.githubusercontent.com/EvaMaeRey/mytidytuesday/refs/heads/main/2024-11-19-gg-prefixes/exported_funs_exts_ggplot2_tidyverse_org.csv") %>% 
  mutate(prefix = str_extract(fun_exported, ".*?_")) %>% 
  mutate(prefix_long = str_extract(fun_exported, ".+_")) %>% 
  mutate(ind_classic_prefix = prefix %in% c("stat_", "geom_", "theme_", "scale_", "coord_", "facet_"))


ggtable(ext_exports)
#> # A tibble: 1 × 1
#>   value
#>   <dbl>
#> 1  5527
```

``` r

ggtable(ext_exports %>% filter(ind_classic_prefix))
#> # A tibble: 1 × 1
#>   value
#>   <dbl>
#> 1  1992
```

``` r

last_table() |>
  set_rows(user) 
#> # A tibble: 86 × 2
#>    user                 value
#>    <chr>                <dbl>
#>  1 AckerDWM                 2
#>  2 AllanCameron            32
#>  3 Ather-Energy             6
#>  4 IndrajeetPatil           1
#>  5 LCBC-UiO                34
#>  6 LKremer                  1
#>  7 ProjectMOSAIC            8
#>  8 PursuitOfDataScience     4
#>  9 Ryo-N7                  54
#> 10 Selbosh                  7
#> # ℹ 76 more rows
```

``` r

last_table() |>
  set_cols(prefix)
#> # A tibble: 86 × 7
#>    user          coord_ facet_ geom_ scale_ stat_ theme_
#>    <chr>          <dbl>  <dbl> <dbl>  <dbl> <dbl>  <dbl>
#>  1 AllanCameron       1     NA    24      6     1     NA
#>  2 cidm-ph            1     NA     4     NA     2     NA
#>  3 davidchall         1     NA     1     NA     1      2
#>  4 easystats          1     NA    14     78    NA      6
#>  5 hrbrmstr           1     NA    11     10     6      9
#>  6 stefanedwards      5      2     3      2    NA     NA
#>  7 teunbrand          2      5    11     47     7      1
#>  8 davidgohel        NA      2    50     94    NA     NA
#>  9 earowang          NA      1     1     NA     1     NA
#> 10 erocoar           NA      1    12     NA     5     NA
#> # ℹ 76 more rows
```

``` r

last_table() |>
  set_rows(c(user, repo))
#> # A tibble: 101 × 8
#>    user          repo         coord_ facet_ geom_ scale_ stat_ theme_
#>    <chr>         <chr>         <dbl>  <dbl> <dbl>  <dbl> <dbl>  <dbl>
#>  1 AllanCameron  geomtextpath      1     NA    24      6     1     NA
#>  2 cidm-ph       ggmapinset        1     NA     4     NA     2     NA
#>  3 davidchall    ggip              1     NA     1     NA     1      2
#>  4 easystats     see               1     NA    14     78    NA      6
#>  5 hrbrmstr      ggalt             1     NA    11     NA     6     NA
#>  6 stefanedwards lemon             5      2     3      2    NA     NA
#>  7 teunbrand     ggh4x             1      5     7      9     7     NA
#>  8 teunbrand     legendry          1     NA    NA     NA    NA      1
#>  9 davidgohel    ggiraph          NA      2    50     94    NA     NA
#> 10 earowang      sugrrants        NA      1     1     NA     1     NA
#> # ℹ 91 more rows
```

``` r


read_csv("https://raw.githubusercontent.com/EvaMaeRey/mytidytuesday/refs/heads/main/2024-12-10-ggplot2-layer-composition/ggplot2_exported_layer_fun_composition.csv") %>% 
  rename(prefix = fun_prefix) ->
ggplot2_layers_definers

ggplot2_layers_definers |>
  ggtable()
#> # A tibble: 1 × 1
#>   value
#>   <dbl>
#> 1   254
```

``` r

last_table() |>
  set_rows(type)
#> # A tibble: 3 × 2
#>   type     value
#>   <chr>    <dbl>
#> 1 geom        84
#> 2 position    85
#> 3 stat        85
```

``` r

last_table() |>
  set_rows(type) |>
  set_cols(default_or_fixed) |>
  set_rows(c(prefix, type))
#> # A tibble: 6 × 4
#>   prefix type     default fixed
#>   <chr>  <chr>      <dbl> <dbl>
#> 1 geom_  geom           2    50
#> 2 geom_  position      51     2
#> 3 geom_  stat          47     6
#> 4 stat_  geom          32    NA
#> 5 stat_  position      32    NA
#> 6 stat_  stat          NA    32
```

``` r
devtools::check()
devtools::install(pkg = ".", upgrade = "never") 
```

``` r
knitr::knit_exit()
```
