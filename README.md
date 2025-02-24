
- [{tidypivot} allows you to create tables by describing them (like
  ggplot2 plot
  description/declaration)](#tidypivot-allows-you-to-create-tables-by-describing-them-like-ggplot2-plot-descriptiondeclaration)
- [declarative table creation with
  ggplot2](#declarative-table-creation-with-ggplot2)
  - [Status quo table creation: Harder than it should
    be?](#status-quo-table-creation-harder-than-it-should-be)
    - [pivotr function: toward declarative table
      generation](#pivotr-function-toward-declarative-table-generation)
- [toward a piped workflow](#toward-a-piped-workflow)
- [examples/derivative](#examplesderivative)
- [filling cells with examples from
  data.](#filling-cells-with-examples-from-data)
- [proportions helpers](#proportions-helpers)

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

- describe layout of table (rows and cols) and compute (default to count
  records)

``` r
data_filter <- function(data, filter = TRUE){

  data <- data |>
      dplyr::filter({{filter}})
  
  data

}

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
                      .drop = TRUE)
  
  
}


data_grouped_to_summarized <- function(data, fun = NULL){
  
      if(is.null(fun))  {fun <- sum}

    ## adding a value as 1 if there is none
  
    ### summarizing ####
    
    data |>
        dplyr::summarise(summary = fun(.data$value))
  
  
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
              dplyr::mutate(prop = round(.data$summary*mult/sum(.data$summary), round))

        # prop is within categories specified by within variable
        }else{

              data <- data |>
                dplyr::ungroup() |>
                dplyr::group_by(dplyr::across(c({{within}})),
                                .drop = FALSE) |>
                dplyr::mutate(prop = round(.data$summary*mult/sum(.data$summary), round))

        }
    }
    
  if(prop|percent){data$display <- data$prop}else{data$display <- data$summary}
  
  data

}


data_proportioned_to_pivoted <- function(data, pivot = TRUE, cols = NULL){
  
    cols_quo  <- rlang::enquo(cols)

    tidy <- data |>
      dplyr::ungroup()

    # do not pivot if argument pivot false or if no columns specified
    if(pivot == F){

      tidy 

      # otherwise pivot by columns
    }else
    
    
    if(rlang::quo_is_null(cols_quo) & pivot){
      
      tidy <- tidy |> dplyr::select(-summary)
      if(!is.null(data$prop)|!is.null(data$percent)){
        tidy <- tidy |>  dplyr::select(-prop)
      }
        
      tidy |>
        dplyr::rename(value = display)
      
    } else
      
    
    if(!rlang::quo_is_null(cols_quo) & pivot){
      # keep only display column, and tabulation vars
      tidy <- tidy |> dplyr::select(-summary)
      if(!is.null(data$prop)|!is.null(data$percent)){
        tidy <- tidy |>  dplyr::select(-prop)
      }

      tidy |>
        tidyr::pivot_wider(names_from = {{cols}}, values_from = display)

    }

  }
```

``` r
tidytitanic::flat_titanic |> 
  dplyr::filter(freq > 30) |>
  data_define_value(value = freq) |> 
  data_to_grouped(rows = survived, cols = sex) |>
  data_grouped_to_summarized() |>
  data_summarized_to_proportioned(percent = T, within = survived) |>
  data_proportioned_to_pivoted(cols = sex)
#> # A tibble: 2 × 3
#>   survived  Male Female
#>   <fct>    <dbl>  <dbl>
#> 1 No        93.9   6.13
#> 2 Yes       52.3  47.7
```

``` r
pivotr <- function(data,
                   rows = NULL,
                   cols = NULL,
                   
                   value = NULL,
                   wt = NULL,
                       
                   fun = NULL,
                       
                   filter = TRUE,
                   prop = FALSE,
                   percent = FALSE,
                   round = NULL,
                       
                   within = NULL,

                   pivot = TRUE
){

  
  data |> 
  data_filter({{filter}}) |>
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

tidytitanic::flat_titanic |> 
  pivotr(value = freq, rows = survived, cols = sex, filter = sex == "Female")
#> # A tibble: 2 × 2
#>   survived Female
#>   <fct>     <dbl>
#> 1 No          126
#> 2 Yes         344
```

``` r
library(tidytitanic)

tidy_titanic |> pivotr()
#> # A tibble: 1 × 1
#>   value
#>   <dbl>
#> 1  2201

tidy_titanic |> pivotr(rows = sex, cols = survived)
#> # A tibble: 2 × 3
#>   sex       No   Yes
#>   <fct>  <dbl> <dbl>
#> 1 Male    1364   367
#> 2 Female   126   344

tidy_titanic |> pivotr(rows = c(sex, age), cols = survived)
#> # A tibble: 4 × 4
#>   sex    age      No   Yes
#>   <fct>  <fct> <dbl> <dbl>
#> 1 Male   Child    35    29
#> 2 Male   Adult  1329   338
#> 3 Female Child    17    28
#> 4 Female Adult   109   316

tidy_titanic |> pivotr(rows = sex, cols = survived, pivot = F)
#> # A tibble: 4 × 4
#>   survived sex    summary display
#>   <fct>    <fct>    <dbl>   <dbl>
#> 1 No       Male      1364    1364
#> 2 No       Female     126     126
#> 3 Yes      Male       367     367
#> 4 Yes      Female     344     344

flat_titanic |> pivotr(rows = sex, value = freq, prop = TRUE)
#> # A tibble: 2 × 2
#>   sex    value
#>   <fct>  <dbl>
#> 1 Male   0.786
#> 2 Female 0.214

flat_titanic |> pivotr(rows = sex, cols = survived, value = freq, prop = TRUE)
#> # A tibble: 2 × 3
#>   sex       No   Yes
#>   <fct>  <dbl> <dbl>
#> 1 Male   0.62  0.167
#> 2 Female 0.057 0.156

flat_titanic |> pivotr(rows = sex, cols = survived, value = freq, prop = TRUE, within = sex)
#> # A tibble: 2 × 3
#>   sex       No   Yes
#>   <fct>  <dbl> <dbl>
#> 1 Male   0.788 0.212
#> 2 Female 0.268 0.732
```

# toward a piped workflow

<https://evamaerey.github.io/mytidytuesday/2024-07-02-s3-tables/s3-tables-tidypivot.html>

``` r
new_tidypivot <- function(data = data.frame(),
                          rows = NULL,
                          cols = NULL,
                          value = NULL,
                          wt = NULL,
                          fun = NULL,
                          filter = TRUE,
                          prop = FALSE,
                          percent = FALSE,
                          round = NULL,
                          within = NULL,
                          pivot = TRUE) {

  # table specification components !
  tp <- list(
    data = data,
    rows = rows,
    cols = cols,
    value = value,
    wt = wt,
    fun = fun,
    prop = prop,
    percent = percent,
    round = round,
    within = within,
    pivot = pivot
    # more 'slots' to be added
  )

  # declare class 'tidypivot'
  class(tp) <- "tidypivot"

  # Return the created object
  invisible(tp)

}


print.tidypivot <- function(tp){
  
  print(do.call(pivotr, tp))
  
  invisible(tp)
  
}

#' @export
ggtable <- function(data = NULL){
  
  # thedata <<- data # don't love this
  data <- data %||% data.frame()
  
  tp <- new_tidypivot()
  
  tp$data <- data
  
  last_tp <<- tp
  
  tp

}


#' @export
last_table <- function(){
  
  last_tp
  
}
```

``` r
ggtable() 
#> # A tibble: 1 × 1
#>   value
#>   <dbl>
#> 1     0


tidytitanic::tidy_titanic |> head()
#>   id class  sex   age survived
#> 1  1   3rd Male Child       No
#> 2  2   3rd Male Child       No
#> 3  3   3rd Male Child       No
#> 4  4   3rd Male Child       No
#> 5  5   3rd Male Child       No
#> 6  6   3rd Male Child       No

ggtable(tidytitanic::tidy_titanic) 
#> # A tibble: 1 × 1
#>   value
#>   <dbl>
#> 1  2201


last_table()
#> # A tibble: 1 × 1
#>   value
#>   <dbl>
#> 1  2201
```

``` r
#' @export
set_rows <- function(tp, rows = NULL){
  
  tp$rows <- enquo(rows)
  
  last_tp <<- tp
  
  tp

  
}


#' @export
set_cols <- function(tp, cols = NULL){
  
  tp$cols <- enquo(cols)

  last_tp <<- tp
  
  tp
  

}

#' @export
set_filter <- function(tp, filter = TRUE){
  
  if(!filter){tp$filter <- !!rlang::enquo(filter)}
  
  last_tp <<- tp
  
  tp
  

}
```

``` r
ggtable(tidytitanic::tidy_titanic) |>
  set_rows(sex) |>
  set_cols(survived) |>
  set_filter(TRUE)
#> # A tibble: 2 × 3
#>   sex       No   Yes
#>   <fct>  <dbl> <dbl>
#> 1 Male    1364   367
#> 2 Female   126   344

# last_table |>
#   set_filter(sex == "Female")
```

``` r
#' @export
set_fun <- function(tp, fun = sum){

  tp$fun <- fun

  last_tp <<- tp

  tp
  
}


#' @export
set_value <- function(tp, value = NULL){
  
  tp$value <- enquo(value)

  last_tp <<- tp
  
  tp
  
}


#' @export
set_wt <- function(tp, wt = NULL){
  
  tp$wt <- enquo(wt)

  last_tp <<- tp
  
  tp
  
}


#' @export
set_weight <- function(tp, weight = NULL){
  
  tp$weight <- enquo(weight)
  
  last_tp <<- tp
  
  tp
  
}


#' @export
set_prop <- function(tp, within = NULL){
  
  tp$percent <- FALSE
  tp$prop <- TRUE
  tp$within <- enquo(within)
  
  last_tp <<- tp
  
  tp
  
}

#' @export
set_percent <- function(tp, within = NULL){
  
  tp$prop <- FALSE
  tp$percent <- TRUE
  tp$within <- enquo(within)
  
  last_tp <<- tp
  
  tp
  
}


#' @export
set_within <- function(tp, within = NULL){
  
  tp$within <- enquo(within)
  
  last_tp <<- tp
  
  tp
  
}

#' @export
no_pivot <- function(tp){
  
  tp$pivot <- FALSE

  last_tp <<- tp

  tp  
  
}

collect <- function(tp){
  
  do.call(pivotr, tp)
  
}
```

``` r
tidytitanic::flat_titanic 
#>    class    sex   age survived freq
#> 1    1st   Male Child       No    0
#> 2    2nd   Male Child       No    0
#> 3    3rd   Male Child       No   35
#> 4   Crew   Male Child       No    0
#> 5    1st Female Child       No    0
#> 6    2nd Female Child       No    0
#> 7    3rd Female Child       No   17
#> 8   Crew Female Child       No    0
#> 9    1st   Male Adult       No  118
#> 10   2nd   Male Adult       No  154
#> 11   3rd   Male Adult       No  387
#> 12  Crew   Male Adult       No  670
#> 13   1st Female Adult       No    4
#> 14   2nd Female Adult       No   13
#> 15   3rd Female Adult       No   89
#> 16  Crew Female Adult       No    3
#> 17   1st   Male Child      Yes    5
#> 18   2nd   Male Child      Yes   11
#> 19   3rd   Male Child      Yes   13
#> 20  Crew   Male Child      Yes    0
#> 21   1st Female Child      Yes    1
#> 22   2nd Female Child      Yes   13
#> 23   3rd Female Child      Yes   14
#> 24  Crew Female Child      Yes    0
#> 25   1st   Male Adult      Yes   57
#> 26   2nd   Male Adult      Yes   14
#> 27   3rd   Male Adult      Yes   75
#> 28  Crew   Male Adult      Yes  192
#> 29   1st Female Adult      Yes  140
#> 30   2nd Female Adult      Yes   80
#> 31   3rd Female Adult      Yes   76
#> 32  Crew Female Adult      Yes   20

ggtable(tidytitanic::flat_titanic) |>
  set_value(freq) |>
  set_rows(sex) |>
  set_cols(survived)
#> # A tibble: 2 × 3
#>   sex       No   Yes
#>   <fct>  <dbl> <dbl>
#> 1 Male    1364   367
#> 2 Female   126   344

last_table() |>
  set_percent()
#> # A tibble: 2 × 3
#>   sex       No   Yes
#>   <fct>  <dbl> <dbl>
#> 1 Male    62    16.7
#> 2 Female   5.7  15.6

last_table() |>
  set_prop()
#> # A tibble: 2 × 3
#>   sex       No   Yes
#>   <fct>  <dbl> <dbl>
#> 1 Male   0.62  0.167
#> 2 Female 0.057 0.156

last_table() |>
  set_prop(within = sex)
#> # A tibble: 2 × 3
#>   sex       No   Yes
#>   <fct>  <dbl> <dbl>
#> 1 Male   0.788 0.212
#> 2 Female 0.268 0.732

# a null table...
ggtable(tidytitanic::flat_titanic) |>
  set_value(NA) |>
  set_rows(sex)
#> # A tibble: 2 × 2
#>   sex    value
#>   <fct>  <int>
#> 1 Male      NA
#> 2 Female    NA

ggtable(tidytitanic::flat_titanic) |>
  set_wt(freq) |>
  set_rows(sex) |>
  set_cols(survived)
#> # A tibble: 2 × 3
#>   sex       No   Yes
#>   <fct>  <dbl> <dbl>
#> 1 Male    1364   367
#> 2 Female   126   344

last_table() |>
  set_fun(mean)
#> # A tibble: 2 × 3
#>   sex       No   Yes
#>   <fct>  <dbl> <dbl>
#> 1 Male   170.   45.9
#> 2 Female  15.8  43

last_table() |> 
  no_pivot()
#> # A tibble: 4 × 4
#>   survived sex    summary display
#>   <fct>    <fct>    <dbl>   <dbl>
#> 1 No       Male     170.    170. 
#> 2 No       Female    15.8    15.8
#> 3 Yes      Male      45.9    45.9
#> 4 Yes      Female    43      43


last_table() |>
  collect()
#> # A tibble: 4 × 4
#>   survived sex    summary display
#>   <fct>    <fct>    <dbl>   <dbl>
#> 1 No       Male     170.    170. 
#> 2 No       Female    15.8    15.8
#> 3 Yes      Male      45.9    45.9
#> 4 Yes      Female    43      43
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

ggtable(ext_exports |> filter(ind_classic_prefix))
#> # A tibble: 1 × 1
#>   value
#>   <dbl>
#> 1  1992

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

# last_table() |>
#   set_rows(c(user, repo))


read_csv("https://raw.githubusercontent.com/EvaMaeRey/mytidytuesday/refs/heads/main/2024-12-10-ggplot2-layer-composition/ggplot2_exported_layer_fun_composition.csv") %>% 
  rename(prefix = fun_prefix) ->
ggplot2_layers_definers

ggplot2_layers_definers |>
  ggtable()
#> # A tibble: 1 × 1
#>   value
#>   <dbl>
#> 1   254

last_table() |>
  set_rows(type)
#> # A tibble: 3 × 2
#>   type     value
#>   <chr>    <dbl>
#> 1 geom        84
#> 2 position    85
#> 3 stat        85

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

tidy_titanic |> pivot_count(rows = sex)
#> # A tibble: 2 × 2
#>   sex    value
#>   <fct>  <int>
#> 1 Male    1731
#> 2 Female   470
tidy_titanic |> pivot_count(rows = sex, col = survived)
#> # A tibble: 2 × 3
#>   sex       No   Yes
#>   <fct>  <int> <int>
#> 1 Male    1364   367
#> 2 Female   126   344
flat_titanic |> pivot_sum(rows = survived, value = freq)
#> # A tibble: 2 × 2
#>   survived value
#>   <fct>    <dbl>
#> 1 No        1490
#> 2 Yes        711
flat_titanic |> pivot_sum(rows = sex,  cols = survived, value = freq)
#> # A tibble: 2 × 3
#>   sex       No   Yes
#>   <fct>  <dbl> <dbl>
#> 1 Male    1364   367
#> 2 Female   126   344

flat_titanic |> pivot_average(rows = sex,  cols = survived, value = freq)
#> # A tibble: 2 × 3
#>   sex       No   Yes
#>   <fct>  <dbl> <dbl>
#> 1 Male   170.   45.9
#> 2 Female  15.8  43
flat_titanic |> pivot_empty(rows = survived, cols = age)
#> # A tibble: 2 × 3
#>   survived Child Adult
#>   <fct>    <lgl> <lgl>
#> 1 No       NA    NA   
#> 2 Yes      NA    NA

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
#> 1 Male       0
#> 2 Female   140

flat_titanic |> pivot_samplen(rows = sex, value = freq)
#> # A tibble: 2 × 2
#>   sex    value      
#>   <fct>  <chr>      
#> 1 Male   154; 11; 57
#> 2 Female 14; 0; 1

flat_titanic |> pivot_list(rows = sex, cols = survived, value = freq)
#> # A tibble: 2 × 3
#>   sex    No                              Yes                          
#>   <fct>  <chr>                           <chr>                        
#> 1 Male   0; 0; 35; 0; 118; 154; 387; 670 5; 11; 13; 0; 57; 14; 75; 192
#> 2 Female 0; 0; 17; 0; 4; 13; 89; 3       1; 13; 14; 0; 140; 80; 76; 20

set.seed(12345)
passengers |> pivot_example(rows = Survived, cols = Sex, value = Name)
#> # A tibble: 2 × 3
#>   Survived female                     male                          
#>      <dbl> <chr>                      <chr>                         
#> 1        0 Solvang, Mrs Lena Jacobsen Meyer, Mr August              
#> 2        1 Gibson, Miss Dorothy       Williams, Mr Richard Norris II
passengers |> pivot_samplen(rows = Survived, cols = Sex, value = Name, n = 2, sep = "; ") 
#> # A tibble: 2 × 3
#>   Survived female                                                          male 
#>      <dbl> <chr>                                                           <chr>
#> 1        0 McGowan, Miss Katherine; Klasen, Miss Gertrud Emilia            Smar…
#> 2        1 Ware, Mrs John James (Florence Louise Long); Dyker, Mrs Adolf … Mock…

passengers |> pivot_samplen(rows = Survived, cols = Sex, value = Age, n = 7) 
#> # A tibble: 2 × 3
#>   Survived female                    male                      
#>      <dbl> <chr>                     <chr>                     
#> 1        0 NA; 44; 20; NA; 18; 2; NA NA; NA; 28; NA; 19; NA; NA
#> 2        1 22; 5; 59; 12; 13; 26; NA 32; 9; 35; 60; NA; NA; NA

passengers |> dplyr::sample_n(20) |> pivot_list(rows = Sex, cols = Survived, value = Age)
#> # A tibble: 2 × 3
#>   Sex    `0`                                                `1`       
#>   <chr>  <chr>                                              <chr>     
#> 1 female NA; 30                                             NA; 45; 22
#> 2 male   NA; 24; 26; 29; 21; 29; 19; 46; 54; NA; 21; 22; NA 19; 2
```

# proportions helpers

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

flat_titanic |> 
  pivotr(rows = sex, cols = survived, 
         value = freq, prop = TRUE)
#> # A tibble: 2 × 3
#>   sex       No   Yes
#>   <fct>  <dbl> <dbl>
#> 1 Male   0.62  0.167
#> 2 Female 0.057 0.156

flat_titanic |> 
  pivotr(rows = sex, cols = survived, 
         value = freq, prop = TRUE, within = sex)
#> # A tibble: 2 × 3
#>   sex       No   Yes
#>   <fct>  <dbl> <dbl>
#> 1 Male   0.788 0.212
#> 2 Female 0.268 0.732

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

``` r
knitr::knit_exit()
```
