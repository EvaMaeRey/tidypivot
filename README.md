
- [Toward declarative table
  generation](#toward-declarative-table-generation)
- [{tidypivot} allows you to create tables by describing them (like
  ggplot2 plotting’s
  description/declaration)](#tidypivot-allows-you-to-create-tables-by-describing-them-like-ggplot2-plottings-descriptiondeclaration)
- [declarative table creation with
  ggplot2](#declarative-table-creation-with-ggplot2)
  - [setup](#setup)
  - [declarative table build](#declarative-table-build)
  - [Status quo table creation: Harder than it should
    be?](#status-quo-table-creation-harder-than-it-should-be)
  - [Under the hood, the mechanics follow the status
    quo…](#under-the-hood-the-mechanics-follow-the-status-quo)
- [So internal helpers `data_filter`, `data_define_value`,
  `data_to_grouped`
  etc…](#so-internal-helpers-data_filter-data_define_value-data_to_grouped-etc)
- [Encapsulating steps in `pivotr()`](#encapsulating-steps-in-pivotr)
  - [Examples](#examples)
- [toward a piped workflow](#toward-a-piped-workflow)
  - [tidypivot object](#tidypivot-object)
  - [pipe-friendly user facing
    functions](#pipe-friendly-user-facing-functions)
- [A tibble: 6 × 5](#a-tibble-6--5)
  - [Examples](#examples-1)
- [A tibble: 4 × 4](#a-tibble-4--4)
- [Packaging](#packaging)
- [ggplot2 extension case study…](#ggplot2-extension-case-study)
- [examples/derivative](#examplesderivative)
- [filling cells with examples from
  data.](#filling-cells-with-examples-from-data)
- [proportions helpers](#proportions-helpers)

<!-- README.md is generated from README.Rmd. Please edit that file -->

### Toward declarative table generation

API describe layout of table (rows and cols) and compute (default to
count records)

``` r
library(tidypivot)

ggtable(tidytitanic::passengers) |>
  set_cols(sex) |>
  set_rows(survived)
```

# {tidypivot} allows you to create tables by describing them (like ggplot2 plotting’s description/declaration)

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

### setup

``` r
library(ggplot2)
StatSumLabel <- ggproto(`_class` = "StatSumLabel",
                        `_inherit` = StatSum,
                        default_aes = aes(label = after_stat(n)))

theme_grey(16) |>
  set_theme()
```

### declarative table build

``` r
library(ggplot2)
# 1. Declare table form
tidytitanic::tidy_titanic |>
  ggplot() + 
  aes(x = sex, # cols
      y = survived)   # rows

# 2. Now fill in the table
last_plot() + 
  geom_text(stat = StatSumLabel)  # geom_count could be used too
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="49%" /><img src="man/figures/README-unnamed-chunk-4-2.png" width="49%" />

## Status quo table creation: Harder than it should be?

1.  grouping on vars anticipated to be rows and cols
2.  compute
3.  pivoting

``` r
library(dplyr)
library(tidyr)

tidytitanic::tidy_titanic |> 
  group_by(survived, 
           sex) |> 
  count() |> 
  pivot_wider(
    names_from = sex,
    values_from = n
              )
#> # A tibble: 2 × 3
#> # Groups:   survived [2]
#>   survived  Male Female
#>   <fct>    <int>  <int>
#> 1 No        1364    126
#> 2 Yes        367    344
```

## Under the hood, the mechanics follow the status quo…

- group by rows and columns
- value in data to consider (1 if not specified)
- wt, weight the value (1 if not specified)
- fun - do an operation (on value) within group

# So internal helpers `data_filter`, `data_define_value`, `data_to_grouped` etc…

<details>

``` r
data_filter <- function(data, filter){
  
  dplyr::filter(data, {{filter}})
  
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

.

``` r
tidytitanic::flat_titanic |> 
  data_filter(freq > 35) |>
  data_define_value(value = freq) |> 
  data_to_grouped(rows = survived, cols = sex) |>
  data_grouped_to_summarized() |>
  data_summarized_to_proportioned(percent = T, within = survived) |>
  data_proportioned_to_pivoted(cols = sex)
#> # A tibble: 2 × 3
#>   survived  Male Female
#>   <fct>    <dbl>  <dbl>
#> 1 No        93.7   6.28
#> 2 Yes       52.3  47.7
```

</details>

# Encapsulating steps in `pivotr()`

<details>

``` r
#' @export
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

</details>

### Examples

``` r

tidytitanic::flat_titanic |> 
  pivotr(value = freq, 
         rows = survived, cols = sex, 
         percent = T, within = survived)
#> # A tibble: 2 × 3
#>   survived  Male Female
#>   <fct>    <dbl>  <dbl>
#> 1 No        91.5    8.5
#> 2 Yes       51.6   48.4

tidytitanic::flat_titanic |> 
  pivotr(value = freq, rows = survived, 
         cols = sex, filter = sex == "Female")
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

## tidypivot object

<details>

<https://evamaerey.github.io/mytidytuesday/2024-07-02-s3-tables/s3-tables-tidypivot.html>

``` r
#' @export
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
  tp_obj <- list(
    # data = data,
    # rows = rows,
    # cols = cols,
    # value = value,
    # wt = wt,
    # fun = fun,
    # prop = prop,
    # percent = percent,
    # round = round,
    # within = within,
    # pivot = pivot
    # # more 'slots' to be added
  )

  # declare class 'tidypivot'
  class(tp_obj) <- "tidypivot"

  # Return the created object
  invisible(tp_obj)

}

#' @export
print.tidypivot <- function(tp_obj){
  
  print(do.call(pivotr, tp_obj) |> gt::gt())
  
  invisible(tp_obj)
  
}
```

</details>

## pipe-friendly user facing functions

<details>

``` r
#' @export
ggtable <- function(data = NULL){
  
  # thedata <<- data # don't love this
  data <- data %||% data.frame()
  
  tp_obj <- new_tidypivot()
  
  tp_obj$data <- data
  
  last_tp_obj <<- tp_obj
  
  tp_obj

}


#' @export
last_table <- function(){
  
  last_tp_obj
  
}
```

``` r
knitr::opts_chunk$set(results = "asis")
```

``` r
ggtable() 
```

<div id="wfzvxflmkk"
style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">

<style>#wfzvxflmkk table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#wfzvxflmkk thead, #wfzvxflmkk tbody, #wfzvxflmkk tfoot, #wfzvxflmkk tr, #wfzvxflmkk td, #wfzvxflmkk th {
  border-style: none;
}
&#10;#wfzvxflmkk p {
  margin: 0;
  padding: 0;
}
&#10;#wfzvxflmkk .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}
&#10;#wfzvxflmkk .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#wfzvxflmkk .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#wfzvxflmkk .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#wfzvxflmkk .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#wfzvxflmkk .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#wfzvxflmkk .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#wfzvxflmkk .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}
&#10;#wfzvxflmkk .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#wfzvxflmkk .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#wfzvxflmkk .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#wfzvxflmkk .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#wfzvxflmkk .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#wfzvxflmkk .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}
&#10;#wfzvxflmkk .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}
&#10;#wfzvxflmkk .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#wfzvxflmkk .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#wfzvxflmkk .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#wfzvxflmkk .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#wfzvxflmkk .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#wfzvxflmkk .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#wfzvxflmkk .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#wfzvxflmkk .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#wfzvxflmkk .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#wfzvxflmkk .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#wfzvxflmkk .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#wfzvxflmkk .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#wfzvxflmkk .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#wfzvxflmkk .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#wfzvxflmkk .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#wfzvxflmkk .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#wfzvxflmkk .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#wfzvxflmkk .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#wfzvxflmkk .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#wfzvxflmkk .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#wfzvxflmkk .gt_left {
  text-align: left;
}
&#10;#wfzvxflmkk .gt_center {
  text-align: center;
}
&#10;#wfzvxflmkk .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#wfzvxflmkk .gt_font_normal {
  font-weight: normal;
}
&#10;#wfzvxflmkk .gt_font_bold {
  font-weight: bold;
}
&#10;#wfzvxflmkk .gt_font_italic {
  font-style: italic;
}
&#10;#wfzvxflmkk .gt_super {
  font-size: 65%;
}
&#10;#wfzvxflmkk .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#wfzvxflmkk .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#wfzvxflmkk .gt_indent_1 {
  text-indent: 5px;
}
&#10;#wfzvxflmkk .gt_indent_2 {
  text-indent: 10px;
}
&#10;#wfzvxflmkk .gt_indent_3 {
  text-indent: 15px;
}
&#10;#wfzvxflmkk .gt_indent_4 {
  text-indent: 20px;
}
&#10;#wfzvxflmkk .gt_indent_5 {
  text-indent: 25px;
}
&#10;#wfzvxflmkk .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#wfzvxflmkk div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
<thead>
<tr class="gt_col_headings">
<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="value">
value
</th>
</tr>
</thead>
<tbody class="gt_table_body">
<tr>
<td headers="value" class="gt_row gt_right">
0
</td>
</tr>
</tbody>
</table>

</div>

``` r


tidytitanic::tidy_titanic |> head()
```

# A tibble: 6 × 5

     id class sex   age   survived

<int> <fct> <fct> <fct> <fct>  
1 1 3rd Male Child No  
2 2 3rd Male Child No  
3 3 3rd Male Child No  
4 4 3rd Male Child No  
5 5 3rd Male Child No  
6 6 3rd Male Child No

``` r

ggtable(tidytitanic::tidy_titanic) 
```

<div id="iuezdhkmoi"
style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">

<style>#iuezdhkmoi table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#iuezdhkmoi thead, #iuezdhkmoi tbody, #iuezdhkmoi tfoot, #iuezdhkmoi tr, #iuezdhkmoi td, #iuezdhkmoi th {
  border-style: none;
}
&#10;#iuezdhkmoi p {
  margin: 0;
  padding: 0;
}
&#10;#iuezdhkmoi .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}
&#10;#iuezdhkmoi .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#iuezdhkmoi .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#iuezdhkmoi .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#iuezdhkmoi .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#iuezdhkmoi .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#iuezdhkmoi .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#iuezdhkmoi .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}
&#10;#iuezdhkmoi .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#iuezdhkmoi .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#iuezdhkmoi .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#iuezdhkmoi .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#iuezdhkmoi .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#iuezdhkmoi .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}
&#10;#iuezdhkmoi .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}
&#10;#iuezdhkmoi .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#iuezdhkmoi .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#iuezdhkmoi .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#iuezdhkmoi .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#iuezdhkmoi .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#iuezdhkmoi .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#iuezdhkmoi .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#iuezdhkmoi .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#iuezdhkmoi .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#iuezdhkmoi .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#iuezdhkmoi .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#iuezdhkmoi .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#iuezdhkmoi .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#iuezdhkmoi .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#iuezdhkmoi .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#iuezdhkmoi .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#iuezdhkmoi .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#iuezdhkmoi .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#iuezdhkmoi .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#iuezdhkmoi .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#iuezdhkmoi .gt_left {
  text-align: left;
}
&#10;#iuezdhkmoi .gt_center {
  text-align: center;
}
&#10;#iuezdhkmoi .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#iuezdhkmoi .gt_font_normal {
  font-weight: normal;
}
&#10;#iuezdhkmoi .gt_font_bold {
  font-weight: bold;
}
&#10;#iuezdhkmoi .gt_font_italic {
  font-style: italic;
}
&#10;#iuezdhkmoi .gt_super {
  font-size: 65%;
}
&#10;#iuezdhkmoi .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#iuezdhkmoi .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#iuezdhkmoi .gt_indent_1 {
  text-indent: 5px;
}
&#10;#iuezdhkmoi .gt_indent_2 {
  text-indent: 10px;
}
&#10;#iuezdhkmoi .gt_indent_3 {
  text-indent: 15px;
}
&#10;#iuezdhkmoi .gt_indent_4 {
  text-indent: 20px;
}
&#10;#iuezdhkmoi .gt_indent_5 {
  text-indent: 25px;
}
&#10;#iuezdhkmoi .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#iuezdhkmoi div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
<thead>
<tr class="gt_col_headings">
<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="value">
value
</th>
</tr>
</thead>
<tbody class="gt_table_body">
<tr>
<td headers="value" class="gt_row gt_right">
2201
</td>
</tr>
</tbody>
</table>

</div>

``` r


last_table()
```

<div id="cnstzmader"
style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">

<style>#cnstzmader table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#cnstzmader thead, #cnstzmader tbody, #cnstzmader tfoot, #cnstzmader tr, #cnstzmader td, #cnstzmader th {
  border-style: none;
}
&#10;#cnstzmader p {
  margin: 0;
  padding: 0;
}
&#10;#cnstzmader .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}
&#10;#cnstzmader .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#cnstzmader .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#cnstzmader .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#cnstzmader .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#cnstzmader .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#cnstzmader .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#cnstzmader .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}
&#10;#cnstzmader .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#cnstzmader .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#cnstzmader .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#cnstzmader .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#cnstzmader .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#cnstzmader .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}
&#10;#cnstzmader .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}
&#10;#cnstzmader .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#cnstzmader .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#cnstzmader .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#cnstzmader .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#cnstzmader .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#cnstzmader .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#cnstzmader .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#cnstzmader .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#cnstzmader .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#cnstzmader .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#cnstzmader .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#cnstzmader .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#cnstzmader .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#cnstzmader .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#cnstzmader .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#cnstzmader .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#cnstzmader .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#cnstzmader .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#cnstzmader .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#cnstzmader .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#cnstzmader .gt_left {
  text-align: left;
}
&#10;#cnstzmader .gt_center {
  text-align: center;
}
&#10;#cnstzmader .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#cnstzmader .gt_font_normal {
  font-weight: normal;
}
&#10;#cnstzmader .gt_font_bold {
  font-weight: bold;
}
&#10;#cnstzmader .gt_font_italic {
  font-style: italic;
}
&#10;#cnstzmader .gt_super {
  font-size: 65%;
}
&#10;#cnstzmader .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#cnstzmader .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#cnstzmader .gt_indent_1 {
  text-indent: 5px;
}
&#10;#cnstzmader .gt_indent_2 {
  text-indent: 10px;
}
&#10;#cnstzmader .gt_indent_3 {
  text-indent: 15px;
}
&#10;#cnstzmader .gt_indent_4 {
  text-indent: 20px;
}
&#10;#cnstzmader .gt_indent_5 {
  text-indent: 25px;
}
&#10;#cnstzmader .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#cnstzmader div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
<thead>
<tr class="gt_col_headings">
<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="value">
value
</th>
</tr>
</thead>
<tbody class="gt_table_body">
<tr>
<td headers="value" class="gt_row gt_right">
2201
</td>
</tr>
</tbody>
</table>

</div>

``` r
#' @export
set_rows <- function(tp_obj, rows = NULL){
  
  tp_obj$rows <- enquo(rows)
  
  last_tp_obj <<- tp_obj
  
  tp_obj

  
}


#' @export
set_cols <- function(tp_obj, cols = NULL){
  
  tp_obj$cols <- enquo(cols)

  last_tp_obj <<- tp_obj
  
  tp_obj
  

}

#' @export
set_filter <- function(tp_obj, filter = TRUE){
  
  if(!filter){tp_obj$filter <- enquo(filter)}
  
  last_tp_obj <<- tp_obj
  
  tp_obj
  

}
```

``` r
ggtable(tidytitanic::tidy_titanic) |>
  set_rows(sex) |>
  set_cols(survived) |>
  set_filter(TRUE)
```

<div id="axntaoihkz"
style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">

<style>#axntaoihkz table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#axntaoihkz thead, #axntaoihkz tbody, #axntaoihkz tfoot, #axntaoihkz tr, #axntaoihkz td, #axntaoihkz th {
  border-style: none;
}
&#10;#axntaoihkz p {
  margin: 0;
  padding: 0;
}
&#10;#axntaoihkz .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}
&#10;#axntaoihkz .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#axntaoihkz .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#axntaoihkz .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#axntaoihkz .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#axntaoihkz .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#axntaoihkz .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#axntaoihkz .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}
&#10;#axntaoihkz .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#axntaoihkz .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#axntaoihkz .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#axntaoihkz .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#axntaoihkz .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#axntaoihkz .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}
&#10;#axntaoihkz .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}
&#10;#axntaoihkz .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#axntaoihkz .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#axntaoihkz .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#axntaoihkz .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#axntaoihkz .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#axntaoihkz .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#axntaoihkz .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#axntaoihkz .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#axntaoihkz .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#axntaoihkz .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#axntaoihkz .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#axntaoihkz .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#axntaoihkz .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#axntaoihkz .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#axntaoihkz .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#axntaoihkz .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#axntaoihkz .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#axntaoihkz .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#axntaoihkz .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#axntaoihkz .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#axntaoihkz .gt_left {
  text-align: left;
}
&#10;#axntaoihkz .gt_center {
  text-align: center;
}
&#10;#axntaoihkz .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#axntaoihkz .gt_font_normal {
  font-weight: normal;
}
&#10;#axntaoihkz .gt_font_bold {
  font-weight: bold;
}
&#10;#axntaoihkz .gt_font_italic {
  font-style: italic;
}
&#10;#axntaoihkz .gt_super {
  font-size: 65%;
}
&#10;#axntaoihkz .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#axntaoihkz .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#axntaoihkz .gt_indent_1 {
  text-indent: 5px;
}
&#10;#axntaoihkz .gt_indent_2 {
  text-indent: 10px;
}
&#10;#axntaoihkz .gt_indent_3 {
  text-indent: 15px;
}
&#10;#axntaoihkz .gt_indent_4 {
  text-indent: 20px;
}
&#10;#axntaoihkz .gt_indent_5 {
  text-indent: 25px;
}
&#10;#axntaoihkz .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#axntaoihkz div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
<thead>
<tr class="gt_col_headings">
<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="sex">
sex
</th>
<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="No">
No
</th>
<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Yes">
Yes
</th>
</tr>
</thead>
<tbody class="gt_table_body">
<tr>
<td headers="sex" class="gt_row gt_center">
Male
</td>
<td headers="No" class="gt_row gt_right">
1364
</td>
<td headers="Yes" class="gt_row gt_right">
367
</td>
</tr>
<tr>
<td headers="sex" class="gt_row gt_center">
Female
</td>
<td headers="No" class="gt_row gt_right">
126
</td>
<td headers="Yes" class="gt_row gt_right">
344
</td>
</tr>
</tbody>
</table>

</div>

``` r

# last_table |>
#   set_filter(sex == "Female")
```

``` r
#' @export
set_fun <- function(tp_obj, fun = sum){

  tp_obj$fun <- fun

  last_tp_obj <<- tp_obj

  tp_obj
  
}


#' @export
set_fun_example <- function(tp_obj){

  tp_obj$fun <- function(x) sample(x, 1)


  last_tp_obj <<- tp_obj

  tp_obj
  
}


#' @export
set_fun_sample <- function(tp_obj, n = 2, sep = "; "){

  tp_obj$fun <- function(x) paste(sample(x, n, replace = F), collapse = sep)


  last_tp_obj <<- tp_obj

  tp_obj
  
}


#' @export
set_fun_list <- function(tp_obj, sep = "; "){

  tp_obj$fun <- function(x) paste(x, collapse = sep)


  last_tp_obj <<- tp_obj

  tp_obj
  
}
```

``` r
#' @export
set_value <- function(tp_obj, value = NULL){
  
  tp_obj$value <- enquo(value)

  last_tp_obj <<- tp_obj
  
  tp_obj
  
}



#' @export
set_wt <- function(tp_obj, wt = NULL){
  
  tp_obj$wt <- enquo(wt)

  last_tp_obj <<- tp_obj
  
  tp_obj
  
}


#' @export
set_weight <- function(tp_obj, weight = NULL){
  
  tp_obj$weight <- enquo(weight)
  
  last_tp_obj <<- tp_obj
  
  tp_obj
  
}
```

``` r

#' @export
calc_prop <- function(tp_obj, within = NULL){
  
  tp_obj$percent <- FALSE
  tp_obj$prop <- TRUE
  tp_obj$within <- enquo(within)
  
  last_tp_obj <<- tp_obj
  
  tp_obj
  
}

#' @export
calc_percent <- function(tp_obj, within = NULL){
  
  tp_obj$prop <- FALSE
  tp_obj$percent <- TRUE
  tp_obj$within <- enquo(within)
  
  last_tp_obj <<- tp_obj
  
  tp_obj
  
}


#' @export
set_within <- function(tp_obj, within = NULL){
  
  tp_obj$within <- enquo(within)
  
  last_tp_obj <<- tp_obj
  
  tp_obj
  
}
```

``` r
#' @export
no_pivot <- function(tp_obj){
  
  tp_obj$pivot <- FALSE

  last_tp_obj <<- tp_obj

  tp_obj  
  
}

collect <- function(tp_obj){
  
  do.call(pivotr, tp_obj)
  
}
```

</details>

### Examples

``` r
tidytitanic::passengers |> head()
```

passenger_id title last_name first_name survived pclass sex age sib_sp 1
1 Mr. Braund Owen 0 3 male 22 1 2 2 Mrs. Cumings Florence 1 1 female 38
1 3 3 Miss. Heikkinen Laina 1 3 female 26 0 4 4 Mrs. Futrelle Lily 1 1
female 35 1 5 5 Mr. Allen William 0 3 male 35 0 6 6 Mr. Moran James 0 3
male NA 0 parch fare cabin embarked ticket maiden_name 1 0 7.2500 S A/5
21171 <NA> 2 0 71.2833 C85 C PC 17599 Florence Briggs Thayer 3 0 7.9250
S STON/O2. 3101282 <NA> 4 0 53.1000 C123 S 113803 Lily May Peel 5 0
8.0500 S 373450 <NA> 6 0 8.4583 Q 330877 <NA> name prefered_name 1
Braund, Mr. Owen Harris <NA> 2 Cumings, Mrs. John Bradley (Florence
Briggs Thayer) <NA> 3 Heikkinen, Miss. Laina <NA> 4 Futrelle,
Mrs. Jacques Heath (Lily May Peel) <NA> 5 Allen, Mr. William Henry <NA>
6 Moran, Mr. James <NA>

``` r
ggtable(tidytitanic::passengers) |>
  set_cols(sex) |>
  set_rows(survived)
```

<div id="uidtowxuco"
style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">

<style>#uidtowxuco table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#uidtowxuco thead, #uidtowxuco tbody, #uidtowxuco tfoot, #uidtowxuco tr, #uidtowxuco td, #uidtowxuco th {
  border-style: none;
}
&#10;#uidtowxuco p {
  margin: 0;
  padding: 0;
}
&#10;#uidtowxuco .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}
&#10;#uidtowxuco .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#uidtowxuco .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#uidtowxuco .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#uidtowxuco .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#uidtowxuco .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#uidtowxuco .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#uidtowxuco .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}
&#10;#uidtowxuco .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#uidtowxuco .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#uidtowxuco .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#uidtowxuco .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#uidtowxuco .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#uidtowxuco .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}
&#10;#uidtowxuco .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}
&#10;#uidtowxuco .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#uidtowxuco .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#uidtowxuco .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#uidtowxuco .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#uidtowxuco .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#uidtowxuco .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#uidtowxuco .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#uidtowxuco .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#uidtowxuco .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#uidtowxuco .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#uidtowxuco .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#uidtowxuco .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#uidtowxuco .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#uidtowxuco .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#uidtowxuco .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#uidtowxuco .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#uidtowxuco .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#uidtowxuco .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#uidtowxuco .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#uidtowxuco .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#uidtowxuco .gt_left {
  text-align: left;
}
&#10;#uidtowxuco .gt_center {
  text-align: center;
}
&#10;#uidtowxuco .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#uidtowxuco .gt_font_normal {
  font-weight: normal;
}
&#10;#uidtowxuco .gt_font_bold {
  font-weight: bold;
}
&#10;#uidtowxuco .gt_font_italic {
  font-style: italic;
}
&#10;#uidtowxuco .gt_super {
  font-size: 65%;
}
&#10;#uidtowxuco .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#uidtowxuco .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#uidtowxuco .gt_indent_1 {
  text-indent: 5px;
}
&#10;#uidtowxuco .gt_indent_2 {
  text-indent: 10px;
}
&#10;#uidtowxuco .gt_indent_3 {
  text-indent: 15px;
}
&#10;#uidtowxuco .gt_indent_4 {
  text-indent: 20px;
}
&#10;#uidtowxuco .gt_indent_5 {
  text-indent: 25px;
}
&#10;#uidtowxuco .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#uidtowxuco div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
<thead>
<tr class="gt_col_headings">
<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="survived">
survived
</th>
<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="female">
female
</th>
<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="male">
male
</th>
</tr>
</thead>
<tbody class="gt_table_body">
<tr>
<td headers="survived" class="gt_row gt_right">
0
</td>
<td headers="female" class="gt_row gt_right">
81
</td>
<td headers="male" class="gt_row gt_right">
734
</td>
</tr>
<tr>
<td headers="survived" class="gt_row gt_right">
1
</td>
<td headers="female" class="gt_row gt_right">
385
</td>
<td headers="male" class="gt_row gt_right">
109
</td>
</tr>
</tbody>
</table>

</div>

``` r

last_table() |>
  calc_percent()
```

<div id="vyasdkxbxk"
style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">

<style>#vyasdkxbxk table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#vyasdkxbxk thead, #vyasdkxbxk tbody, #vyasdkxbxk tfoot, #vyasdkxbxk tr, #vyasdkxbxk td, #vyasdkxbxk th {
  border-style: none;
}
&#10;#vyasdkxbxk p {
  margin: 0;
  padding: 0;
}
&#10;#vyasdkxbxk .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}
&#10;#vyasdkxbxk .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#vyasdkxbxk .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#vyasdkxbxk .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#vyasdkxbxk .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#vyasdkxbxk .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#vyasdkxbxk .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#vyasdkxbxk .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}
&#10;#vyasdkxbxk .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#vyasdkxbxk .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#vyasdkxbxk .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#vyasdkxbxk .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#vyasdkxbxk .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#vyasdkxbxk .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}
&#10;#vyasdkxbxk .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}
&#10;#vyasdkxbxk .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#vyasdkxbxk .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#vyasdkxbxk .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#vyasdkxbxk .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#vyasdkxbxk .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#vyasdkxbxk .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#vyasdkxbxk .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#vyasdkxbxk .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#vyasdkxbxk .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#vyasdkxbxk .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#vyasdkxbxk .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#vyasdkxbxk .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#vyasdkxbxk .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#vyasdkxbxk .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#vyasdkxbxk .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#vyasdkxbxk .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#vyasdkxbxk .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#vyasdkxbxk .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#vyasdkxbxk .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#vyasdkxbxk .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#vyasdkxbxk .gt_left {
  text-align: left;
}
&#10;#vyasdkxbxk .gt_center {
  text-align: center;
}
&#10;#vyasdkxbxk .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#vyasdkxbxk .gt_font_normal {
  font-weight: normal;
}
&#10;#vyasdkxbxk .gt_font_bold {
  font-weight: bold;
}
&#10;#vyasdkxbxk .gt_font_italic {
  font-style: italic;
}
&#10;#vyasdkxbxk .gt_super {
  font-size: 65%;
}
&#10;#vyasdkxbxk .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#vyasdkxbxk .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#vyasdkxbxk .gt_indent_1 {
  text-indent: 5px;
}
&#10;#vyasdkxbxk .gt_indent_2 {
  text-indent: 10px;
}
&#10;#vyasdkxbxk .gt_indent_3 {
  text-indent: 15px;
}
&#10;#vyasdkxbxk .gt_indent_4 {
  text-indent: 20px;
}
&#10;#vyasdkxbxk .gt_indent_5 {
  text-indent: 25px;
}
&#10;#vyasdkxbxk .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#vyasdkxbxk div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
<thead>
<tr class="gt_col_headings">
<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="survived">
survived
</th>
<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="female">
female
</th>
<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="male">
male
</th>
</tr>
</thead>
<tbody class="gt_table_body">
<tr>
<td headers="survived" class="gt_row gt_right">
0
</td>
<td headers="female" class="gt_row gt_right">
6.2
</td>
<td headers="male" class="gt_row gt_right">
56.1
</td>
</tr>
<tr>
<td headers="survived" class="gt_row gt_right">
1
</td>
<td headers="female" class="gt_row gt_right">
29.4
</td>
<td headers="male" class="gt_row gt_right">
8.3
</td>
</tr>
</tbody>
</table>

</div>

``` r

last_table() |>
  calc_prop()
```

<div id="eolfhojzte"
style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">

<style>#eolfhojzte table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#eolfhojzte thead, #eolfhojzte tbody, #eolfhojzte tfoot, #eolfhojzte tr, #eolfhojzte td, #eolfhojzte th {
  border-style: none;
}
&#10;#eolfhojzte p {
  margin: 0;
  padding: 0;
}
&#10;#eolfhojzte .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}
&#10;#eolfhojzte .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#eolfhojzte .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#eolfhojzte .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#eolfhojzte .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#eolfhojzte .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#eolfhojzte .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#eolfhojzte .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}
&#10;#eolfhojzte .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#eolfhojzte .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#eolfhojzte .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#eolfhojzte .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#eolfhojzte .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#eolfhojzte .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}
&#10;#eolfhojzte .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}
&#10;#eolfhojzte .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#eolfhojzte .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#eolfhojzte .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#eolfhojzte .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#eolfhojzte .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#eolfhojzte .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#eolfhojzte .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#eolfhojzte .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#eolfhojzte .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#eolfhojzte .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#eolfhojzte .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#eolfhojzte .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#eolfhojzte .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#eolfhojzte .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#eolfhojzte .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#eolfhojzte .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#eolfhojzte .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#eolfhojzte .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#eolfhojzte .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#eolfhojzte .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#eolfhojzte .gt_left {
  text-align: left;
}
&#10;#eolfhojzte .gt_center {
  text-align: center;
}
&#10;#eolfhojzte .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#eolfhojzte .gt_font_normal {
  font-weight: normal;
}
&#10;#eolfhojzte .gt_font_bold {
  font-weight: bold;
}
&#10;#eolfhojzte .gt_font_italic {
  font-style: italic;
}
&#10;#eolfhojzte .gt_super {
  font-size: 65%;
}
&#10;#eolfhojzte .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#eolfhojzte .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#eolfhojzte .gt_indent_1 {
  text-indent: 5px;
}
&#10;#eolfhojzte .gt_indent_2 {
  text-indent: 10px;
}
&#10;#eolfhojzte .gt_indent_3 {
  text-indent: 15px;
}
&#10;#eolfhojzte .gt_indent_4 {
  text-indent: 20px;
}
&#10;#eolfhojzte .gt_indent_5 {
  text-indent: 25px;
}
&#10;#eolfhojzte .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#eolfhojzte div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
<thead>
<tr class="gt_col_headings">
<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="survived">
survived
</th>
<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="female">
female
</th>
<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="male">
male
</th>
</tr>
</thead>
<tbody class="gt_table_body">
<tr>
<td headers="survived" class="gt_row gt_right">
0
</td>
<td headers="female" class="gt_row gt_right">
0.062
</td>
<td headers="male" class="gt_row gt_right">
0.561
</td>
</tr>
<tr>
<td headers="survived" class="gt_row gt_right">
1
</td>
<td headers="female" class="gt_row gt_right">
0.294
</td>
<td headers="male" class="gt_row gt_right">
0.083
</td>
</tr>
</tbody>
</table>

</div>

``` r

last_table() |>
  calc_prop(within = sex)
```

<div id="tqntzaopmr"
style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">

<style>#tqntzaopmr table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#tqntzaopmr thead, #tqntzaopmr tbody, #tqntzaopmr tfoot, #tqntzaopmr tr, #tqntzaopmr td, #tqntzaopmr th {
  border-style: none;
}
&#10;#tqntzaopmr p {
  margin: 0;
  padding: 0;
}
&#10;#tqntzaopmr .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}
&#10;#tqntzaopmr .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#tqntzaopmr .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#tqntzaopmr .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#tqntzaopmr .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#tqntzaopmr .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#tqntzaopmr .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#tqntzaopmr .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}
&#10;#tqntzaopmr .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#tqntzaopmr .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#tqntzaopmr .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#tqntzaopmr .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#tqntzaopmr .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#tqntzaopmr .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}
&#10;#tqntzaopmr .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}
&#10;#tqntzaopmr .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#tqntzaopmr .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#tqntzaopmr .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#tqntzaopmr .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#tqntzaopmr .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#tqntzaopmr .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#tqntzaopmr .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#tqntzaopmr .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#tqntzaopmr .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#tqntzaopmr .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#tqntzaopmr .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#tqntzaopmr .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#tqntzaopmr .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#tqntzaopmr .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#tqntzaopmr .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#tqntzaopmr .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#tqntzaopmr .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#tqntzaopmr .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#tqntzaopmr .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#tqntzaopmr .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#tqntzaopmr .gt_left {
  text-align: left;
}
&#10;#tqntzaopmr .gt_center {
  text-align: center;
}
&#10;#tqntzaopmr .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#tqntzaopmr .gt_font_normal {
  font-weight: normal;
}
&#10;#tqntzaopmr .gt_font_bold {
  font-weight: bold;
}
&#10;#tqntzaopmr .gt_font_italic {
  font-style: italic;
}
&#10;#tqntzaopmr .gt_super {
  font-size: 65%;
}
&#10;#tqntzaopmr .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#tqntzaopmr .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#tqntzaopmr .gt_indent_1 {
  text-indent: 5px;
}
&#10;#tqntzaopmr .gt_indent_2 {
  text-indent: 10px;
}
&#10;#tqntzaopmr .gt_indent_3 {
  text-indent: 15px;
}
&#10;#tqntzaopmr .gt_indent_4 {
  text-indent: 20px;
}
&#10;#tqntzaopmr .gt_indent_5 {
  text-indent: 25px;
}
&#10;#tqntzaopmr .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#tqntzaopmr div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
<thead>
<tr class="gt_col_headings">
<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="survived">
survived
</th>
<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="female">
female
</th>
<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="male">
male
</th>
</tr>
</thead>
<tbody class="gt_table_body">
<tr>
<td headers="survived" class="gt_row gt_right">
0
</td>
<td headers="female" class="gt_row gt_right">
0.174
</td>
<td headers="male" class="gt_row gt_right">
0.871
</td>
</tr>
<tr>
<td headers="survived" class="gt_row gt_right">
1
</td>
<td headers="female" class="gt_row gt_right">
0.826
</td>
<td headers="male" class="gt_row gt_right">
0.129
</td>
</tr>
</tbody>
</table>

</div>

``` r

# a null table...
ggtable(tidytitanic::flat_titanic) |>
  set_value(NA) |>
  set_rows(sex)
```

<div id="auobrlwdfy"
style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">

<style>#auobrlwdfy table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#auobrlwdfy thead, #auobrlwdfy tbody, #auobrlwdfy tfoot, #auobrlwdfy tr, #auobrlwdfy td, #auobrlwdfy th {
  border-style: none;
}
&#10;#auobrlwdfy p {
  margin: 0;
  padding: 0;
}
&#10;#auobrlwdfy .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}
&#10;#auobrlwdfy .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#auobrlwdfy .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#auobrlwdfy .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#auobrlwdfy .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#auobrlwdfy .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#auobrlwdfy .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#auobrlwdfy .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}
&#10;#auobrlwdfy .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#auobrlwdfy .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#auobrlwdfy .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#auobrlwdfy .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#auobrlwdfy .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#auobrlwdfy .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}
&#10;#auobrlwdfy .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}
&#10;#auobrlwdfy .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#auobrlwdfy .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#auobrlwdfy .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#auobrlwdfy .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#auobrlwdfy .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#auobrlwdfy .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#auobrlwdfy .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#auobrlwdfy .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#auobrlwdfy .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#auobrlwdfy .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#auobrlwdfy .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#auobrlwdfy .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#auobrlwdfy .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#auobrlwdfy .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#auobrlwdfy .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#auobrlwdfy .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#auobrlwdfy .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#auobrlwdfy .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#auobrlwdfy .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#auobrlwdfy .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#auobrlwdfy .gt_left {
  text-align: left;
}
&#10;#auobrlwdfy .gt_center {
  text-align: center;
}
&#10;#auobrlwdfy .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#auobrlwdfy .gt_font_normal {
  font-weight: normal;
}
&#10;#auobrlwdfy .gt_font_bold {
  font-weight: bold;
}
&#10;#auobrlwdfy .gt_font_italic {
  font-style: italic;
}
&#10;#auobrlwdfy .gt_super {
  font-size: 65%;
}
&#10;#auobrlwdfy .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#auobrlwdfy .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#auobrlwdfy .gt_indent_1 {
  text-indent: 5px;
}
&#10;#auobrlwdfy .gt_indent_2 {
  text-indent: 10px;
}
&#10;#auobrlwdfy .gt_indent_3 {
  text-indent: 15px;
}
&#10;#auobrlwdfy .gt_indent_4 {
  text-indent: 20px;
}
&#10;#auobrlwdfy .gt_indent_5 {
  text-indent: 25px;
}
&#10;#auobrlwdfy .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#auobrlwdfy div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
<thead>
<tr class="gt_col_headings">
<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="sex">
sex
</th>
<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="value">
value
</th>
</tr>
</thead>
<tbody class="gt_table_body">
<tr>
<td headers="sex" class="gt_row gt_center">
Male
</td>
<td headers="value" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="sex" class="gt_row gt_center">
Female
</td>
<td headers="value" class="gt_row gt_right">
NA
</td>
</tr>
</tbody>
</table>

</div>

``` r

ggtable(tidytitanic::flat_titanic) |>
  set_wt(freq) |>
  set_rows(sex) |>
  set_cols(survived) 
```

<div id="jvhjclhmbi"
style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">

<style>#jvhjclhmbi table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#jvhjclhmbi thead, #jvhjclhmbi tbody, #jvhjclhmbi tfoot, #jvhjclhmbi tr, #jvhjclhmbi td, #jvhjclhmbi th {
  border-style: none;
}
&#10;#jvhjclhmbi p {
  margin: 0;
  padding: 0;
}
&#10;#jvhjclhmbi .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}
&#10;#jvhjclhmbi .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#jvhjclhmbi .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#jvhjclhmbi .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#jvhjclhmbi .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#jvhjclhmbi .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#jvhjclhmbi .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#jvhjclhmbi .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}
&#10;#jvhjclhmbi .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#jvhjclhmbi .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#jvhjclhmbi .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#jvhjclhmbi .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#jvhjclhmbi .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#jvhjclhmbi .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}
&#10;#jvhjclhmbi .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}
&#10;#jvhjclhmbi .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#jvhjclhmbi .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#jvhjclhmbi .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#jvhjclhmbi .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#jvhjclhmbi .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#jvhjclhmbi .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#jvhjclhmbi .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#jvhjclhmbi .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#jvhjclhmbi .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#jvhjclhmbi .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#jvhjclhmbi .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#jvhjclhmbi .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#jvhjclhmbi .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#jvhjclhmbi .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#jvhjclhmbi .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#jvhjclhmbi .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#jvhjclhmbi .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#jvhjclhmbi .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#jvhjclhmbi .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#jvhjclhmbi .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#jvhjclhmbi .gt_left {
  text-align: left;
}
&#10;#jvhjclhmbi .gt_center {
  text-align: center;
}
&#10;#jvhjclhmbi .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#jvhjclhmbi .gt_font_normal {
  font-weight: normal;
}
&#10;#jvhjclhmbi .gt_font_bold {
  font-weight: bold;
}
&#10;#jvhjclhmbi .gt_font_italic {
  font-style: italic;
}
&#10;#jvhjclhmbi .gt_super {
  font-size: 65%;
}
&#10;#jvhjclhmbi .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#jvhjclhmbi .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#jvhjclhmbi .gt_indent_1 {
  text-indent: 5px;
}
&#10;#jvhjclhmbi .gt_indent_2 {
  text-indent: 10px;
}
&#10;#jvhjclhmbi .gt_indent_3 {
  text-indent: 15px;
}
&#10;#jvhjclhmbi .gt_indent_4 {
  text-indent: 20px;
}
&#10;#jvhjclhmbi .gt_indent_5 {
  text-indent: 25px;
}
&#10;#jvhjclhmbi .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#jvhjclhmbi div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
<thead>
<tr class="gt_col_headings">
<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="sex">
sex
</th>
<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="No">
No
</th>
<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Yes">
Yes
</th>
</tr>
</thead>
<tbody class="gt_table_body">
<tr>
<td headers="sex" class="gt_row gt_center">
Male
</td>
<td headers="No" class="gt_row gt_right">
1364
</td>
<td headers="Yes" class="gt_row gt_right">
367
</td>
</tr>
<tr>
<td headers="sex" class="gt_row gt_center">
Female
</td>
<td headers="No" class="gt_row gt_right">
126
</td>
<td headers="Yes" class="gt_row gt_right">
344
</td>
</tr>
</tbody>
</table>

</div>

``` r

last_table() |>
  set_fun(mean)
```

<div id="xrzlfvrvlw"
style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">

<style>#xrzlfvrvlw table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#xrzlfvrvlw thead, #xrzlfvrvlw tbody, #xrzlfvrvlw tfoot, #xrzlfvrvlw tr, #xrzlfvrvlw td, #xrzlfvrvlw th {
  border-style: none;
}
&#10;#xrzlfvrvlw p {
  margin: 0;
  padding: 0;
}
&#10;#xrzlfvrvlw .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}
&#10;#xrzlfvrvlw .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#xrzlfvrvlw .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#xrzlfvrvlw .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#xrzlfvrvlw .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#xrzlfvrvlw .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#xrzlfvrvlw .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#xrzlfvrvlw .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}
&#10;#xrzlfvrvlw .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#xrzlfvrvlw .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#xrzlfvrvlw .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#xrzlfvrvlw .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#xrzlfvrvlw .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#xrzlfvrvlw .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}
&#10;#xrzlfvrvlw .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}
&#10;#xrzlfvrvlw .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#xrzlfvrvlw .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#xrzlfvrvlw .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#xrzlfvrvlw .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#xrzlfvrvlw .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#xrzlfvrvlw .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#xrzlfvrvlw .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#xrzlfvrvlw .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#xrzlfvrvlw .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#xrzlfvrvlw .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#xrzlfvrvlw .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#xrzlfvrvlw .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#xrzlfvrvlw .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#xrzlfvrvlw .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#xrzlfvrvlw .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#xrzlfvrvlw .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#xrzlfvrvlw .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#xrzlfvrvlw .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#xrzlfvrvlw .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#xrzlfvrvlw .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#xrzlfvrvlw .gt_left {
  text-align: left;
}
&#10;#xrzlfvrvlw .gt_center {
  text-align: center;
}
&#10;#xrzlfvrvlw .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#xrzlfvrvlw .gt_font_normal {
  font-weight: normal;
}
&#10;#xrzlfvrvlw .gt_font_bold {
  font-weight: bold;
}
&#10;#xrzlfvrvlw .gt_font_italic {
  font-style: italic;
}
&#10;#xrzlfvrvlw .gt_super {
  font-size: 65%;
}
&#10;#xrzlfvrvlw .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#xrzlfvrvlw .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#xrzlfvrvlw .gt_indent_1 {
  text-indent: 5px;
}
&#10;#xrzlfvrvlw .gt_indent_2 {
  text-indent: 10px;
}
&#10;#xrzlfvrvlw .gt_indent_3 {
  text-indent: 15px;
}
&#10;#xrzlfvrvlw .gt_indent_4 {
  text-indent: 20px;
}
&#10;#xrzlfvrvlw .gt_indent_5 {
  text-indent: 25px;
}
&#10;#xrzlfvrvlw .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#xrzlfvrvlw div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
<thead>
<tr class="gt_col_headings">
<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="sex">
sex
</th>
<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="No">
No
</th>
<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Yes">
Yes
</th>
</tr>
</thead>
<tbody class="gt_table_body">
<tr>
<td headers="sex" class="gt_row gt_center">
Male
</td>
<td headers="No" class="gt_row gt_right">
170.50
</td>
<td headers="Yes" class="gt_row gt_right">
45.875
</td>
</tr>
<tr>
<td headers="sex" class="gt_row gt_center">
Female
</td>
<td headers="No" class="gt_row gt_right">
15.75
</td>
<td headers="Yes" class="gt_row gt_right">
43.000
</td>
</tr>
</tbody>
</table>

</div>

``` r

last_table() |> 
  no_pivot()
```

<div id="vybthmvnek"
style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">

<style>#vybthmvnek table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#vybthmvnek thead, #vybthmvnek tbody, #vybthmvnek tfoot, #vybthmvnek tr, #vybthmvnek td, #vybthmvnek th {
  border-style: none;
}
&#10;#vybthmvnek p {
  margin: 0;
  padding: 0;
}
&#10;#vybthmvnek .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}
&#10;#vybthmvnek .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#vybthmvnek .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#vybthmvnek .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#vybthmvnek .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#vybthmvnek .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#vybthmvnek .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#vybthmvnek .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}
&#10;#vybthmvnek .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#vybthmvnek .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#vybthmvnek .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#vybthmvnek .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#vybthmvnek .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#vybthmvnek .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}
&#10;#vybthmvnek .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}
&#10;#vybthmvnek .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#vybthmvnek .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#vybthmvnek .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#vybthmvnek .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#vybthmvnek .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#vybthmvnek .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#vybthmvnek .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#vybthmvnek .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#vybthmvnek .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#vybthmvnek .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#vybthmvnek .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#vybthmvnek .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#vybthmvnek .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#vybthmvnek .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#vybthmvnek .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#vybthmvnek .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#vybthmvnek .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#vybthmvnek .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#vybthmvnek .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#vybthmvnek .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#vybthmvnek .gt_left {
  text-align: left;
}
&#10;#vybthmvnek .gt_center {
  text-align: center;
}
&#10;#vybthmvnek .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#vybthmvnek .gt_font_normal {
  font-weight: normal;
}
&#10;#vybthmvnek .gt_font_bold {
  font-weight: bold;
}
&#10;#vybthmvnek .gt_font_italic {
  font-style: italic;
}
&#10;#vybthmvnek .gt_super {
  font-size: 65%;
}
&#10;#vybthmvnek .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#vybthmvnek .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#vybthmvnek .gt_indent_1 {
  text-indent: 5px;
}
&#10;#vybthmvnek .gt_indent_2 {
  text-indent: 10px;
}
&#10;#vybthmvnek .gt_indent_3 {
  text-indent: 15px;
}
&#10;#vybthmvnek .gt_indent_4 {
  text-indent: 20px;
}
&#10;#vybthmvnek .gt_indent_5 {
  text-indent: 25px;
}
&#10;#vybthmvnek .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#vybthmvnek div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
<thead>
<tr class="gt_col_headings">
<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="survived">
survived
</th>
<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="sex">
sex
</th>
<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="summary">
summary
</th>
<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="display">
display
</th>
</tr>
</thead>
<tbody class="gt_table_body">
<tr>
<td headers="survived" class="gt_row gt_center">
No
</td>
<td headers="sex" class="gt_row gt_center">
Male
</td>
<td headers="summary" class="gt_row gt_right">
170.500
</td>
<td headers="display" class="gt_row gt_right">
170.500
</td>
</tr>
<tr>
<td headers="survived" class="gt_row gt_center">
No
</td>
<td headers="sex" class="gt_row gt_center">
Female
</td>
<td headers="summary" class="gt_row gt_right">
15.750
</td>
<td headers="display" class="gt_row gt_right">
15.750
</td>
</tr>
<tr>
<td headers="survived" class="gt_row gt_center">
Yes
</td>
<td headers="sex" class="gt_row gt_center">
Male
</td>
<td headers="summary" class="gt_row gt_right">
45.875
</td>
<td headers="display" class="gt_row gt_right">
45.875
</td>
</tr>
<tr>
<td headers="survived" class="gt_row gt_center">
Yes
</td>
<td headers="sex" class="gt_row gt_center">
Female
</td>
<td headers="summary" class="gt_row gt_right">
43.000
</td>
<td headers="display" class="gt_row gt_right">
43.000
</td>
</tr>
</tbody>
</table>

</div>

``` r


last_table() |>
  collect()  # returns what would be printed as data frame
```

# A tibble: 4 × 4

survived sex summary display <fct> <fct> <dbl> <dbl> 1 No Male 170. 170.
2 No Female 15.8 15.8 3 Yes Male 45.9 45.9 4 Yes Female 43 43

``` r


ggtable(tidytitanic::flat_titanic) |>
  set_value(freq) |>
  set_rows(sex) |>
  set_fun_example()
```

<div id="wmnvuxyltj"
style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">

<style>#wmnvuxyltj table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#wmnvuxyltj thead, #wmnvuxyltj tbody, #wmnvuxyltj tfoot, #wmnvuxyltj tr, #wmnvuxyltj td, #wmnvuxyltj th {
  border-style: none;
}
&#10;#wmnvuxyltj p {
  margin: 0;
  padding: 0;
}
&#10;#wmnvuxyltj .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}
&#10;#wmnvuxyltj .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#wmnvuxyltj .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#wmnvuxyltj .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#wmnvuxyltj .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#wmnvuxyltj .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#wmnvuxyltj .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#wmnvuxyltj .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}
&#10;#wmnvuxyltj .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#wmnvuxyltj .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#wmnvuxyltj .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#wmnvuxyltj .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#wmnvuxyltj .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#wmnvuxyltj .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}
&#10;#wmnvuxyltj .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}
&#10;#wmnvuxyltj .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#wmnvuxyltj .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#wmnvuxyltj .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#wmnvuxyltj .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#wmnvuxyltj .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#wmnvuxyltj .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#wmnvuxyltj .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#wmnvuxyltj .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#wmnvuxyltj .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#wmnvuxyltj .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#wmnvuxyltj .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#wmnvuxyltj .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#wmnvuxyltj .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#wmnvuxyltj .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#wmnvuxyltj .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#wmnvuxyltj .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#wmnvuxyltj .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#wmnvuxyltj .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#wmnvuxyltj .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#wmnvuxyltj .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#wmnvuxyltj .gt_left {
  text-align: left;
}
&#10;#wmnvuxyltj .gt_center {
  text-align: center;
}
&#10;#wmnvuxyltj .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#wmnvuxyltj .gt_font_normal {
  font-weight: normal;
}
&#10;#wmnvuxyltj .gt_font_bold {
  font-weight: bold;
}
&#10;#wmnvuxyltj .gt_font_italic {
  font-style: italic;
}
&#10;#wmnvuxyltj .gt_super {
  font-size: 65%;
}
&#10;#wmnvuxyltj .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#wmnvuxyltj .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#wmnvuxyltj .gt_indent_1 {
  text-indent: 5px;
}
&#10;#wmnvuxyltj .gt_indent_2 {
  text-indent: 10px;
}
&#10;#wmnvuxyltj .gt_indent_3 {
  text-indent: 15px;
}
&#10;#wmnvuxyltj .gt_indent_4 {
  text-indent: 20px;
}
&#10;#wmnvuxyltj .gt_indent_5 {
  text-indent: 25px;
}
&#10;#wmnvuxyltj .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#wmnvuxyltj div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
<thead>
<tr class="gt_col_headings">
<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="sex">
sex
</th>
<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="value">
value
</th>
</tr>
</thead>
<tbody class="gt_table_body">
<tr>
<td headers="sex" class="gt_row gt_center">
Male
</td>
<td headers="value" class="gt_row gt_right">
13
</td>
</tr>
<tr>
<td headers="sex" class="gt_row gt_center">
Female
</td>
<td headers="value" class="gt_row gt_right">
20
</td>
</tr>
</tbody>
</table>

</div>

``` r

ggtable(tidytitanic::flat_titanic) |>
  set_value(freq) |>
  set_rows(sex) |>
  set_fun_sample(n = 3)
```

<div id="wnunldouvv"
style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">

<style>#wnunldouvv table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#wnunldouvv thead, #wnunldouvv tbody, #wnunldouvv tfoot, #wnunldouvv tr, #wnunldouvv td, #wnunldouvv th {
  border-style: none;
}
&#10;#wnunldouvv p {
  margin: 0;
  padding: 0;
}
&#10;#wnunldouvv .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}
&#10;#wnunldouvv .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#wnunldouvv .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#wnunldouvv .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#wnunldouvv .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#wnunldouvv .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#wnunldouvv .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#wnunldouvv .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}
&#10;#wnunldouvv .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#wnunldouvv .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#wnunldouvv .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#wnunldouvv .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#wnunldouvv .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#wnunldouvv .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}
&#10;#wnunldouvv .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}
&#10;#wnunldouvv .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#wnunldouvv .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#wnunldouvv .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#wnunldouvv .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#wnunldouvv .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#wnunldouvv .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#wnunldouvv .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#wnunldouvv .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#wnunldouvv .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#wnunldouvv .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#wnunldouvv .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#wnunldouvv .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#wnunldouvv .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#wnunldouvv .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#wnunldouvv .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#wnunldouvv .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#wnunldouvv .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#wnunldouvv .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#wnunldouvv .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#wnunldouvv .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#wnunldouvv .gt_left {
  text-align: left;
}
&#10;#wnunldouvv .gt_center {
  text-align: center;
}
&#10;#wnunldouvv .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#wnunldouvv .gt_font_normal {
  font-weight: normal;
}
&#10;#wnunldouvv .gt_font_bold {
  font-weight: bold;
}
&#10;#wnunldouvv .gt_font_italic {
  font-style: italic;
}
&#10;#wnunldouvv .gt_super {
  font-size: 65%;
}
&#10;#wnunldouvv .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#wnunldouvv .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#wnunldouvv .gt_indent_1 {
  text-indent: 5px;
}
&#10;#wnunldouvv .gt_indent_2 {
  text-indent: 10px;
}
&#10;#wnunldouvv .gt_indent_3 {
  text-indent: 15px;
}
&#10;#wnunldouvv .gt_indent_4 {
  text-indent: 20px;
}
&#10;#wnunldouvv .gt_indent_5 {
  text-indent: 25px;
}
&#10;#wnunldouvv .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#wnunldouvv div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
<thead>
<tr class="gt_col_headings">
<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="sex">
sex
</th>
<th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="value">
value
</th>
</tr>
</thead>
<tbody class="gt_table_body">
<tr>
<td headers="sex" class="gt_row gt_center">
Male
</td>
<td headers="value" class="gt_row gt_left">
75; 0; 0
</td>
</tr>
<tr>
<td headers="sex" class="gt_row gt_center">
Female
</td>
<td headers="value" class="gt_row gt_left">
0; 1; 13
</td>
</tr>
</tbody>
</table>

</div>

``` r

ggtable(tidytitanic::flat_titanic) |>
  set_value(freq) |>
  set_rows(sex) |>
  set_fun_list()
```

<div id="mcyknazuhb"
style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">

<style>#mcyknazuhb table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#mcyknazuhb thead, #mcyknazuhb tbody, #mcyknazuhb tfoot, #mcyknazuhb tr, #mcyknazuhb td, #mcyknazuhb th {
  border-style: none;
}
&#10;#mcyknazuhb p {
  margin: 0;
  padding: 0;
}
&#10;#mcyknazuhb .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}
&#10;#mcyknazuhb .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#mcyknazuhb .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#mcyknazuhb .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#mcyknazuhb .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#mcyknazuhb .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#mcyknazuhb .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#mcyknazuhb .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}
&#10;#mcyknazuhb .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#mcyknazuhb .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#mcyknazuhb .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#mcyknazuhb .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#mcyknazuhb .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#mcyknazuhb .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}
&#10;#mcyknazuhb .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}
&#10;#mcyknazuhb .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#mcyknazuhb .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#mcyknazuhb .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#mcyknazuhb .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#mcyknazuhb .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#mcyknazuhb .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#mcyknazuhb .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#mcyknazuhb .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#mcyknazuhb .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#mcyknazuhb .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#mcyknazuhb .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#mcyknazuhb .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#mcyknazuhb .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#mcyknazuhb .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#mcyknazuhb .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#mcyknazuhb .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#mcyknazuhb .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#mcyknazuhb .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#mcyknazuhb .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#mcyknazuhb .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#mcyknazuhb .gt_left {
  text-align: left;
}
&#10;#mcyknazuhb .gt_center {
  text-align: center;
}
&#10;#mcyknazuhb .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#mcyknazuhb .gt_font_normal {
  font-weight: normal;
}
&#10;#mcyknazuhb .gt_font_bold {
  font-weight: bold;
}
&#10;#mcyknazuhb .gt_font_italic {
  font-style: italic;
}
&#10;#mcyknazuhb .gt_super {
  font-size: 65%;
}
&#10;#mcyknazuhb .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#mcyknazuhb .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#mcyknazuhb .gt_indent_1 {
  text-indent: 5px;
}
&#10;#mcyknazuhb .gt_indent_2 {
  text-indent: 10px;
}
&#10;#mcyknazuhb .gt_indent_3 {
  text-indent: 15px;
}
&#10;#mcyknazuhb .gt_indent_4 {
  text-indent: 20px;
}
&#10;#mcyknazuhb .gt_indent_5 {
  text-indent: 25px;
}
&#10;#mcyknazuhb .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#mcyknazuhb div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
<thead>
<tr class="gt_col_headings">
<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="sex">
sex
</th>
<th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="value">
value
</th>
</tr>
</thead>
<tbody class="gt_table_body">
<tr>
<td headers="sex" class="gt_row gt_center">
Male
</td>
<td headers="value" class="gt_row gt_left">
0; 0; 35; 0; 118; 154; 387; 670; 5; 11; 13; 0; 57; 14; 75; 192
</td>
</tr>
<tr>
<td headers="sex" class="gt_row gt_center">
Female
</td>
<td headers="value" class="gt_row gt_left">
0; 0; 17; 0; 4; 13; 89; 3; 1; 13; 14; 0; 140; 80; 76; 20
</td>
</tr>
</tbody>
</table>

</div>

# Packaging

``` r
knitrExtra::chunk_names_get()

knitrExtra::chunk_to_dir("helpers")
knitrExtra::chunk_to_dir("pivotr")
knitrExtra::chunk_to_dir("tidypivot-object")
knitrExtra::chunk_to_dir("ggtable")
knitrExtra::chunk_to_dir("set-rows")
knitrExtra::chunk_to_dir("set-fun")
knitrExtra::chunk_to_dir("set-value")
knitrExtra::chunk_to_dir("set-prop")
knitrExtra::chunk_to_dir("no-pivot")
```

``` r
devtools::document()
devtools::check()
devtools::install(pkg = ".", upgrade = "never") 
```

# ggplot2 extension case study…

``` r
library(tidyverse)

ext_exports <- read_csv("https://raw.githubusercontent.com/EvaMaeRey/mytidytuesday/refs/heads/main/2024-11-19-gg-prefixes/exported_funs_exts_ggplot2_tidyverse_org.csv") |> 
  mutate(prefix = str_extract(fun_exported, ".*?_")) |> 
  mutate(prefix_long = str_extract(fun_exported, ".+_")) |> 
  mutate(ind_classic_prefix = prefix %in% c("stat_", "geom_", "theme_", "scale_", "coord_", "facet_"))


ggtable(ext_exports)
```

<div id="ycwkoftdmq"
style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">

<style>#ycwkoftdmq table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#ycwkoftdmq thead, #ycwkoftdmq tbody, #ycwkoftdmq tfoot, #ycwkoftdmq tr, #ycwkoftdmq td, #ycwkoftdmq th {
  border-style: none;
}
&#10;#ycwkoftdmq p {
  margin: 0;
  padding: 0;
}
&#10;#ycwkoftdmq .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}
&#10;#ycwkoftdmq .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#ycwkoftdmq .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#ycwkoftdmq .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#ycwkoftdmq .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#ycwkoftdmq .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#ycwkoftdmq .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#ycwkoftdmq .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}
&#10;#ycwkoftdmq .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#ycwkoftdmq .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#ycwkoftdmq .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#ycwkoftdmq .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#ycwkoftdmq .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#ycwkoftdmq .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}
&#10;#ycwkoftdmq .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}
&#10;#ycwkoftdmq .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#ycwkoftdmq .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#ycwkoftdmq .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#ycwkoftdmq .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ycwkoftdmq .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#ycwkoftdmq .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#ycwkoftdmq .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#ycwkoftdmq .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ycwkoftdmq .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#ycwkoftdmq .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#ycwkoftdmq .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#ycwkoftdmq .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ycwkoftdmq .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#ycwkoftdmq .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#ycwkoftdmq .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#ycwkoftdmq .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#ycwkoftdmq .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#ycwkoftdmq .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ycwkoftdmq .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#ycwkoftdmq .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ycwkoftdmq .gt_left {
  text-align: left;
}
&#10;#ycwkoftdmq .gt_center {
  text-align: center;
}
&#10;#ycwkoftdmq .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#ycwkoftdmq .gt_font_normal {
  font-weight: normal;
}
&#10;#ycwkoftdmq .gt_font_bold {
  font-weight: bold;
}
&#10;#ycwkoftdmq .gt_font_italic {
  font-style: italic;
}
&#10;#ycwkoftdmq .gt_super {
  font-size: 65%;
}
&#10;#ycwkoftdmq .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#ycwkoftdmq .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#ycwkoftdmq .gt_indent_1 {
  text-indent: 5px;
}
&#10;#ycwkoftdmq .gt_indent_2 {
  text-indent: 10px;
}
&#10;#ycwkoftdmq .gt_indent_3 {
  text-indent: 15px;
}
&#10;#ycwkoftdmq .gt_indent_4 {
  text-indent: 20px;
}
&#10;#ycwkoftdmq .gt_indent_5 {
  text-indent: 25px;
}
&#10;#ycwkoftdmq .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#ycwkoftdmq div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
<thead>
<tr class="gt_col_headings">
<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="value">
value
</th>
</tr>
</thead>
<tbody class="gt_table_body">
<tr>
<td headers="value" class="gt_row gt_right">
5527
</td>
</tr>
</tbody>
</table>

</div>

``` r

ggtable(ext_exports |> filter(ind_classic_prefix))
```

<div id="oivfmnounx"
style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">

<style>#oivfmnounx table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#oivfmnounx thead, #oivfmnounx tbody, #oivfmnounx tfoot, #oivfmnounx tr, #oivfmnounx td, #oivfmnounx th {
  border-style: none;
}
&#10;#oivfmnounx p {
  margin: 0;
  padding: 0;
}
&#10;#oivfmnounx .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}
&#10;#oivfmnounx .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#oivfmnounx .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#oivfmnounx .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#oivfmnounx .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#oivfmnounx .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#oivfmnounx .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#oivfmnounx .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}
&#10;#oivfmnounx .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#oivfmnounx .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#oivfmnounx .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#oivfmnounx .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#oivfmnounx .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#oivfmnounx .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}
&#10;#oivfmnounx .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}
&#10;#oivfmnounx .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#oivfmnounx .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#oivfmnounx .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#oivfmnounx .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#oivfmnounx .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#oivfmnounx .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#oivfmnounx .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#oivfmnounx .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#oivfmnounx .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#oivfmnounx .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#oivfmnounx .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#oivfmnounx .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#oivfmnounx .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#oivfmnounx .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#oivfmnounx .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#oivfmnounx .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#oivfmnounx .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#oivfmnounx .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#oivfmnounx .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#oivfmnounx .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#oivfmnounx .gt_left {
  text-align: left;
}
&#10;#oivfmnounx .gt_center {
  text-align: center;
}
&#10;#oivfmnounx .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#oivfmnounx .gt_font_normal {
  font-weight: normal;
}
&#10;#oivfmnounx .gt_font_bold {
  font-weight: bold;
}
&#10;#oivfmnounx .gt_font_italic {
  font-style: italic;
}
&#10;#oivfmnounx .gt_super {
  font-size: 65%;
}
&#10;#oivfmnounx .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#oivfmnounx .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#oivfmnounx .gt_indent_1 {
  text-indent: 5px;
}
&#10;#oivfmnounx .gt_indent_2 {
  text-indent: 10px;
}
&#10;#oivfmnounx .gt_indent_3 {
  text-indent: 15px;
}
&#10;#oivfmnounx .gt_indent_4 {
  text-indent: 20px;
}
&#10;#oivfmnounx .gt_indent_5 {
  text-indent: 25px;
}
&#10;#oivfmnounx .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#oivfmnounx div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
<thead>
<tr class="gt_col_headings">
<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="value">
value
</th>
</tr>
</thead>
<tbody class="gt_table_body">
<tr>
<td headers="value" class="gt_row gt_right">
1992
</td>
</tr>
</tbody>
</table>

</div>

``` r

last_table() |>
  set_rows(user) 
```

<div id="upclevqvhk"
style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">

<style>#upclevqvhk table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#upclevqvhk thead, #upclevqvhk tbody, #upclevqvhk tfoot, #upclevqvhk tr, #upclevqvhk td, #upclevqvhk th {
  border-style: none;
}
&#10;#upclevqvhk p {
  margin: 0;
  padding: 0;
}
&#10;#upclevqvhk .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}
&#10;#upclevqvhk .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#upclevqvhk .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#upclevqvhk .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#upclevqvhk .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#upclevqvhk .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#upclevqvhk .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#upclevqvhk .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}
&#10;#upclevqvhk .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#upclevqvhk .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#upclevqvhk .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#upclevqvhk .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#upclevqvhk .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#upclevqvhk .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}
&#10;#upclevqvhk .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}
&#10;#upclevqvhk .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#upclevqvhk .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#upclevqvhk .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#upclevqvhk .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#upclevqvhk .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#upclevqvhk .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#upclevqvhk .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#upclevqvhk .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#upclevqvhk .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#upclevqvhk .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#upclevqvhk .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#upclevqvhk .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#upclevqvhk .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#upclevqvhk .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#upclevqvhk .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#upclevqvhk .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#upclevqvhk .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#upclevqvhk .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#upclevqvhk .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#upclevqvhk .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#upclevqvhk .gt_left {
  text-align: left;
}
&#10;#upclevqvhk .gt_center {
  text-align: center;
}
&#10;#upclevqvhk .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#upclevqvhk .gt_font_normal {
  font-weight: normal;
}
&#10;#upclevqvhk .gt_font_bold {
  font-weight: bold;
}
&#10;#upclevqvhk .gt_font_italic {
  font-style: italic;
}
&#10;#upclevqvhk .gt_super {
  font-size: 65%;
}
&#10;#upclevqvhk .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#upclevqvhk .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#upclevqvhk .gt_indent_1 {
  text-indent: 5px;
}
&#10;#upclevqvhk .gt_indent_2 {
  text-indent: 10px;
}
&#10;#upclevqvhk .gt_indent_3 {
  text-indent: 15px;
}
&#10;#upclevqvhk .gt_indent_4 {
  text-indent: 20px;
}
&#10;#upclevqvhk .gt_indent_5 {
  text-indent: 25px;
}
&#10;#upclevqvhk .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#upclevqvhk div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
<thead>
<tr class="gt_col_headings">
<th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="user">
user
</th>
<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="value">
value
</th>
</tr>
</thead>
<tbody class="gt_table_body">
<tr>
<td headers="user" class="gt_row gt_left">
AckerDWM
</td>
<td headers="value" class="gt_row gt_right">
2
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
AllanCameron
</td>
<td headers="value" class="gt_row gt_right">
32
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
Ather-Energy
</td>
<td headers="value" class="gt_row gt_right">
6
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
IndrajeetPatil
</td>
<td headers="value" class="gt_row gt_right">
1
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
LCBC-UiO
</td>
<td headers="value" class="gt_row gt_right">
34
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
LKremer
</td>
<td headers="value" class="gt_row gt_right">
1
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
ProjectMOSAIC
</td>
<td headers="value" class="gt_row gt_right">
8
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
PursuitOfDataScience
</td>
<td headers="value" class="gt_row gt_right">
4
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
Ryo-N7
</td>
<td headers="value" class="gt_row gt_right">
54
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
Selbosh
</td>
<td headers="value" class="gt_row gt_right">
7
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
YuLab-SMU
</td>
<td headers="value" class="gt_row gt_right">
5
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
Yunuuuu
</td>
<td headers="value" class="gt_row gt_right">
7
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
aloy
</td>
<td headers="value" class="gt_row gt_right">
7
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
aphalo
</td>
<td headers="value" class="gt_row gt_right">
157
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
benskov
</td>
<td headers="value" class="gt_row gt_right">
3
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
briatte
</td>
<td headers="value" class="gt_row gt_right">
12
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
cgoo4
</td>
<td headers="value" class="gt_row gt_right">
1
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
cidm-ph
</td>
<td headers="value" class="gt_row gt_right">
7
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
clauswilke
</td>
<td headers="value" class="gt_row gt_right">
44
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
const-ae
</td>
<td headers="value" class="gt_row gt_right">
5
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
corybrunson
</td>
<td headers="value" class="gt_row gt_right">
7
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
cttobin
</td>
<td headers="value" class="gt_row gt_right">
1
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
davidchall
</td>
<td headers="value" class="gt_row gt_right">
5
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
davidgohel
</td>
<td headers="value" class="gt_row gt_right">
146
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
davidsjoberg
</td>
<td headers="value" class="gt_row gt_right">
2
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
dieghernan
</td>
<td headers="value" class="gt_row gt_right">
95
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
dzhang32
</td>
<td headers="value" class="gt_row gt_right">
5
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
earowang
</td>
<td headers="value" class="gt_row gt_right">
3
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
easystats
</td>
<td headers="value" class="gt_row gt_right">
99
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
eclarke
</td>
<td headers="value" class="gt_row gt_right">
2
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
erocoar
</td>
<td headers="value" class="gt_row gt_right">
18
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
ggobi
</td>
<td headers="value" class="gt_row gt_right">
6
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
graysonwhite
</td>
<td headers="value" class="gt_row gt_right">
7
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
hafen
</td>
<td headers="value" class="gt_row gt_right">
1
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
haleyjeppson
</td>
<td headers="value" class="gt_row gt_right">
9
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
hrbrmstr
</td>
<td headers="value" class="gt_row gt_right">
37
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
hughjonesd
</td>
<td headers="value" class="gt_row gt_right">
2
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
jamesotto852
</td>
<td headers="value" class="gt_row gt_right">
16
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
jbengler
</td>
<td headers="value" class="gt_row gt_right">
5
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
jhrcook
</td>
<td headers="value" class="gt_row gt_right">
13
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
jjchern
</td>
<td headers="value" class="gt_row gt_right">
2
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
jrnold
</td>
<td headers="value" class="gt_row gt_right">
89
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
jtlandis
</td>
<td headers="value" class="gt_row gt_right">
93
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
kassambara
</td>
<td headers="value" class="gt_row gt_right">
27
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
kenithgrey
</td>
<td headers="value" class="gt_row gt_right">
11
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
krassowski
</td>
<td headers="value" class="gt_row gt_right">
6
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
larmarange
</td>
<td headers="value" class="gt_row gt_right">
16
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
lepennec
</td>
<td headers="value" class="gt_row gt_right">
2
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
lionel-
</td>
<td headers="value" class="gt_row gt_right">
14
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
malcolmbarrett
</td>
<td headers="value" class="gt_row gt_right">
23
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
marcmenem
</td>
<td headers="value" class="gt_row gt_right">
30
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
martin-borkovec
</td>
<td headers="value" class="gt_row gt_right">
6
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
mdhall272
</td>
<td headers="value" class="gt_row gt_right">
1
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
metrumresearchgroup
</td>
<td headers="value" class="gt_row gt_right">
1
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
mikabr
</td>
<td headers="value" class="gt_row gt_right">
1
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
mikmart
</td>
<td headers="value" class="gt_row gt_right">
2
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
mitchelloharawild
</td>
<td headers="value" class="gt_row gt_right">
2
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
mivalek
</td>
<td headers="value" class="gt_row gt_right">
1
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
mjskay
</td>
<td headers="value" class="gt_row gt_right">
90
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
msberends
</td>
<td headers="value" class="gt_row gt_right">
9
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
nflverse
</td>
<td headers="value" class="gt_row gt_right">
9
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
njudd
</td>
<td headers="value" class="gt_row gt_right">
1
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
nsgrantham
</td>
<td headers="value" class="gt_row gt_right">
2
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
oldlipe
</td>
<td headers="value" class="gt_row gt_right">
1
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
omarwagih
</td>
<td headers="value" class="gt_row gt_right">
2
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
piecepackr
</td>
<td headers="value" class="gt_row gt_right">
3
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
rcorty
</td>
<td headers="value" class="gt_row gt_right">
3
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
ricardo-bion
</td>
<td headers="value" class="gt_row gt_right">
5
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
rivasiker
</td>
<td headers="value" class="gt_row gt_right">
3
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
rnabioco
</td>
<td headers="value" class="gt_row gt_right">
4
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
road2stat
</td>
<td headers="value" class="gt_row gt_right">
75
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
sachsmc
</td>
<td headers="value" class="gt_row gt_right">
4
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
sctyner
</td>
<td headers="value" class="gt_row gt_right">
4
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
slowkow
</td>
<td headers="value" class="gt_row gt_right">
2
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
statsmaths
</td>
<td headers="value" class="gt_row gt_right">
2
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
stefanedwards
</td>
<td headers="value" class="gt_row gt_right">
12
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
teunbrand
</td>
<td headers="value" class="gt_row gt_right">
73
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
thackl
</td>
<td headers="value" class="gt_row gt_right">
24
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
thomas-neitmann
</td>
<td headers="value" class="gt_row gt_right">
5
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
thomasp85
</td>
<td headers="value" class="gt_row gt_right">
231
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
trevorld
</td>
<td headers="value" class="gt_row gt_right">
181
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
vpetukhov
</td>
<td headers="value" class="gt_row gt_right">
8
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
wilkelab
</td>
<td headers="value" class="gt_row gt_right">
7
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
wilkox
</td>
<td headers="value" class="gt_row gt_right">
20
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
xiangpin
</td>
<td headers="value" class="gt_row gt_right">
8
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
yanpd01
</td>
<td headers="value" class="gt_row gt_right">
1
</td>
</tr>
</tbody>
</table>

</div>

``` r

last_table() |>
  set_cols(prefix)
```

<div id="lnrzxshuzy"
style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">

<style>#lnrzxshuzy table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#lnrzxshuzy thead, #lnrzxshuzy tbody, #lnrzxshuzy tfoot, #lnrzxshuzy tr, #lnrzxshuzy td, #lnrzxshuzy th {
  border-style: none;
}
&#10;#lnrzxshuzy p {
  margin: 0;
  padding: 0;
}
&#10;#lnrzxshuzy .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}
&#10;#lnrzxshuzy .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#lnrzxshuzy .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#lnrzxshuzy .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#lnrzxshuzy .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#lnrzxshuzy .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#lnrzxshuzy .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#lnrzxshuzy .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}
&#10;#lnrzxshuzy .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#lnrzxshuzy .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#lnrzxshuzy .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#lnrzxshuzy .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#lnrzxshuzy .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#lnrzxshuzy .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}
&#10;#lnrzxshuzy .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}
&#10;#lnrzxshuzy .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#lnrzxshuzy .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#lnrzxshuzy .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#lnrzxshuzy .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#lnrzxshuzy .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#lnrzxshuzy .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#lnrzxshuzy .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#lnrzxshuzy .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#lnrzxshuzy .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#lnrzxshuzy .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#lnrzxshuzy .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#lnrzxshuzy .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#lnrzxshuzy .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#lnrzxshuzy .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#lnrzxshuzy .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#lnrzxshuzy .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#lnrzxshuzy .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#lnrzxshuzy .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#lnrzxshuzy .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#lnrzxshuzy .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#lnrzxshuzy .gt_left {
  text-align: left;
}
&#10;#lnrzxshuzy .gt_center {
  text-align: center;
}
&#10;#lnrzxshuzy .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#lnrzxshuzy .gt_font_normal {
  font-weight: normal;
}
&#10;#lnrzxshuzy .gt_font_bold {
  font-weight: bold;
}
&#10;#lnrzxshuzy .gt_font_italic {
  font-style: italic;
}
&#10;#lnrzxshuzy .gt_super {
  font-size: 65%;
}
&#10;#lnrzxshuzy .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#lnrzxshuzy .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#lnrzxshuzy .gt_indent_1 {
  text-indent: 5px;
}
&#10;#lnrzxshuzy .gt_indent_2 {
  text-indent: 10px;
}
&#10;#lnrzxshuzy .gt_indent_3 {
  text-indent: 15px;
}
&#10;#lnrzxshuzy .gt_indent_4 {
  text-indent: 20px;
}
&#10;#lnrzxshuzy .gt_indent_5 {
  text-indent: 25px;
}
&#10;#lnrzxshuzy .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#lnrzxshuzy div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
<thead>
<tr class="gt_col_headings">
<th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="user">
user
</th>
<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="coord_">
coord\_
</th>
<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="facet_">
facet\_
</th>
<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="geom_">
geom\_
</th>
<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="scale_">
scale\_
</th>
<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="stat_">
stat\_
</th>
<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="theme_">
theme\_
</th>
</tr>
</thead>
<tbody class="gt_table_body">
<tr>
<td headers="user" class="gt_row gt_left">
AllanCameron
</td>
<td headers="coord_" class="gt_row gt_right">
1
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
24
</td>
<td headers="scale_" class="gt_row gt_right">
6
</td>
<td headers="stat_" class="gt_row gt_right">
1
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
cidm-ph
</td>
<td headers="coord_" class="gt_row gt_right">
1
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
4
</td>
<td headers="scale_" class="gt_row gt_right">
NA
</td>
<td headers="stat_" class="gt_row gt_right">
2
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
davidchall
</td>
<td headers="coord_" class="gt_row gt_right">
1
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
1
</td>
<td headers="scale_" class="gt_row gt_right">
NA
</td>
<td headers="stat_" class="gt_row gt_right">
1
</td>
<td headers="theme_" class="gt_row gt_right">
2
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
easystats
</td>
<td headers="coord_" class="gt_row gt_right">
1
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
14
</td>
<td headers="scale_" class="gt_row gt_right">
78
</td>
<td headers="stat_" class="gt_row gt_right">
NA
</td>
<td headers="theme_" class="gt_row gt_right">
6
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
hrbrmstr
</td>
<td headers="coord_" class="gt_row gt_right">
1
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
11
</td>
<td headers="scale_" class="gt_row gt_right">
10
</td>
<td headers="stat_" class="gt_row gt_right">
6
</td>
<td headers="theme_" class="gt_row gt_right">
9
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
stefanedwards
</td>
<td headers="coord_" class="gt_row gt_right">
5
</td>
<td headers="facet_" class="gt_row gt_right">
2
</td>
<td headers="geom_" class="gt_row gt_right">
3
</td>
<td headers="scale_" class="gt_row gt_right">
2
</td>
<td headers="stat_" class="gt_row gt_right">
NA
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
teunbrand
</td>
<td headers="coord_" class="gt_row gt_right">
2
</td>
<td headers="facet_" class="gt_row gt_right">
5
</td>
<td headers="geom_" class="gt_row gt_right">
11
</td>
<td headers="scale_" class="gt_row gt_right">
47
</td>
<td headers="stat_" class="gt_row gt_right">
7
</td>
<td headers="theme_" class="gt_row gt_right">
1
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
davidgohel
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
2
</td>
<td headers="geom_" class="gt_row gt_right">
50
</td>
<td headers="scale_" class="gt_row gt_right">
94
</td>
<td headers="stat_" class="gt_row gt_right">
NA
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
earowang
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
1
</td>
<td headers="geom_" class="gt_row gt_right">
1
</td>
<td headers="scale_" class="gt_row gt_right">
NA
</td>
<td headers="stat_" class="gt_row gt_right">
1
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
erocoar
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
1
</td>
<td headers="geom_" class="gt_row gt_right">
12
</td>
<td headers="scale_" class="gt_row gt_right">
NA
</td>
<td headers="stat_" class="gt_row gt_right">
5
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
hafen
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
1
</td>
<td headers="geom_" class="gt_row gt_right">
NA
</td>
<td headers="scale_" class="gt_row gt_right">
NA
</td>
<td headers="stat_" class="gt_row gt_right">
NA
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
mikmart
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
2
</td>
<td headers="geom_" class="gt_row gt_right">
NA
</td>
<td headers="scale_" class="gt_row gt_right">
NA
</td>
<td headers="stat_" class="gt_row gt_right">
NA
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
mjskay
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
3
</td>
<td headers="geom_" class="gt_row gt_right">
11
</td>
<td headers="scale_" class="gt_row gt_right">
45
</td>
<td headers="stat_" class="gt_row gt_right">
29
</td>
<td headers="theme_" class="gt_row gt_right">
2
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
msberends
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
1
</td>
<td headers="geom_" class="gt_row gt_right">
1
</td>
<td headers="scale_" class="gt_row gt_right">
6
</td>
<td headers="stat_" class="gt_row gt_right">
NA
</td>
<td headers="theme_" class="gt_row gt_right">
1
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
thomasp85
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
10
</td>
<td headers="geom_" class="gt_row gt_right">
95
</td>
<td headers="scale_" class="gt_row gt_right">
97
</td>
<td headers="stat_" class="gt_row gt_right">
27
</td>
<td headers="theme_" class="gt_row gt_right">
2
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
LCBC-UiO
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
2
</td>
<td headers="scale_" class="gt_row gt_right">
24
</td>
<td headers="stat_" class="gt_row gt_right">
NA
</td>
<td headers="theme_" class="gt_row gt_right">
8
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
LKremer
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
1
</td>
<td headers="scale_" class="gt_row gt_right">
NA
</td>
<td headers="stat_" class="gt_row gt_right">
NA
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
ProjectMOSAIC
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
3
</td>
<td headers="scale_" class="gt_row gt_right">
NA
</td>
<td headers="stat_" class="gt_row gt_right">
5
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
PursuitOfDataScience
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
3
</td>
<td headers="scale_" class="gt_row gt_right">
NA
</td>
<td headers="stat_" class="gt_row gt_right">
NA
</td>
<td headers="theme_" class="gt_row gt_right">
1
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
Selbosh
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
1
</td>
<td headers="scale_" class="gt_row gt_right">
6
</td>
<td headers="stat_" class="gt_row gt_right">
NA
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
Yunuuuu
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
4
</td>
<td headers="scale_" class="gt_row gt_right">
1
</td>
<td headers="stat_" class="gt_row gt_right">
NA
</td>
<td headers="theme_" class="gt_row gt_right">
2
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
aloy
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
1
</td>
<td headers="scale_" class="gt_row gt_right">
NA
</td>
<td headers="stat_" class="gt_row gt_right">
6
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
aphalo
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
43
</td>
<td headers="scale_" class="gt_row gt_right">
44
</td>
<td headers="stat_" class="gt_row gt_right">
70
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
benskov
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
3
</td>
<td headers="scale_" class="gt_row gt_right">
NA
</td>
<td headers="stat_" class="gt_row gt_right">
NA
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
briatte
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
10
</td>
<td headers="scale_" class="gt_row gt_right">
NA
</td>
<td headers="stat_" class="gt_row gt_right">
NA
</td>
<td headers="theme_" class="gt_row gt_right">
2
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
cgoo4
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
1
</td>
<td headers="scale_" class="gt_row gt_right">
NA
</td>
<td headers="stat_" class="gt_row gt_right">
NA
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
clauswilke
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
9
</td>
<td headers="scale_" class="gt_row gt_right">
32
</td>
<td headers="stat_" class="gt_row gt_right">
2
</td>
<td headers="theme_" class="gt_row gt_right">
1
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
const-ae
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
1
</td>
<td headers="scale_" class="gt_row gt_right">
2
</td>
<td headers="stat_" class="gt_row gt_right">
1
</td>
<td headers="theme_" class="gt_row gt_right">
1
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
corybrunson
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
4
</td>
<td headers="scale_" class="gt_row gt_right">
NA
</td>
<td headers="stat_" class="gt_row gt_right">
3
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
davidsjoberg
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
2
</td>
<td headers="scale_" class="gt_row gt_right">
NA
</td>
<td headers="stat_" class="gt_row gt_right">
NA
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
dieghernan
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
8
</td>
<td headers="scale_" class="gt_row gt_right">
84
</td>
<td headers="stat_" class="gt_row gt_right">
3
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
dzhang32
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
5
</td>
<td headers="scale_" class="gt_row gt_right">
NA
</td>
<td headers="stat_" class="gt_row gt_right">
NA
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
eclarke
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
2
</td>
<td headers="scale_" class="gt_row gt_right">
NA
</td>
<td headers="stat_" class="gt_row gt_right">
NA
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
ggobi
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
2
</td>
<td headers="scale_" class="gt_row gt_right">
NA
</td>
<td headers="stat_" class="gt_row gt_right">
4
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
haleyjeppson
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
3
</td>
<td headers="scale_" class="gt_row gt_right">
2
</td>
<td headers="stat_" class="gt_row gt_right">
3
</td>
<td headers="theme_" class="gt_row gt_right">
1
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
hughjonesd
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
2
</td>
<td headers="scale_" class="gt_row gt_right">
NA
</td>
<td headers="stat_" class="gt_row gt_right">
NA
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
jamesotto852
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
8
</td>
<td headers="scale_" class="gt_row gt_right">
NA
</td>
<td headers="stat_" class="gt_row gt_right">
8
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
jhrcook
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
1
</td>
<td headers="scale_" class="gt_row gt_right">
12
</td>
<td headers="stat_" class="gt_row gt_right">
NA
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
jrnold
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
2
</td>
<td headers="scale_" class="gt_row gt_right">
64
</td>
<td headers="stat_" class="gt_row gt_right">
1
</td>
<td headers="theme_" class="gt_row gt_right">
22
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
jtlandis
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
36
</td>
<td headers="scale_" class="gt_row gt_right">
44
</td>
<td headers="stat_" class="gt_row gt_right">
4
</td>
<td headers="theme_" class="gt_row gt_right">
9
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
kassambara
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
4
</td>
<td headers="scale_" class="gt_row gt_right">
NA
</td>
<td headers="stat_" class="gt_row gt_right">
16
</td>
<td headers="theme_" class="gt_row gt_right">
7
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
krassowski
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
4
</td>
<td headers="scale_" class="gt_row gt_right">
2
</td>
<td headers="stat_" class="gt_row gt_right">
NA
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
larmarange
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
10
</td>
<td headers="scale_" class="gt_row gt_right">
3
</td>
<td headers="stat_" class="gt_row gt_right">
3
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
lepennec
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
2
</td>
<td headers="scale_" class="gt_row gt_right">
NA
</td>
<td headers="stat_" class="gt_row gt_right">
NA
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
lionel-
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
9
</td>
<td headers="scale_" class="gt_row gt_right">
NA
</td>
<td headers="stat_" class="gt_row gt_right">
5
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
malcolmbarrett
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
14
</td>
<td headers="scale_" class="gt_row gt_right">
2
</td>
<td headers="stat_" class="gt_row gt_right">
NA
</td>
<td headers="theme_" class="gt_row gt_right">
7
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
marcmenem
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
8
</td>
<td headers="scale_" class="gt_row gt_right">
22
</td>
<td headers="stat_" class="gt_row gt_right">
NA
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
martin-borkovec
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
6
</td>
<td headers="scale_" class="gt_row gt_right">
NA
</td>
<td headers="stat_" class="gt_row gt_right">
NA
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
mdhall272
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
1
</td>
<td headers="scale_" class="gt_row gt_right">
NA
</td>
<td headers="stat_" class="gt_row gt_right">
NA
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
metrumresearchgroup
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
1
</td>
<td headers="scale_" class="gt_row gt_right">
NA
</td>
<td headers="stat_" class="gt_row gt_right">
NA
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
mikabr
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
1
</td>
<td headers="scale_" class="gt_row gt_right">
NA
</td>
<td headers="stat_" class="gt_row gt_right">
NA
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
mitchelloharawild
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
1
</td>
<td headers="scale_" class="gt_row gt_right">
NA
</td>
<td headers="stat_" class="gt_row gt_right">
1
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
mivalek
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
1
</td>
<td headers="scale_" class="gt_row gt_right">
NA
</td>
<td headers="stat_" class="gt_row gt_right">
NA
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
nflverse
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
6
</td>
<td headers="scale_" class="gt_row gt_right">
3
</td>
<td headers="stat_" class="gt_row gt_right">
NA
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
njudd
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
1
</td>
<td headers="scale_" class="gt_row gt_right">
NA
</td>
<td headers="stat_" class="gt_row gt_right">
NA
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
nsgrantham
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
1
</td>
<td headers="scale_" class="gt_row gt_right">
NA
</td>
<td headers="stat_" class="gt_row gt_right">
1
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
oldlipe
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
1
</td>
<td headers="scale_" class="gt_row gt_right">
NA
</td>
<td headers="stat_" class="gt_row gt_right">
NA
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
omarwagih
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
1
</td>
<td headers="scale_" class="gt_row gt_right">
NA
</td>
<td headers="stat_" class="gt_row gt_right">
NA
</td>
<td headers="theme_" class="gt_row gt_right">
1
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
piecepackr
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
1
</td>
<td headers="scale_" class="gt_row gt_right">
2
</td>
<td headers="stat_" class="gt_row gt_right">
NA
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
rcorty
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
1
</td>
<td headers="scale_" class="gt_row gt_right">
2
</td>
<td headers="stat_" class="gt_row gt_right">
NA
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
ricardo-bion
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
1
</td>
<td headers="scale_" class="gt_row gt_right">
2
</td>
<td headers="stat_" class="gt_row gt_right">
NA
</td>
<td headers="theme_" class="gt_row gt_right">
2
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
rivasiker
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
1
</td>
<td headers="scale_" class="gt_row gt_right">
1
</td>
<td headers="stat_" class="gt_row gt_right">
1
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
rnabioco
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
4
</td>
<td headers="scale_" class="gt_row gt_right">
NA
</td>
<td headers="stat_" class="gt_row gt_right">
NA
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
sachsmc
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
2
</td>
<td headers="scale_" class="gt_row gt_right">
NA
</td>
<td headers="stat_" class="gt_row gt_right">
2
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
sctyner
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
2
</td>
<td headers="scale_" class="gt_row gt_right">
NA
</td>
<td headers="stat_" class="gt_row gt_right">
1
</td>
<td headers="theme_" class="gt_row gt_right">
1
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
slowkow
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
2
</td>
<td headers="scale_" class="gt_row gt_right">
NA
</td>
<td headers="stat_" class="gt_row gt_right">
NA
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
statsmaths
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
2
</td>
<td headers="scale_" class="gt_row gt_right">
NA
</td>
<td headers="stat_" class="gt_row gt_right">
NA
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
thackl
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
20
</td>
<td headers="scale_" class="gt_row gt_right">
3
</td>
<td headers="stat_" class="gt_row gt_right">
NA
</td>
<td headers="theme_" class="gt_row gt_right">
1
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
trevorld
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
18
</td>
<td headers="scale_" class="gt_row gt_right">
163
</td>
<td headers="stat_" class="gt_row gt_right">
NA
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
vpetukhov
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
7
</td>
<td headers="scale_" class="gt_row gt_right">
NA
</td>
<td headers="stat_" class="gt_row gt_right">
NA
</td>
<td headers="theme_" class="gt_row gt_right">
1
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
wilkox
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
18
</td>
<td headers="scale_" class="gt_row gt_right">
NA
</td>
<td headers="stat_" class="gt_row gt_right">
NA
</td>
<td headers="theme_" class="gt_row gt_right">
2
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
xiangpin
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
1
</td>
<td headers="scale_" class="gt_row gt_right">
7
</td>
<td headers="stat_" class="gt_row gt_right">
NA
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
yanpd01
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
1
</td>
<td headers="scale_" class="gt_row gt_right">
NA
</td>
<td headers="stat_" class="gt_row gt_right">
NA
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
Ryo-N7
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
NA
</td>
<td headers="scale_" class="gt_row gt_right">
42
</td>
<td headers="stat_" class="gt_row gt_right">
NA
</td>
<td headers="theme_" class="gt_row gt_right">
12
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
YuLab-SMU
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
NA
</td>
<td headers="scale_" class="gt_row gt_right">
5
</td>
<td headers="stat_" class="gt_row gt_right">
NA
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
cttobin
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
NA
</td>
<td headers="scale_" class="gt_row gt_right">
1
</td>
<td headers="stat_" class="gt_row gt_right">
NA
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
road2stat
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
NA
</td>
<td headers="scale_" class="gt_row gt_right">
75
</td>
<td headers="stat_" class="gt_row gt_right">
NA
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
AckerDWM
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
NA
</td>
<td headers="scale_" class="gt_row gt_right">
NA
</td>
<td headers="stat_" class="gt_row gt_right">
2
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
Ather-Energy
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
NA
</td>
<td headers="scale_" class="gt_row gt_right">
NA
</td>
<td headers="stat_" class="gt_row gt_right">
6
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
graysonwhite
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
NA
</td>
<td headers="scale_" class="gt_row gt_right">
NA
</td>
<td headers="stat_" class="gt_row gt_right">
7
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
jjchern
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
NA
</td>
<td headers="scale_" class="gt_row gt_right">
NA
</td>
<td headers="stat_" class="gt_row gt_right">
2
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
kenithgrey
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
NA
</td>
<td headers="scale_" class="gt_row gt_right">
NA
</td>
<td headers="stat_" class="gt_row gt_right">
11
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
IndrajeetPatil
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
NA
</td>
<td headers="scale_" class="gt_row gt_right">
NA
</td>
<td headers="stat_" class="gt_row gt_right">
NA
</td>
<td headers="theme_" class="gt_row gt_right">
1
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
jbengler
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
NA
</td>
<td headers="scale_" class="gt_row gt_right">
NA
</td>
<td headers="stat_" class="gt_row gt_right">
NA
</td>
<td headers="theme_" class="gt_row gt_right">
5
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
thomas-neitmann
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
NA
</td>
<td headers="scale_" class="gt_row gt_right">
NA
</td>
<td headers="stat_" class="gt_row gt_right">
NA
</td>
<td headers="theme_" class="gt_row gt_right">
5
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
wilkelab
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
NA
</td>
<td headers="scale_" class="gt_row gt_right">
NA
</td>
<td headers="stat_" class="gt_row gt_right">
NA
</td>
<td headers="theme_" class="gt_row gt_right">
7
</td>
</tr>
</tbody>
</table>

</div>

``` r

last_table() |>
  set_rows(c(user, repo))
```

<div id="glxpqxfgaw"
style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">

<style>#glxpqxfgaw table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#glxpqxfgaw thead, #glxpqxfgaw tbody, #glxpqxfgaw tfoot, #glxpqxfgaw tr, #glxpqxfgaw td, #glxpqxfgaw th {
  border-style: none;
}
&#10;#glxpqxfgaw p {
  margin: 0;
  padding: 0;
}
&#10;#glxpqxfgaw .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}
&#10;#glxpqxfgaw .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#glxpqxfgaw .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#glxpqxfgaw .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#glxpqxfgaw .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#glxpqxfgaw .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#glxpqxfgaw .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#glxpqxfgaw .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}
&#10;#glxpqxfgaw .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#glxpqxfgaw .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#glxpqxfgaw .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#glxpqxfgaw .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#glxpqxfgaw .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#glxpqxfgaw .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}
&#10;#glxpqxfgaw .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}
&#10;#glxpqxfgaw .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#glxpqxfgaw .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#glxpqxfgaw .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#glxpqxfgaw .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#glxpqxfgaw .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#glxpqxfgaw .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#glxpqxfgaw .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#glxpqxfgaw .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#glxpqxfgaw .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#glxpqxfgaw .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#glxpqxfgaw .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#glxpqxfgaw .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#glxpqxfgaw .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#glxpqxfgaw .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#glxpqxfgaw .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#glxpqxfgaw .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#glxpqxfgaw .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#glxpqxfgaw .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#glxpqxfgaw .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#glxpqxfgaw .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#glxpqxfgaw .gt_left {
  text-align: left;
}
&#10;#glxpqxfgaw .gt_center {
  text-align: center;
}
&#10;#glxpqxfgaw .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#glxpqxfgaw .gt_font_normal {
  font-weight: normal;
}
&#10;#glxpqxfgaw .gt_font_bold {
  font-weight: bold;
}
&#10;#glxpqxfgaw .gt_font_italic {
  font-style: italic;
}
&#10;#glxpqxfgaw .gt_super {
  font-size: 65%;
}
&#10;#glxpqxfgaw .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#glxpqxfgaw .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#glxpqxfgaw .gt_indent_1 {
  text-indent: 5px;
}
&#10;#glxpqxfgaw .gt_indent_2 {
  text-indent: 10px;
}
&#10;#glxpqxfgaw .gt_indent_3 {
  text-indent: 15px;
}
&#10;#glxpqxfgaw .gt_indent_4 {
  text-indent: 20px;
}
&#10;#glxpqxfgaw .gt_indent_5 {
  text-indent: 25px;
}
&#10;#glxpqxfgaw .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#glxpqxfgaw div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
<thead>
<tr class="gt_col_headings">
<th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="user">
user
</th>
<th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="repo">
repo
</th>
<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="coord_">
coord\_
</th>
<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="facet_">
facet\_
</th>
<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="geom_">
geom\_
</th>
<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="scale_">
scale\_
</th>
<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="stat_">
stat\_
</th>
<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="theme_">
theme\_
</th>
</tr>
</thead>
<tbody class="gt_table_body">
<tr>
<td headers="user" class="gt_row gt_left">
AllanCameron
</td>
<td headers="repo" class="gt_row gt_left">
geomtextpath
</td>
<td headers="coord_" class="gt_row gt_right">
1
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
24
</td>
<td headers="scale_" class="gt_row gt_right">
6
</td>
<td headers="stat_" class="gt_row gt_right">
1
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
cidm-ph
</td>
<td headers="repo" class="gt_row gt_left">
ggmapinset
</td>
<td headers="coord_" class="gt_row gt_right">
1
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
4
</td>
<td headers="scale_" class="gt_row gt_right">
NA
</td>
<td headers="stat_" class="gt_row gt_right">
2
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
davidchall
</td>
<td headers="repo" class="gt_row gt_left">
ggip
</td>
<td headers="coord_" class="gt_row gt_right">
1
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
1
</td>
<td headers="scale_" class="gt_row gt_right">
NA
</td>
<td headers="stat_" class="gt_row gt_right">
1
</td>
<td headers="theme_" class="gt_row gt_right">
2
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
easystats
</td>
<td headers="repo" class="gt_row gt_left">
see
</td>
<td headers="coord_" class="gt_row gt_right">
1
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
14
</td>
<td headers="scale_" class="gt_row gt_right">
78
</td>
<td headers="stat_" class="gt_row gt_right">
NA
</td>
<td headers="theme_" class="gt_row gt_right">
6
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
hrbrmstr
</td>
<td headers="repo" class="gt_row gt_left">
ggalt
</td>
<td headers="coord_" class="gt_row gt_right">
1
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
11
</td>
<td headers="scale_" class="gt_row gt_right">
NA
</td>
<td headers="stat_" class="gt_row gt_right">
6
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
stefanedwards
</td>
<td headers="repo" class="gt_row gt_left">
lemon
</td>
<td headers="coord_" class="gt_row gt_right">
5
</td>
<td headers="facet_" class="gt_row gt_right">
2
</td>
<td headers="geom_" class="gt_row gt_right">
3
</td>
<td headers="scale_" class="gt_row gt_right">
2
</td>
<td headers="stat_" class="gt_row gt_right">
NA
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
teunbrand
</td>
<td headers="repo" class="gt_row gt_left">
ggh4x
</td>
<td headers="coord_" class="gt_row gt_right">
1
</td>
<td headers="facet_" class="gt_row gt_right">
5
</td>
<td headers="geom_" class="gt_row gt_right">
7
</td>
<td headers="scale_" class="gt_row gt_right">
9
</td>
<td headers="stat_" class="gt_row gt_right">
7
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
teunbrand
</td>
<td headers="repo" class="gt_row gt_left">
legendry
</td>
<td headers="coord_" class="gt_row gt_right">
1
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
NA
</td>
<td headers="scale_" class="gt_row gt_right">
NA
</td>
<td headers="stat_" class="gt_row gt_right">
NA
</td>
<td headers="theme_" class="gt_row gt_right">
1
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
davidgohel
</td>
<td headers="repo" class="gt_row gt_left">
ggiraph
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
2
</td>
<td headers="geom_" class="gt_row gt_right">
50
</td>
<td headers="scale_" class="gt_row gt_right">
94
</td>
<td headers="stat_" class="gt_row gt_right">
NA
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
earowang
</td>
<td headers="repo" class="gt_row gt_left">
sugrrants
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
1
</td>
<td headers="geom_" class="gt_row gt_right">
1
</td>
<td headers="scale_" class="gt_row gt_right">
NA
</td>
<td headers="stat_" class="gt_row gt_right">
1
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
erocoar
</td>
<td headers="repo" class="gt_row gt_left">
ggpol
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
1
</td>
<td headers="geom_" class="gt_row gt_right">
7
</td>
<td headers="scale_" class="gt_row gt_right">
NA
</td>
<td headers="stat_" class="gt_row gt_right">
4
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
hafen
</td>
<td headers="repo" class="gt_row gt_left">
geofacet
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
1
</td>
<td headers="geom_" class="gt_row gt_right">
NA
</td>
<td headers="scale_" class="gt_row gt_right">
NA
</td>
<td headers="stat_" class="gt_row gt_right">
NA
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
mikmart
</td>
<td headers="repo" class="gt_row gt_left">
ggragged
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
2
</td>
<td headers="geom_" class="gt_row gt_right">
NA
</td>
<td headers="scale_" class="gt_row gt_right">
NA
</td>
<td headers="stat_" class="gt_row gt_right">
NA
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
mjskay
</td>
<td headers="repo" class="gt_row gt_left">
ggdist
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
3
</td>
<td headers="geom_" class="gt_row gt_right">
11
</td>
<td headers="scale_" class="gt_row gt_right">
45
</td>
<td headers="stat_" class="gt_row gt_right">
29
</td>
<td headers="theme_" class="gt_row gt_right">
2
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
msberends
</td>
<td headers="repo" class="gt_row gt_left">
AMR
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
1
</td>
<td headers="geom_" class="gt_row gt_right">
1
</td>
<td headers="scale_" class="gt_row gt_right">
6
</td>
<td headers="stat_" class="gt_row gt_right">
NA
</td>
<td headers="theme_" class="gt_row gt_right">
1
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
thomasp85
</td>
<td headers="repo" class="gt_row gt_left">
ggforce
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
7
</td>
<td headers="geom_" class="gt_row gt_right">
40
</td>
<td headers="scale_" class="gt_row gt_right">
5
</td>
<td headers="stat_" class="gt_row gt_right">
27
</td>
<td headers="theme_" class="gt_row gt_right">
1
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
thomasp85
</td>
<td headers="repo" class="gt_row gt_left">
ggraph
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
3
</td>
<td headers="geom_" class="gt_row gt_right">
55
</td>
<td headers="scale_" class="gt_row gt_right">
92
</td>
<td headers="stat_" class="gt_row gt_right">
NA
</td>
<td headers="theme_" class="gt_row gt_right">
1
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
LCBC-UiO
</td>
<td headers="repo" class="gt_row gt_left">
ggseg
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
2
</td>
<td headers="scale_" class="gt_row gt_right">
24
</td>
<td headers="stat_" class="gt_row gt_right">
NA
</td>
<td headers="theme_" class="gt_row gt_right">
8
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
LKremer
</td>
<td headers="repo" class="gt_row gt_left">
ggpointdensity
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
1
</td>
<td headers="scale_" class="gt_row gt_right">
NA
</td>
<td headers="stat_" class="gt_row gt_right">
NA
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
ProjectMOSAIC
</td>
<td headers="repo" class="gt_row gt_left">
ggformula
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
3
</td>
<td headers="scale_" class="gt_row gt_right">
NA
</td>
<td headers="stat_" class="gt_row gt_right">
5
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
PursuitOfDataScience
</td>
<td headers="repo" class="gt_row gt_left">
ggDoubleHeat
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
3
</td>
<td headers="scale_" class="gt_row gt_right">
NA
</td>
<td headers="stat_" class="gt_row gt_right">
NA
</td>
<td headers="theme_" class="gt_row gt_right">
1
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
Selbosh
</td>
<td headers="repo" class="gt_row gt_left">
ggChernoff
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
1
</td>
<td headers="scale_" class="gt_row gt_right">
6
</td>
<td headers="stat_" class="gt_row gt_right">
NA
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
Yunuuuu
</td>
<td headers="repo" class="gt_row gt_left">
ggalign
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
4
</td>
<td headers="scale_" class="gt_row gt_right">
1
</td>
<td headers="stat_" class="gt_row gt_right">
NA
</td>
<td headers="theme_" class="gt_row gt_right">
2
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
aloy
</td>
<td headers="repo" class="gt_row gt_left">
qqplotr
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
1
</td>
<td headers="scale_" class="gt_row gt_right">
NA
</td>
<td headers="stat_" class="gt_row gt_right">
6
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
aphalo
</td>
<td headers="repo" class="gt_row gt_left">
gginnards
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
4
</td>
<td headers="scale_" class="gt_row gt_right">
NA
</td>
<td headers="stat_" class="gt_row gt_right">
2
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
aphalo
</td>
<td headers="repo" class="gt_row gt_left">
ggpp
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
38
</td>
<td headers="scale_" class="gt_row gt_right">
4
</td>
<td headers="stat_" class="gt_row gt_right">
26
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
aphalo
</td>
<td headers="repo" class="gt_row gt_left">
ggspectra
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
1
</td>
<td headers="scale_" class="gt_row gt_right">
27
</td>
<td headers="stat_" class="gt_row gt_right">
24
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
benskov
</td>
<td headers="repo" class="gt_row gt_left">
humapr
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
3
</td>
<td headers="scale_" class="gt_row gt_right">
NA
</td>
<td headers="stat_" class="gt_row gt_right">
NA
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
briatte
</td>
<td headers="repo" class="gt_row gt_left">
ggnetwork
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
10
</td>
<td headers="scale_" class="gt_row gt_right">
NA
</td>
<td headers="stat_" class="gt_row gt_right">
NA
</td>
<td headers="theme_" class="gt_row gt_right">
2
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
cgoo4
</td>
<td headers="repo" class="gt_row gt_left">
ggfoundry
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
1
</td>
<td headers="scale_" class="gt_row gt_right">
NA
</td>
<td headers="stat_" class="gt_row gt_right">
NA
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
clauswilke
</td>
<td headers="repo" class="gt_row gt_left">
ggridges
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
7
</td>
<td headers="scale_" class="gt_row gt_right">
32
</td>
<td headers="stat_" class="gt_row gt_right">
2
</td>
<td headers="theme_" class="gt_row gt_right">
1
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
clauswilke
</td>
<td headers="repo" class="gt_row gt_left">
ggtext
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
2
</td>
<td headers="scale_" class="gt_row gt_right">
NA
</td>
<td headers="stat_" class="gt_row gt_right">
NA
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
const-ae
</td>
<td headers="repo" class="gt_row gt_left">
ggsignif
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
1
</td>
<td headers="scale_" class="gt_row gt_right">
NA
</td>
<td headers="stat_" class="gt_row gt_right">
1
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
corybrunson
</td>
<td headers="repo" class="gt_row gt_left">
ggalluvial
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
4
</td>
<td headers="scale_" class="gt_row gt_right">
NA
</td>
<td headers="stat_" class="gt_row gt_right">
3
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
davidsjoberg
</td>
<td headers="repo" class="gt_row gt_left">
ggbump
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
2
</td>
<td headers="scale_" class="gt_row gt_right">
NA
</td>
<td headers="stat_" class="gt_row gt_right">
NA
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
dieghernan
</td>
<td headers="repo" class="gt_row gt_left">
tidyterra
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
8
</td>
<td headers="scale_" class="gt_row gt_right">
84
</td>
<td headers="stat_" class="gt_row gt_right">
3
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
dzhang32
</td>
<td headers="repo" class="gt_row gt_left">
ggtranscript
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
5
</td>
<td headers="scale_" class="gt_row gt_right">
NA
</td>
<td headers="stat_" class="gt_row gt_right">
NA
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
eclarke
</td>
<td headers="repo" class="gt_row gt_left">
ggbeeswarm
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
2
</td>
<td headers="scale_" class="gt_row gt_right">
NA
</td>
<td headers="stat_" class="gt_row gt_right">
NA
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
erocoar
</td>
<td headers="repo" class="gt_row gt_left">
gghalves
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
5
</td>
<td headers="scale_" class="gt_row gt_right">
NA
</td>
<td headers="stat_" class="gt_row gt_right">
1
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
ggobi
</td>
<td headers="repo" class="gt_row gt_left">
ggally
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
2
</td>
<td headers="scale_" class="gt_row gt_right">
NA
</td>
<td headers="stat_" class="gt_row gt_right">
4
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
haleyjeppson
</td>
<td headers="repo" class="gt_row gt_left">
ggmosaic
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
3
</td>
<td headers="scale_" class="gt_row gt_right">
2
</td>
<td headers="stat_" class="gt_row gt_right">
3
</td>
<td headers="theme_" class="gt_row gt_right">
1
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
hughjonesd
</td>
<td headers="repo" class="gt_row gt_left">
ggmagnify
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
2
</td>
<td headers="scale_" class="gt_row gt_right">
NA
</td>
<td headers="stat_" class="gt_row gt_right">
NA
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
jamesotto852
</td>
<td headers="repo" class="gt_row gt_left">
ggdensity
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
8
</td>
<td headers="scale_" class="gt_row gt_right">
NA
</td>
<td headers="stat_" class="gt_row gt_right">
8
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
jhrcook
</td>
<td headers="repo" class="gt_row gt_left">
ggasym
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
1
</td>
<td headers="scale_" class="gt_row gt_right">
12
</td>
<td headers="stat_" class="gt_row gt_right">
NA
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
jrnold
</td>
<td headers="repo" class="gt_row gt_left">
ggthemes
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
2
</td>
<td headers="scale_" class="gt_row gt_right">
64
</td>
<td headers="stat_" class="gt_row gt_right">
1
</td>
<td headers="theme_" class="gt_row gt_right">
22
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
jtlandis
</td>
<td headers="repo" class="gt_row gt_left">
ggside
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
36
</td>
<td headers="scale_" class="gt_row gt_right">
44
</td>
<td headers="stat_" class="gt_row gt_right">
4
</td>
<td headers="theme_" class="gt_row gt_right">
9
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
kassambara
</td>
<td headers="repo" class="gt_row gt_left">
ggpubr
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
4
</td>
<td headers="scale_" class="gt_row gt_right">
NA
</td>
<td headers="stat_" class="gt_row gt_right">
16
</td>
<td headers="theme_" class="gt_row gt_right">
5
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
krassowski
</td>
<td headers="repo" class="gt_row gt_left">
complex-upset
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
4
</td>
<td headers="scale_" class="gt_row gt_right">
2
</td>
<td headers="stat_" class="gt_row gt_right">
NA
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
larmarange
</td>
<td headers="repo" class="gt_row gt_left">
ggstats
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
10
</td>
<td headers="scale_" class="gt_row gt_right">
3
</td>
<td headers="stat_" class="gt_row gt_right">
3
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
lepennec
</td>
<td headers="repo" class="gt_row gt_left">
ggwordcloud
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
2
</td>
<td headers="scale_" class="gt_row gt_right">
NA
</td>
<td headers="stat_" class="gt_row gt_right">
NA
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
lionel-
</td>
<td headers="repo" class="gt_row gt_left">
ggstance
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
9
</td>
<td headers="scale_" class="gt_row gt_right">
NA
</td>
<td headers="stat_" class="gt_row gt_right">
5
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
malcolmbarrett
</td>
<td headers="repo" class="gt_row gt_left">
ggdag
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
14
</td>
<td headers="scale_" class="gt_row gt_right">
2
</td>
<td headers="stat_" class="gt_row gt_right">
NA
</td>
<td headers="theme_" class="gt_row gt_right">
7
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
marcmenem
</td>
<td headers="repo" class="gt_row gt_left">
ggshadow
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
8
</td>
<td headers="scale_" class="gt_row gt_right">
22
</td>
<td headers="stat_" class="gt_row gt_right">
NA
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
martin-borkovec
</td>
<td headers="repo" class="gt_row gt_left">
ggparty
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
6
</td>
<td headers="scale_" class="gt_row gt_right">
NA
</td>
<td headers="stat_" class="gt_row gt_right">
NA
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
mdhall272
</td>
<td headers="repo" class="gt_row gt_left">
ggarchery
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
1
</td>
<td headers="scale_" class="gt_row gt_right">
NA
</td>
<td headers="stat_" class="gt_row gt_right">
NA
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
metrumresearchgroup
</td>
<td headers="repo" class="gt_row gt_left">
ggedit
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
1
</td>
<td headers="scale_" class="gt_row gt_right">
NA
</td>
<td headers="stat_" class="gt_row gt_right">
NA
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
mikabr
</td>
<td headers="repo" class="gt_row gt_left">
ggpirate
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
1
</td>
<td headers="scale_" class="gt_row gt_right">
NA
</td>
<td headers="stat_" class="gt_row gt_right">
NA
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
mitchelloharawild
</td>
<td headers="repo" class="gt_row gt_left">
ggquiver
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
1
</td>
<td headers="scale_" class="gt_row gt_right">
NA
</td>
<td headers="stat_" class="gt_row gt_right">
1
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
mivalek
</td>
<td headers="repo" class="gt_row gt_left">
ggterrorbar
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
1
</td>
<td headers="scale_" class="gt_row gt_right">
NA
</td>
<td headers="stat_" class="gt_row gt_right">
NA
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
nflverse
</td>
<td headers="repo" class="gt_row gt_left">
nflplotR
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
6
</td>
<td headers="scale_" class="gt_row gt_right">
3
</td>
<td headers="stat_" class="gt_row gt_right">
NA
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
njudd
</td>
<td headers="repo" class="gt_row gt_left">
ggrain
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
1
</td>
<td headers="scale_" class="gt_row gt_right">
NA
</td>
<td headers="stat_" class="gt_row gt_right">
NA
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
nsgrantham
</td>
<td headers="repo" class="gt_row gt_left">
ggbraid
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
1
</td>
<td headers="scale_" class="gt_row gt_right">
NA
</td>
<td headers="stat_" class="gt_row gt_right">
1
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
oldlipe
</td>
<td headers="repo" class="gt_row gt_left">
ggsom
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
1
</td>
<td headers="scale_" class="gt_row gt_right">
NA
</td>
<td headers="stat_" class="gt_row gt_right">
NA
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
omarwagih
</td>
<td headers="repo" class="gt_row gt_left">
ggseqlogo
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
1
</td>
<td headers="scale_" class="gt_row gt_right">
NA
</td>
<td headers="stat_" class="gt_row gt_right">
NA
</td>
<td headers="theme_" class="gt_row gt_right">
1
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
piecepackr
</td>
<td headers="repo" class="gt_row gt_left">
piecepackr
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
1
</td>
<td headers="scale_" class="gt_row gt_right">
2
</td>
<td headers="stat_" class="gt_row gt_right">
NA
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
rcorty
</td>
<td headers="repo" class="gt_row gt_left">
ggQQunif
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
1
</td>
<td headers="scale_" class="gt_row gt_right">
2
</td>
<td headers="stat_" class="gt_row gt_right">
NA
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
ricardo-bion
</td>
<td headers="repo" class="gt_row gt_left">
ggtech
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
1
</td>
<td headers="scale_" class="gt_row gt_right">
2
</td>
<td headers="stat_" class="gt_row gt_right">
NA
</td>
<td headers="theme_" class="gt_row gt_right">
2
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
rivasiker
</td>
<td headers="repo" class="gt_row gt_left">
ggHoriPlot
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
1
</td>
<td headers="scale_" class="gt_row gt_right">
1
</td>
<td headers="stat_" class="gt_row gt_right">
1
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
rnabioco
</td>
<td headers="repo" class="gt_row gt_left">
ggtrace
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
4
</td>
<td headers="scale_" class="gt_row gt_right">
NA
</td>
<td headers="stat_" class="gt_row gt_right">
NA
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
sachsmc
</td>
<td headers="repo" class="gt_row gt_left">
plotROC
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
2
</td>
<td headers="scale_" class="gt_row gt_right">
NA
</td>
<td headers="stat_" class="gt_row gt_right">
2
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
sctyner
</td>
<td headers="repo" class="gt_row gt_left">
geomnet
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
2
</td>
<td headers="scale_" class="gt_row gt_right">
NA
</td>
<td headers="stat_" class="gt_row gt_right">
1
</td>
<td headers="theme_" class="gt_row gt_right">
1
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
slowkow
</td>
<td headers="repo" class="gt_row gt_left">
ggrepel
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
2
</td>
<td headers="scale_" class="gt_row gt_right">
NA
</td>
<td headers="stat_" class="gt_row gt_right">
NA
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
statsmaths
</td>
<td headers="repo" class="gt_row gt_left">
ggimg
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
2
</td>
<td headers="scale_" class="gt_row gt_right">
NA
</td>
<td headers="stat_" class="gt_row gt_right">
NA
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
teunbrand
</td>
<td headers="repo" class="gt_row gt_left">
ggarrow
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
4
</td>
<td headers="scale_" class="gt_row gt_right">
8
</td>
<td headers="stat_" class="gt_row gt_right">
NA
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
thackl
</td>
<td headers="repo" class="gt_row gt_left">
gggenomes
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
20
</td>
<td headers="scale_" class="gt_row gt_right">
3
</td>
<td headers="stat_" class="gt_row gt_right">
NA
</td>
<td headers="theme_" class="gt_row gt_right">
1
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
trevorld
</td>
<td headers="repo" class="gt_row gt_left">
ggpattern
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
16
</td>
<td headers="scale_" class="gt_row gt_right">
163
</td>
<td headers="stat_" class="gt_row gt_right">
NA
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
trevorld
</td>
<td headers="repo" class="gt_row gt_left">
oblicubes
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
2
</td>
<td headers="scale_" class="gt_row gt_right">
NA
</td>
<td headers="stat_" class="gt_row gt_right">
NA
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
vpetukhov
</td>
<td headers="repo" class="gt_row gt_left">
ggrastr
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
7
</td>
<td headers="scale_" class="gt_row gt_right">
NA
</td>
<td headers="stat_" class="gt_row gt_right">
NA
</td>
<td headers="theme_" class="gt_row gt_right">
1
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
wilkox
</td>
<td headers="repo" class="gt_row gt_left">
ggfittext
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
2
</td>
<td headers="scale_" class="gt_row gt_right">
NA
</td>
<td headers="stat_" class="gt_row gt_right">
NA
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
wilkox
</td>
<td headers="repo" class="gt_row gt_left">
gggenes
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
8
</td>
<td headers="scale_" class="gt_row gt_right">
NA
</td>
<td headers="stat_" class="gt_row gt_right">
NA
</td>
<td headers="theme_" class="gt_row gt_right">
2
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
wilkox
</td>
<td headers="repo" class="gt_row gt_left">
treemapify
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
8
</td>
<td headers="scale_" class="gt_row gt_right">
NA
</td>
<td headers="stat_" class="gt_row gt_right">
NA
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
xiangpin
</td>
<td headers="repo" class="gt_row gt_left">
ggstar
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
1
</td>
<td headers="scale_" class="gt_row gt_right">
7
</td>
<td headers="stat_" class="gt_row gt_right">
NA
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
yanpd01
</td>
<td headers="repo" class="gt_row gt_left">
ggsector
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
1
</td>
<td headers="scale_" class="gt_row gt_right">
NA
</td>
<td headers="stat_" class="gt_row gt_right">
NA
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
Ryo-N7
</td>
<td headers="repo" class="gt_row gt_left">
tvthemes
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
NA
</td>
<td headers="scale_" class="gt_row gt_right">
42
</td>
<td headers="stat_" class="gt_row gt_right">
NA
</td>
<td headers="theme_" class="gt_row gt_right">
12
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
YuLab-SMU
</td>
<td headers="repo" class="gt_row gt_left">
ggbreak
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
NA
</td>
<td headers="scale_" class="gt_row gt_right">
5
</td>
<td headers="stat_" class="gt_row gt_right">
NA
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
aphalo
</td>
<td headers="repo" class="gt_row gt_left">
ggpmisc
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
NA
</td>
<td headers="scale_" class="gt_row gt_right">
13
</td>
<td headers="stat_" class="gt_row gt_right">
18
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
const-ae
</td>
<td headers="repo" class="gt_row gt_left">
ggupset
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
NA
</td>
<td headers="scale_" class="gt_row gt_right">
2
</td>
<td headers="stat_" class="gt_row gt_right">
NA
</td>
<td headers="theme_" class="gt_row gt_right">
1
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
cttobin
</td>
<td headers="repo" class="gt_row gt_left">
ggthemr
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
NA
</td>
<td headers="scale_" class="gt_row gt_right">
1
</td>
<td headers="stat_" class="gt_row gt_right">
NA
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
hrbrmstr
</td>
<td headers="repo" class="gt_row gt_left">
hrbrthemes
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
NA
</td>
<td headers="scale_" class="gt_row gt_right">
10
</td>
<td headers="stat_" class="gt_row gt_right">
NA
</td>
<td headers="theme_" class="gt_row gt_right">
9
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
road2stat
</td>
<td headers="repo" class="gt_row gt_left">
ggsci
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
NA
</td>
<td headers="scale_" class="gt_row gt_right">
75
</td>
<td headers="stat_" class="gt_row gt_right">
NA
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
teunbrand
</td>
<td headers="repo" class="gt_row gt_left">
ggchromatic
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
NA
</td>
<td headers="scale_" class="gt_row gt_right">
30
</td>
<td headers="stat_" class="gt_row gt_right">
NA
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
AckerDWM
</td>
<td headers="repo" class="gt_row gt_left">
gg3D
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
NA
</td>
<td headers="scale_" class="gt_row gt_right">
NA
</td>
<td headers="stat_" class="gt_row gt_right">
2
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
Ather-Energy
</td>
<td headers="repo" class="gt_row gt_left">
ggTimeSeries
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
NA
</td>
<td headers="scale_" class="gt_row gt_right">
NA
</td>
<td headers="stat_" class="gt_row gt_right">
6
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
graysonwhite
</td>
<td headers="repo" class="gt_row gt_left">
gglm
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
NA
</td>
<td headers="scale_" class="gt_row gt_right">
NA
</td>
<td headers="stat_" class="gt_row gt_right">
7
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
jjchern
</td>
<td headers="repo" class="gt_row gt_left">
gglorenz
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
NA
</td>
<td headers="scale_" class="gt_row gt_right">
NA
</td>
<td headers="stat_" class="gt_row gt_right">
2
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
kenithgrey
</td>
<td headers="repo" class="gt_row gt_left">
ggQC
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
NA
</td>
<td headers="scale_" class="gt_row gt_right">
NA
</td>
<td headers="stat_" class="gt_row gt_right">
11
</td>
<td headers="theme_" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
IndrajeetPatil
</td>
<td headers="repo" class="gt_row gt_left">
ggstatsplot
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
NA
</td>
<td headers="scale_" class="gt_row gt_right">
NA
</td>
<td headers="stat_" class="gt_row gt_right">
NA
</td>
<td headers="theme_" class="gt_row gt_right">
1
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
jbengler
</td>
<td headers="repo" class="gt_row gt_left">
tidyplots
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
NA
</td>
<td headers="scale_" class="gt_row gt_right">
NA
</td>
<td headers="stat_" class="gt_row gt_right">
NA
</td>
<td headers="theme_" class="gt_row gt_right">
5
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
kassambara
</td>
<td headers="repo" class="gt_row gt_left">
survminer
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
NA
</td>
<td headers="scale_" class="gt_row gt_right">
NA
</td>
<td headers="stat_" class="gt_row gt_right">
NA
</td>
<td headers="theme_" class="gt_row gt_right">
2
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
thomas-neitmann
</td>
<td headers="repo" class="gt_row gt_left">
ggcharts
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
NA
</td>
<td headers="scale_" class="gt_row gt_right">
NA
</td>
<td headers="stat_" class="gt_row gt_right">
NA
</td>
<td headers="theme_" class="gt_row gt_right">
5
</td>
</tr>
<tr>
<td headers="user" class="gt_row gt_left">
wilkelab
</td>
<td headers="repo" class="gt_row gt_left">
cowplot
</td>
<td headers="coord_" class="gt_row gt_right">
NA
</td>
<td headers="facet_" class="gt_row gt_right">
NA
</td>
<td headers="geom_" class="gt_row gt_right">
NA
</td>
<td headers="scale_" class="gt_row gt_right">
NA
</td>
<td headers="stat_" class="gt_row gt_right">
NA
</td>
<td headers="theme_" class="gt_row gt_right">
7
</td>
</tr>
</tbody>
</table>

</div>

``` r

# last_table() |>
#   set_rows(c(user, repo))


read_csv("https://raw.githubusercontent.com/EvaMaeRey/mytidytuesday/refs/heads/main/2024-12-10-ggplot2-layer-composition/ggplot2_exported_layer_fun_composition.csv") |> 
  rename(prefix = fun_prefix) ->
ggplot2_layers_definers

ggplot2_layers_definers |>
  ggtable()
```

<div id="dauzvtcqxm"
style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">

<style>#dauzvtcqxm table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#dauzvtcqxm thead, #dauzvtcqxm tbody, #dauzvtcqxm tfoot, #dauzvtcqxm tr, #dauzvtcqxm td, #dauzvtcqxm th {
  border-style: none;
}
&#10;#dauzvtcqxm p {
  margin: 0;
  padding: 0;
}
&#10;#dauzvtcqxm .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}
&#10;#dauzvtcqxm .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#dauzvtcqxm .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#dauzvtcqxm .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#dauzvtcqxm .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#dauzvtcqxm .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#dauzvtcqxm .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#dauzvtcqxm .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}
&#10;#dauzvtcqxm .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#dauzvtcqxm .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#dauzvtcqxm .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#dauzvtcqxm .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#dauzvtcqxm .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#dauzvtcqxm .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}
&#10;#dauzvtcqxm .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}
&#10;#dauzvtcqxm .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#dauzvtcqxm .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#dauzvtcqxm .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#dauzvtcqxm .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#dauzvtcqxm .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#dauzvtcqxm .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#dauzvtcqxm .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#dauzvtcqxm .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#dauzvtcqxm .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#dauzvtcqxm .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#dauzvtcqxm .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#dauzvtcqxm .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#dauzvtcqxm .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#dauzvtcqxm .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#dauzvtcqxm .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#dauzvtcqxm .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#dauzvtcqxm .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#dauzvtcqxm .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#dauzvtcqxm .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#dauzvtcqxm .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#dauzvtcqxm .gt_left {
  text-align: left;
}
&#10;#dauzvtcqxm .gt_center {
  text-align: center;
}
&#10;#dauzvtcqxm .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#dauzvtcqxm .gt_font_normal {
  font-weight: normal;
}
&#10;#dauzvtcqxm .gt_font_bold {
  font-weight: bold;
}
&#10;#dauzvtcqxm .gt_font_italic {
  font-style: italic;
}
&#10;#dauzvtcqxm .gt_super {
  font-size: 65%;
}
&#10;#dauzvtcqxm .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#dauzvtcqxm .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#dauzvtcqxm .gt_indent_1 {
  text-indent: 5px;
}
&#10;#dauzvtcqxm .gt_indent_2 {
  text-indent: 10px;
}
&#10;#dauzvtcqxm .gt_indent_3 {
  text-indent: 15px;
}
&#10;#dauzvtcqxm .gt_indent_4 {
  text-indent: 20px;
}
&#10;#dauzvtcqxm .gt_indent_5 {
  text-indent: 25px;
}
&#10;#dauzvtcqxm .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#dauzvtcqxm div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
<thead>
<tr class="gt_col_headings">
<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="value">
value
</th>
</tr>
</thead>
<tbody class="gt_table_body">
<tr>
<td headers="value" class="gt_row gt_right">
254
</td>
</tr>
</tbody>
</table>

</div>

``` r

last_table() |>
  set_rows(type)
```

<div id="ibkvganjqo"
style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">

<style>#ibkvganjqo table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#ibkvganjqo thead, #ibkvganjqo tbody, #ibkvganjqo tfoot, #ibkvganjqo tr, #ibkvganjqo td, #ibkvganjqo th {
  border-style: none;
}
&#10;#ibkvganjqo p {
  margin: 0;
  padding: 0;
}
&#10;#ibkvganjqo .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}
&#10;#ibkvganjqo .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#ibkvganjqo .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#ibkvganjqo .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#ibkvganjqo .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#ibkvganjqo .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#ibkvganjqo .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#ibkvganjqo .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}
&#10;#ibkvganjqo .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#ibkvganjqo .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#ibkvganjqo .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#ibkvganjqo .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#ibkvganjqo .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#ibkvganjqo .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}
&#10;#ibkvganjqo .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}
&#10;#ibkvganjqo .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#ibkvganjqo .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#ibkvganjqo .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#ibkvganjqo .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ibkvganjqo .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#ibkvganjqo .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#ibkvganjqo .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#ibkvganjqo .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ibkvganjqo .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#ibkvganjqo .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#ibkvganjqo .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#ibkvganjqo .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ibkvganjqo .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#ibkvganjqo .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#ibkvganjqo .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#ibkvganjqo .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#ibkvganjqo .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#ibkvganjqo .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ibkvganjqo .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#ibkvganjqo .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ibkvganjqo .gt_left {
  text-align: left;
}
&#10;#ibkvganjqo .gt_center {
  text-align: center;
}
&#10;#ibkvganjqo .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#ibkvganjqo .gt_font_normal {
  font-weight: normal;
}
&#10;#ibkvganjqo .gt_font_bold {
  font-weight: bold;
}
&#10;#ibkvganjqo .gt_font_italic {
  font-style: italic;
}
&#10;#ibkvganjqo .gt_super {
  font-size: 65%;
}
&#10;#ibkvganjqo .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#ibkvganjqo .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#ibkvganjqo .gt_indent_1 {
  text-indent: 5px;
}
&#10;#ibkvganjqo .gt_indent_2 {
  text-indent: 10px;
}
&#10;#ibkvganjqo .gt_indent_3 {
  text-indent: 15px;
}
&#10;#ibkvganjqo .gt_indent_4 {
  text-indent: 20px;
}
&#10;#ibkvganjqo .gt_indent_5 {
  text-indent: 25px;
}
&#10;#ibkvganjqo .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#ibkvganjqo div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
<thead>
<tr class="gt_col_headings">
<th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="type">
type
</th>
<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="value">
value
</th>
</tr>
</thead>
<tbody class="gt_table_body">
<tr>
<td headers="type" class="gt_row gt_left">
geom
</td>
<td headers="value" class="gt_row gt_right">
84
</td>
</tr>
<tr>
<td headers="type" class="gt_row gt_left">
position
</td>
<td headers="value" class="gt_row gt_right">
85
</td>
</tr>
<tr>
<td headers="type" class="gt_row gt_left">
stat
</td>
<td headers="value" class="gt_row gt_right">
85
</td>
</tr>
</tbody>
</table>

</div>

``` r

last_table() |>
  set_rows(type) |>
  set_cols(default_or_fixed) |>
  set_rows(c(prefix, type))
```

<div id="waqeszcevc"
style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">

<style>#waqeszcevc table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#waqeszcevc thead, #waqeszcevc tbody, #waqeszcevc tfoot, #waqeszcevc tr, #waqeszcevc td, #waqeszcevc th {
  border-style: none;
}
&#10;#waqeszcevc p {
  margin: 0;
  padding: 0;
}
&#10;#waqeszcevc .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}
&#10;#waqeszcevc .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#waqeszcevc .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#waqeszcevc .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#waqeszcevc .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#waqeszcevc .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#waqeszcevc .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#waqeszcevc .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}
&#10;#waqeszcevc .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#waqeszcevc .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#waqeszcevc .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#waqeszcevc .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#waqeszcevc .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#waqeszcevc .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}
&#10;#waqeszcevc .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}
&#10;#waqeszcevc .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#waqeszcevc .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#waqeszcevc .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#waqeszcevc .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#waqeszcevc .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#waqeszcevc .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#waqeszcevc .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#waqeszcevc .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#waqeszcevc .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#waqeszcevc .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#waqeszcevc .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#waqeszcevc .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#waqeszcevc .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#waqeszcevc .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#waqeszcevc .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#waqeszcevc .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#waqeszcevc .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#waqeszcevc .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#waqeszcevc .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#waqeszcevc .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#waqeszcevc .gt_left {
  text-align: left;
}
&#10;#waqeszcevc .gt_center {
  text-align: center;
}
&#10;#waqeszcevc .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#waqeszcevc .gt_font_normal {
  font-weight: normal;
}
&#10;#waqeszcevc .gt_font_bold {
  font-weight: bold;
}
&#10;#waqeszcevc .gt_font_italic {
  font-style: italic;
}
&#10;#waqeszcevc .gt_super {
  font-size: 65%;
}
&#10;#waqeszcevc .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#waqeszcevc .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#waqeszcevc .gt_indent_1 {
  text-indent: 5px;
}
&#10;#waqeszcevc .gt_indent_2 {
  text-indent: 10px;
}
&#10;#waqeszcevc .gt_indent_3 {
  text-indent: 15px;
}
&#10;#waqeszcevc .gt_indent_4 {
  text-indent: 20px;
}
&#10;#waqeszcevc .gt_indent_5 {
  text-indent: 25px;
}
&#10;#waqeszcevc .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#waqeszcevc div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
<thead>
<tr class="gt_col_headings">
<th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="prefix">
prefix
</th>
<th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="type">
type
</th>
<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="default">
default
</th>
<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="fixed">
fixed
</th>
</tr>
</thead>
<tbody class="gt_table_body">
<tr>
<td headers="prefix" class="gt_row gt_left">
geom\_
</td>
<td headers="type" class="gt_row gt_left">
geom
</td>
<td headers="default" class="gt_row gt_right">
2
</td>
<td headers="fixed" class="gt_row gt_right">
50
</td>
</tr>
<tr>
<td headers="prefix" class="gt_row gt_left">
geom\_
</td>
<td headers="type" class="gt_row gt_left">
position
</td>
<td headers="default" class="gt_row gt_right">
51
</td>
<td headers="fixed" class="gt_row gt_right">
2
</td>
</tr>
<tr>
<td headers="prefix" class="gt_row gt_left">
geom\_
</td>
<td headers="type" class="gt_row gt_left">
stat
</td>
<td headers="default" class="gt_row gt_right">
47
</td>
<td headers="fixed" class="gt_row gt_right">
6
</td>
</tr>
<tr>
<td headers="prefix" class="gt_row gt_left">
stat\_
</td>
<td headers="type" class="gt_row gt_left">
geom
</td>
<td headers="default" class="gt_row gt_right">
32
</td>
<td headers="fixed" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="prefix" class="gt_row gt_left">
stat\_
</td>
<td headers="type" class="gt_row gt_left">
position
</td>
<td headers="default" class="gt_row gt_right">
32
</td>
<td headers="fixed" class="gt_row gt_right">
NA
</td>
</tr>
<tr>
<td headers="prefix" class="gt_row gt_left">
stat\_
</td>
<td headers="type" class="gt_row gt_left">
stat
</td>
<td headers="default" class="gt_row gt_right">
NA
</td>
<td headers="fixed" class="gt_row gt_right">
32
</td>
</tr>
</tbody>
</table>

</div>

# examples/derivative

``` r
knitr::opts_chunk$set(results = "markup")
```

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
#> 2 Female    17

flat_titanic |> pivot_samplen(rows = sex, value = freq)
#> # A tibble: 2 × 2
#>   sex    value     
#>   <fct>  <chr>     
#> 1 Male   0; 118; 14
#> 2 Female 76; 14; 13

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
