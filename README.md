
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
#> # A tibble: 4 × 3
#>   survived sex    value
#>   <fct>    <fct>  <dbl>
#> 1 No       Male    1364
#> 2 No       Female   126
#> 3 Yes      Male     367
#> 4 Yes      Female   344

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
                          wt = NULL) {

  # table specification components !
  tp <- list(
    data = data,
    rows = rows,
    cols = cols,
    value = value,
    wt = wt
    # more 'slots' to be added
  )

  # declare class 'tidypivot'
  class(tp) <- "tidypivot"

  # Return the created object
  invisible(tp)

}


print.tidypivot <- function(tp){
  
  print(gt::gt(do.call(pivotr, tp)))
  
  invisible(tp)
  
}

#' @export
ggtable <- function(data = data.frame()){
  
  thedata <<- data # don't love this

  tp <- new_tidypivot()
  
  tp$data <- data
  
  last_tp <<- tp
  
  tp

}
```

``` r
#' @export
set_rows <- function(tp, rows = NULL){
  
  tp$rows <- enquo(rows)
  
  last_tp <<- tp
  
  tp

  
}

ggtable(tidytitanic::tidy_titanic) |>
  set_rows(sex)
```

<div id="xrcretbbgx"
style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">

<style>#xrcretbbgx table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#xrcretbbgx thead, #xrcretbbgx tbody, #xrcretbbgx tfoot, #xrcretbbgx tr, #xrcretbbgx td, #xrcretbbgx th {
  border-style: none;
}
&#10;#xrcretbbgx p {
  margin: 0;
  padding: 0;
}
&#10;#xrcretbbgx .gt_table {
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
&#10;#xrcretbbgx .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#xrcretbbgx .gt_title {
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
&#10;#xrcretbbgx .gt_subtitle {
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
&#10;#xrcretbbgx .gt_heading {
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
&#10;#xrcretbbgx .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#xrcretbbgx .gt_col_headings {
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
&#10;#xrcretbbgx .gt_col_heading {
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
&#10;#xrcretbbgx .gt_column_spanner_outer {
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
&#10;#xrcretbbgx .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#xrcretbbgx .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#xrcretbbgx .gt_column_spanner {
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
&#10;#xrcretbbgx .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#xrcretbbgx .gt_group_heading {
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
&#10;#xrcretbbgx .gt_empty_group_heading {
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
&#10;#xrcretbbgx .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#xrcretbbgx .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#xrcretbbgx .gt_row {
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
&#10;#xrcretbbgx .gt_stub {
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
&#10;#xrcretbbgx .gt_stub_row_group {
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
&#10;#xrcretbbgx .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#xrcretbbgx .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#xrcretbbgx .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#xrcretbbgx .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#xrcretbbgx .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#xrcretbbgx .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#xrcretbbgx .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#xrcretbbgx .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#xrcretbbgx .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#xrcretbbgx .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#xrcretbbgx .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#xrcretbbgx .gt_footnotes {
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
&#10;#xrcretbbgx .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#xrcretbbgx .gt_sourcenotes {
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
&#10;#xrcretbbgx .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#xrcretbbgx .gt_left {
  text-align: left;
}
&#10;#xrcretbbgx .gt_center {
  text-align: center;
}
&#10;#xrcretbbgx .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#xrcretbbgx .gt_font_normal {
  font-weight: normal;
}
&#10;#xrcretbbgx .gt_font_bold {
  font-weight: bold;
}
&#10;#xrcretbbgx .gt_font_italic {
  font-style: italic;
}
&#10;#xrcretbbgx .gt_super {
  font-size: 65%;
}
&#10;#xrcretbbgx .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#xrcretbbgx .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#xrcretbbgx .gt_indent_1 {
  text-indent: 5px;
}
&#10;#xrcretbbgx .gt_indent_2 {
  text-indent: 10px;
}
&#10;#xrcretbbgx .gt_indent_3 {
  text-indent: 15px;
}
&#10;#xrcretbbgx .gt_indent_4 {
  text-indent: 20px;
}
&#10;#xrcretbbgx .gt_indent_5 {
  text-indent: 25px;
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
1731
</td>
</tr>
<tr>
<td headers="sex" class="gt_row gt_center">
Female
</td>
<td headers="value" class="gt_row gt_right">
470
</td>
</tr>
</tbody>
</table>

</div>

``` r
  

#' @export
set_cols <- function(tp, cols = NULL){
  
  tp$cols <- enquo(cols)

  tp
  
  last_tp <<- tp
  
  tp
  

}

ggtable(tidytitanic::tidy_titanic) |>
  set_rows(sex) |>
  set_cols(survived)
```

<div id="inmxbuxhsy"
style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">

<style>#inmxbuxhsy table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#inmxbuxhsy thead, #inmxbuxhsy tbody, #inmxbuxhsy tfoot, #inmxbuxhsy tr, #inmxbuxhsy td, #inmxbuxhsy th {
  border-style: none;
}
&#10;#inmxbuxhsy p {
  margin: 0;
  padding: 0;
}
&#10;#inmxbuxhsy .gt_table {
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
&#10;#inmxbuxhsy .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#inmxbuxhsy .gt_title {
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
&#10;#inmxbuxhsy .gt_subtitle {
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
&#10;#inmxbuxhsy .gt_heading {
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
&#10;#inmxbuxhsy .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#inmxbuxhsy .gt_col_headings {
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
&#10;#inmxbuxhsy .gt_col_heading {
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
&#10;#inmxbuxhsy .gt_column_spanner_outer {
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
&#10;#inmxbuxhsy .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#inmxbuxhsy .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#inmxbuxhsy .gt_column_spanner {
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
&#10;#inmxbuxhsy .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#inmxbuxhsy .gt_group_heading {
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
&#10;#inmxbuxhsy .gt_empty_group_heading {
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
&#10;#inmxbuxhsy .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#inmxbuxhsy .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#inmxbuxhsy .gt_row {
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
&#10;#inmxbuxhsy .gt_stub {
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
&#10;#inmxbuxhsy .gt_stub_row_group {
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
&#10;#inmxbuxhsy .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#inmxbuxhsy .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#inmxbuxhsy .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#inmxbuxhsy .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#inmxbuxhsy .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#inmxbuxhsy .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#inmxbuxhsy .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#inmxbuxhsy .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#inmxbuxhsy .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#inmxbuxhsy .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#inmxbuxhsy .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#inmxbuxhsy .gt_footnotes {
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
&#10;#inmxbuxhsy .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#inmxbuxhsy .gt_sourcenotes {
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
&#10;#inmxbuxhsy .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#inmxbuxhsy .gt_left {
  text-align: left;
}
&#10;#inmxbuxhsy .gt_center {
  text-align: center;
}
&#10;#inmxbuxhsy .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#inmxbuxhsy .gt_font_normal {
  font-weight: normal;
}
&#10;#inmxbuxhsy .gt_font_bold {
  font-weight: bold;
}
&#10;#inmxbuxhsy .gt_font_italic {
  font-style: italic;
}
&#10;#inmxbuxhsy .gt_super {
  font-size: 65%;
}
&#10;#inmxbuxhsy .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#inmxbuxhsy .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#inmxbuxhsy .gt_indent_1 {
  text-indent: 5px;
}
&#10;#inmxbuxhsy .gt_indent_2 {
  text-indent: 10px;
}
&#10;#inmxbuxhsy .gt_indent_3 {
  text-indent: 15px;
}
&#10;#inmxbuxhsy .gt_indent_4 {
  text-indent: 20px;
}
&#10;#inmxbuxhsy .gt_indent_5 {
  text-indent: 25px;
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
```

<div id="zobkvetxvh"
style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">

<style>#zobkvetxvh table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#zobkvetxvh thead, #zobkvetxvh tbody, #zobkvetxvh tfoot, #zobkvetxvh tr, #zobkvetxvh td, #zobkvetxvh th {
  border-style: none;
}
&#10;#zobkvetxvh p {
  margin: 0;
  padding: 0;
}
&#10;#zobkvetxvh .gt_table {
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
&#10;#zobkvetxvh .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#zobkvetxvh .gt_title {
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
&#10;#zobkvetxvh .gt_subtitle {
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
&#10;#zobkvetxvh .gt_heading {
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
&#10;#zobkvetxvh .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#zobkvetxvh .gt_col_headings {
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
&#10;#zobkvetxvh .gt_col_heading {
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
&#10;#zobkvetxvh .gt_column_spanner_outer {
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
&#10;#zobkvetxvh .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#zobkvetxvh .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#zobkvetxvh .gt_column_spanner {
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
&#10;#zobkvetxvh .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#zobkvetxvh .gt_group_heading {
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
&#10;#zobkvetxvh .gt_empty_group_heading {
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
&#10;#zobkvetxvh .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#zobkvetxvh .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#zobkvetxvh .gt_row {
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
&#10;#zobkvetxvh .gt_stub {
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
&#10;#zobkvetxvh .gt_stub_row_group {
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
&#10;#zobkvetxvh .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#zobkvetxvh .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#zobkvetxvh .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#zobkvetxvh .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#zobkvetxvh .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#zobkvetxvh .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#zobkvetxvh .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#zobkvetxvh .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#zobkvetxvh .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#zobkvetxvh .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#zobkvetxvh .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#zobkvetxvh .gt_footnotes {
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
&#10;#zobkvetxvh .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#zobkvetxvh .gt_sourcenotes {
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
&#10;#zobkvetxvh .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#zobkvetxvh .gt_left {
  text-align: left;
}
&#10;#zobkvetxvh .gt_center {
  text-align: center;
}
&#10;#zobkvetxvh .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#zobkvetxvh .gt_font_normal {
  font-weight: normal;
}
&#10;#zobkvetxvh .gt_font_bold {
  font-weight: bold;
}
&#10;#zobkvetxvh .gt_font_italic {
  font-style: italic;
}
&#10;#zobkvetxvh .gt_super {
  font-size: 65%;
}
&#10;#zobkvetxvh .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#zobkvetxvh .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#zobkvetxvh .gt_indent_1 {
  text-indent: 5px;
}
&#10;#zobkvetxvh .gt_indent_2 {
  text-indent: 10px;
}
&#10;#zobkvetxvh .gt_indent_3 {
  text-indent: 15px;
}
&#10;#zobkvetxvh .gt_indent_4 {
  text-indent: 20px;
}
&#10;#zobkvetxvh .gt_indent_5 {
  text-indent: 25px;
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

ggtable(ext_exports %>% filter(ind_classic_prefix))
```

<div id="htcmbibwce"
style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">

<style>#htcmbibwce table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#htcmbibwce thead, #htcmbibwce tbody, #htcmbibwce tfoot, #htcmbibwce tr, #htcmbibwce td, #htcmbibwce th {
  border-style: none;
}
&#10;#htcmbibwce p {
  margin: 0;
  padding: 0;
}
&#10;#htcmbibwce .gt_table {
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
&#10;#htcmbibwce .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#htcmbibwce .gt_title {
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
&#10;#htcmbibwce .gt_subtitle {
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
&#10;#htcmbibwce .gt_heading {
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
&#10;#htcmbibwce .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#htcmbibwce .gt_col_headings {
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
&#10;#htcmbibwce .gt_col_heading {
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
&#10;#htcmbibwce .gt_column_spanner_outer {
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
&#10;#htcmbibwce .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#htcmbibwce .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#htcmbibwce .gt_column_spanner {
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
&#10;#htcmbibwce .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#htcmbibwce .gt_group_heading {
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
&#10;#htcmbibwce .gt_empty_group_heading {
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
&#10;#htcmbibwce .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#htcmbibwce .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#htcmbibwce .gt_row {
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
&#10;#htcmbibwce .gt_stub {
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
&#10;#htcmbibwce .gt_stub_row_group {
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
&#10;#htcmbibwce .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#htcmbibwce .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#htcmbibwce .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#htcmbibwce .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#htcmbibwce .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#htcmbibwce .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#htcmbibwce .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#htcmbibwce .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#htcmbibwce .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#htcmbibwce .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#htcmbibwce .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#htcmbibwce .gt_footnotes {
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
&#10;#htcmbibwce .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#htcmbibwce .gt_sourcenotes {
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
&#10;#htcmbibwce .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#htcmbibwce .gt_left {
  text-align: left;
}
&#10;#htcmbibwce .gt_center {
  text-align: center;
}
&#10;#htcmbibwce .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#htcmbibwce .gt_font_normal {
  font-weight: normal;
}
&#10;#htcmbibwce .gt_font_bold {
  font-weight: bold;
}
&#10;#htcmbibwce .gt_font_italic {
  font-style: italic;
}
&#10;#htcmbibwce .gt_super {
  font-size: 65%;
}
&#10;#htcmbibwce .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#htcmbibwce .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#htcmbibwce .gt_indent_1 {
  text-indent: 5px;
}
&#10;#htcmbibwce .gt_indent_2 {
  text-indent: 10px;
}
&#10;#htcmbibwce .gt_indent_3 {
  text-indent: 15px;
}
&#10;#htcmbibwce .gt_indent_4 {
  text-indent: 20px;
}
&#10;#htcmbibwce .gt_indent_5 {
  text-indent: 25px;
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

<div id="iozloiedxn"
style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">

<style>#iozloiedxn table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#iozloiedxn thead, #iozloiedxn tbody, #iozloiedxn tfoot, #iozloiedxn tr, #iozloiedxn td, #iozloiedxn th {
  border-style: none;
}
&#10;#iozloiedxn p {
  margin: 0;
  padding: 0;
}
&#10;#iozloiedxn .gt_table {
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
&#10;#iozloiedxn .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#iozloiedxn .gt_title {
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
&#10;#iozloiedxn .gt_subtitle {
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
&#10;#iozloiedxn .gt_heading {
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
&#10;#iozloiedxn .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#iozloiedxn .gt_col_headings {
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
&#10;#iozloiedxn .gt_col_heading {
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
&#10;#iozloiedxn .gt_column_spanner_outer {
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
&#10;#iozloiedxn .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#iozloiedxn .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#iozloiedxn .gt_column_spanner {
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
&#10;#iozloiedxn .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#iozloiedxn .gt_group_heading {
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
&#10;#iozloiedxn .gt_empty_group_heading {
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
&#10;#iozloiedxn .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#iozloiedxn .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#iozloiedxn .gt_row {
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
&#10;#iozloiedxn .gt_stub {
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
&#10;#iozloiedxn .gt_stub_row_group {
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
&#10;#iozloiedxn .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#iozloiedxn .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#iozloiedxn .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#iozloiedxn .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#iozloiedxn .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#iozloiedxn .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#iozloiedxn .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#iozloiedxn .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#iozloiedxn .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#iozloiedxn .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#iozloiedxn .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#iozloiedxn .gt_footnotes {
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
&#10;#iozloiedxn .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#iozloiedxn .gt_sourcenotes {
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
&#10;#iozloiedxn .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#iozloiedxn .gt_left {
  text-align: left;
}
&#10;#iozloiedxn .gt_center {
  text-align: center;
}
&#10;#iozloiedxn .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#iozloiedxn .gt_font_normal {
  font-weight: normal;
}
&#10;#iozloiedxn .gt_font_bold {
  font-weight: bold;
}
&#10;#iozloiedxn .gt_font_italic {
  font-style: italic;
}
&#10;#iozloiedxn .gt_super {
  font-size: 65%;
}
&#10;#iozloiedxn .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#iozloiedxn .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#iozloiedxn .gt_indent_1 {
  text-indent: 5px;
}
&#10;#iozloiedxn .gt_indent_2 {
  text-indent: 10px;
}
&#10;#iozloiedxn .gt_indent_3 {
  text-indent: 15px;
}
&#10;#iozloiedxn .gt_indent_4 {
  text-indent: 20px;
}
&#10;#iozloiedxn .gt_indent_5 {
  text-indent: 25px;
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

<div id="suyzhpxtgr"
style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">

<style>#suyzhpxtgr table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#suyzhpxtgr thead, #suyzhpxtgr tbody, #suyzhpxtgr tfoot, #suyzhpxtgr tr, #suyzhpxtgr td, #suyzhpxtgr th {
  border-style: none;
}
&#10;#suyzhpxtgr p {
  margin: 0;
  padding: 0;
}
&#10;#suyzhpxtgr .gt_table {
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
&#10;#suyzhpxtgr .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#suyzhpxtgr .gt_title {
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
&#10;#suyzhpxtgr .gt_subtitle {
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
&#10;#suyzhpxtgr .gt_heading {
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
&#10;#suyzhpxtgr .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#suyzhpxtgr .gt_col_headings {
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
&#10;#suyzhpxtgr .gt_col_heading {
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
&#10;#suyzhpxtgr .gt_column_spanner_outer {
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
&#10;#suyzhpxtgr .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#suyzhpxtgr .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#suyzhpxtgr .gt_column_spanner {
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
&#10;#suyzhpxtgr .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#suyzhpxtgr .gt_group_heading {
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
&#10;#suyzhpxtgr .gt_empty_group_heading {
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
&#10;#suyzhpxtgr .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#suyzhpxtgr .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#suyzhpxtgr .gt_row {
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
&#10;#suyzhpxtgr .gt_stub {
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
&#10;#suyzhpxtgr .gt_stub_row_group {
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
&#10;#suyzhpxtgr .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#suyzhpxtgr .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#suyzhpxtgr .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#suyzhpxtgr .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#suyzhpxtgr .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#suyzhpxtgr .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#suyzhpxtgr .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#suyzhpxtgr .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#suyzhpxtgr .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#suyzhpxtgr .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#suyzhpxtgr .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#suyzhpxtgr .gt_footnotes {
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
&#10;#suyzhpxtgr .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#suyzhpxtgr .gt_sourcenotes {
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
&#10;#suyzhpxtgr .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#suyzhpxtgr .gt_left {
  text-align: left;
}
&#10;#suyzhpxtgr .gt_center {
  text-align: center;
}
&#10;#suyzhpxtgr .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#suyzhpxtgr .gt_font_normal {
  font-weight: normal;
}
&#10;#suyzhpxtgr .gt_font_bold {
  font-weight: bold;
}
&#10;#suyzhpxtgr .gt_font_italic {
  font-style: italic;
}
&#10;#suyzhpxtgr .gt_super {
  font-size: 65%;
}
&#10;#suyzhpxtgr .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#suyzhpxtgr .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#suyzhpxtgr .gt_indent_1 {
  text-indent: 5px;
}
&#10;#suyzhpxtgr .gt_indent_2 {
  text-indent: 10px;
}
&#10;#suyzhpxtgr .gt_indent_3 {
  text-indent: 15px;
}
&#10;#suyzhpxtgr .gt_indent_4 {
  text-indent: 20px;
}
&#10;#suyzhpxtgr .gt_indent_5 {
  text-indent: 25px;
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

# last_table() |>
#   set_rows(c(user, repo))


read_csv("https://raw.githubusercontent.com/EvaMaeRey/mytidytuesday/refs/heads/main/2024-12-10-ggplot2-layer-composition/ggplot2_exported_layer_fun_composition.csv") %>% 
  rename(prefix = fun_prefix) ->
ggplot2_layers_definers

ggplot2_layers_definers |>
  ggtable()
```

<div id="rhegidynrd"
style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">

<style>#rhegidynrd table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#rhegidynrd thead, #rhegidynrd tbody, #rhegidynrd tfoot, #rhegidynrd tr, #rhegidynrd td, #rhegidynrd th {
  border-style: none;
}
&#10;#rhegidynrd p {
  margin: 0;
  padding: 0;
}
&#10;#rhegidynrd .gt_table {
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
&#10;#rhegidynrd .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#rhegidynrd .gt_title {
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
&#10;#rhegidynrd .gt_subtitle {
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
&#10;#rhegidynrd .gt_heading {
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
&#10;#rhegidynrd .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#rhegidynrd .gt_col_headings {
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
&#10;#rhegidynrd .gt_col_heading {
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
&#10;#rhegidynrd .gt_column_spanner_outer {
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
&#10;#rhegidynrd .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#rhegidynrd .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#rhegidynrd .gt_column_spanner {
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
&#10;#rhegidynrd .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#rhegidynrd .gt_group_heading {
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
&#10;#rhegidynrd .gt_empty_group_heading {
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
&#10;#rhegidynrd .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#rhegidynrd .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#rhegidynrd .gt_row {
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
&#10;#rhegidynrd .gt_stub {
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
&#10;#rhegidynrd .gt_stub_row_group {
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
&#10;#rhegidynrd .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#rhegidynrd .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#rhegidynrd .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#rhegidynrd .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#rhegidynrd .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#rhegidynrd .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#rhegidynrd .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#rhegidynrd .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#rhegidynrd .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#rhegidynrd .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#rhegidynrd .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#rhegidynrd .gt_footnotes {
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
&#10;#rhegidynrd .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#rhegidynrd .gt_sourcenotes {
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
&#10;#rhegidynrd .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#rhegidynrd .gt_left {
  text-align: left;
}
&#10;#rhegidynrd .gt_center {
  text-align: center;
}
&#10;#rhegidynrd .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#rhegidynrd .gt_font_normal {
  font-weight: normal;
}
&#10;#rhegidynrd .gt_font_bold {
  font-weight: bold;
}
&#10;#rhegidynrd .gt_font_italic {
  font-style: italic;
}
&#10;#rhegidynrd .gt_super {
  font-size: 65%;
}
&#10;#rhegidynrd .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#rhegidynrd .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#rhegidynrd .gt_indent_1 {
  text-indent: 5px;
}
&#10;#rhegidynrd .gt_indent_2 {
  text-indent: 10px;
}
&#10;#rhegidynrd .gt_indent_3 {
  text-indent: 15px;
}
&#10;#rhegidynrd .gt_indent_4 {
  text-indent: 20px;
}
&#10;#rhegidynrd .gt_indent_5 {
  text-indent: 25px;
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

<div id="hmmwdxyrxl"
style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">

<style>#hmmwdxyrxl table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#hmmwdxyrxl thead, #hmmwdxyrxl tbody, #hmmwdxyrxl tfoot, #hmmwdxyrxl tr, #hmmwdxyrxl td, #hmmwdxyrxl th {
  border-style: none;
}
&#10;#hmmwdxyrxl p {
  margin: 0;
  padding: 0;
}
&#10;#hmmwdxyrxl .gt_table {
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
&#10;#hmmwdxyrxl .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#hmmwdxyrxl .gt_title {
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
&#10;#hmmwdxyrxl .gt_subtitle {
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
&#10;#hmmwdxyrxl .gt_heading {
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
&#10;#hmmwdxyrxl .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#hmmwdxyrxl .gt_col_headings {
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
&#10;#hmmwdxyrxl .gt_col_heading {
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
&#10;#hmmwdxyrxl .gt_column_spanner_outer {
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
&#10;#hmmwdxyrxl .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#hmmwdxyrxl .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#hmmwdxyrxl .gt_column_spanner {
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
&#10;#hmmwdxyrxl .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#hmmwdxyrxl .gt_group_heading {
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
&#10;#hmmwdxyrxl .gt_empty_group_heading {
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
&#10;#hmmwdxyrxl .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#hmmwdxyrxl .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#hmmwdxyrxl .gt_row {
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
&#10;#hmmwdxyrxl .gt_stub {
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
&#10;#hmmwdxyrxl .gt_stub_row_group {
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
&#10;#hmmwdxyrxl .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#hmmwdxyrxl .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#hmmwdxyrxl .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#hmmwdxyrxl .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#hmmwdxyrxl .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#hmmwdxyrxl .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#hmmwdxyrxl .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#hmmwdxyrxl .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#hmmwdxyrxl .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#hmmwdxyrxl .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#hmmwdxyrxl .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#hmmwdxyrxl .gt_footnotes {
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
&#10;#hmmwdxyrxl .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#hmmwdxyrxl .gt_sourcenotes {
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
&#10;#hmmwdxyrxl .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#hmmwdxyrxl .gt_left {
  text-align: left;
}
&#10;#hmmwdxyrxl .gt_center {
  text-align: center;
}
&#10;#hmmwdxyrxl .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#hmmwdxyrxl .gt_font_normal {
  font-weight: normal;
}
&#10;#hmmwdxyrxl .gt_font_bold {
  font-weight: bold;
}
&#10;#hmmwdxyrxl .gt_font_italic {
  font-style: italic;
}
&#10;#hmmwdxyrxl .gt_super {
  font-size: 65%;
}
&#10;#hmmwdxyrxl .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#hmmwdxyrxl .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#hmmwdxyrxl .gt_indent_1 {
  text-indent: 5px;
}
&#10;#hmmwdxyrxl .gt_indent_2 {
  text-indent: 10px;
}
&#10;#hmmwdxyrxl .gt_indent_3 {
  text-indent: 15px;
}
&#10;#hmmwdxyrxl .gt_indent_4 {
  text-indent: 20px;
}
&#10;#hmmwdxyrxl .gt_indent_5 {
  text-indent: 25px;
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

<div id="mrnrttmwlu"
style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">

<style>#mrnrttmwlu table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#mrnrttmwlu thead, #mrnrttmwlu tbody, #mrnrttmwlu tfoot, #mrnrttmwlu tr, #mrnrttmwlu td, #mrnrttmwlu th {
  border-style: none;
}
&#10;#mrnrttmwlu p {
  margin: 0;
  padding: 0;
}
&#10;#mrnrttmwlu .gt_table {
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
&#10;#mrnrttmwlu .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#mrnrttmwlu .gt_title {
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
&#10;#mrnrttmwlu .gt_subtitle {
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
&#10;#mrnrttmwlu .gt_heading {
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
&#10;#mrnrttmwlu .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#mrnrttmwlu .gt_col_headings {
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
&#10;#mrnrttmwlu .gt_col_heading {
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
&#10;#mrnrttmwlu .gt_column_spanner_outer {
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
&#10;#mrnrttmwlu .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#mrnrttmwlu .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#mrnrttmwlu .gt_column_spanner {
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
&#10;#mrnrttmwlu .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#mrnrttmwlu .gt_group_heading {
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
&#10;#mrnrttmwlu .gt_empty_group_heading {
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
&#10;#mrnrttmwlu .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#mrnrttmwlu .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#mrnrttmwlu .gt_row {
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
&#10;#mrnrttmwlu .gt_stub {
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
&#10;#mrnrttmwlu .gt_stub_row_group {
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
&#10;#mrnrttmwlu .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#mrnrttmwlu .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#mrnrttmwlu .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#mrnrttmwlu .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#mrnrttmwlu .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#mrnrttmwlu .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#mrnrttmwlu .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#mrnrttmwlu .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#mrnrttmwlu .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#mrnrttmwlu .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#mrnrttmwlu .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#mrnrttmwlu .gt_footnotes {
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
&#10;#mrnrttmwlu .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#mrnrttmwlu .gt_sourcenotes {
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
&#10;#mrnrttmwlu .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#mrnrttmwlu .gt_left {
  text-align: left;
}
&#10;#mrnrttmwlu .gt_center {
  text-align: center;
}
&#10;#mrnrttmwlu .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#mrnrttmwlu .gt_font_normal {
  font-weight: normal;
}
&#10;#mrnrttmwlu .gt_font_bold {
  font-weight: bold;
}
&#10;#mrnrttmwlu .gt_font_italic {
  font-style: italic;
}
&#10;#mrnrttmwlu .gt_super {
  font-size: 65%;
}
&#10;#mrnrttmwlu .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#mrnrttmwlu .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#mrnrttmwlu .gt_indent_1 {
  text-indent: 5px;
}
&#10;#mrnrttmwlu .gt_indent_2 {
  text-indent: 10px;
}
&#10;#mrnrttmwlu .gt_indent_3 {
  text-indent: 15px;
}
&#10;#mrnrttmwlu .gt_indent_4 {
  text-indent: 20px;
}
&#10;#mrnrttmwlu .gt_indent_5 {
  text-indent: 25px;
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
#> 1 Male       5
#> 2 Female     0

flat_titanic |> pivot_samplen(rows = sex, value = freq)
#> # A tibble: 2 × 2
#>   sex    value     
#>   <fct>  <chr>     
#> 1 Male   75; 11; 57
#> 2 Female 0; 17; 76

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
