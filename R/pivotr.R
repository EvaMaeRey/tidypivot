pivotr <- function(data,
                   rows = NULL,
                   cols = NULL,
                   
                   filter = NULL,
                   .by = NULL,
                   
                   value = NULL,
                   wt = NULL,
                       
                   fun = NULL,
                       
                   prop = FALSE,
                   percent = FALSE,
                   round = NULL,
                       
                   within = NULL,

                   pivot = NULL
){


  filter_quo  <- rlang::enquo(filter)

  if(!rlang::quo_is_null(filter_quo) ){
  data |> 
  dplyr::filter({{filter}}, .by = {{.by}}
                ) ->
data    
  }

  data |>
  data_define_value(value = {{value}}, wt = {{wt}}) |> 
  data_to_grouped(rows = {{rows}}, cols = {{cols}}) |>
  data_grouped_to_summarized(fun = fun) |>
  data_summarized_to_proportioned(prop = prop, percent = percent, within = {{within}}, round = round) |>
  data_proportioned_to_pivoted(pivot = pivot, cols = {{cols}})
  
}

