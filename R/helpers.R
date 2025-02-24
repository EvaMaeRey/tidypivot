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
