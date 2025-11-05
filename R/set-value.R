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
