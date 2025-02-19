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

  print(tp)
  
  last_tp <<- tp
  
}


#' @export
unpivot <- function(tp){
  
  tp$pivot <- FALSE

  print(tp)
  
  last_tp <<- tp
  
  
}
