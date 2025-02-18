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
