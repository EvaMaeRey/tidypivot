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
