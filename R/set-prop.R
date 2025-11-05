
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
