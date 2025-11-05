#' @export
no_pivot <- function(tp){
  
  tp$pivot <- FALSE

  last_tp <<- tp

  tp  
  
}

collect <- function(tp){
  
  do.call(pivotr, tp)
  
}
