#' @export
set_rows <- function(tp, rows = NULL){
  
  tp$rows <- enquo(rows)
  
  last_tp <<- tp
  
  tp

  
}


#' @export
set_cols <- function(tp, cols = NULL){
  
  tp$cols <- enquo(cols)

  last_tp <<- tp
  
  tp
  

}

#' @export
set_filter <- function(tp, filter = TRUE){
  
  if(!filter){tp$filter <- enquo(filter)}
  
  last_tp <<- tp
  
  tp
  

}
