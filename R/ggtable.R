#' @export
ggtable <- function(data = NULL){
  
  # thedata <<- data # don't love this
  data <- data %||% data.frame()
  
  tp <- new_tidypivot()
  
  tp$data <- data
  
  last_tp <<- tp
  
  tp

}


#' @export
last_table <- function(){
  
  last_tp
  
}

