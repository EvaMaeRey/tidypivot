#' @export
set_fun <- function(tp, fun = sum){

  tp$fun <- fun

  last_tp <<- tp

  tp
  
}


#' @export
set_fun_example <- function(tp){

  tp$fun <- function(x) sample(x, 1)


  last_tp <<- tp

  tp
  
}


#' @export
set_fun_sample <- function(tp, n = 2, sep = "; "){

  tp$fun <- function(x) paste(sample(x, n, replace = F), collapse = sep)


  last_tp <<- tp

  tp
  
}


#' @export
set_fun_list <- function(tp, sep = "; "){

  tp$fun <- function(x) paste(x, collapse = sep)


  last_tp <<- tp

  tp
  
}


