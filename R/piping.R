new_tidypivot <- function(data = data.frame(),
                          rows = NULL,
                          columns = NULL,
                          value = NULL,
                          wt = NULL) {

  # table specification components !
  tp <- list(
    data = data,
    rows = rows,
    columns = columns,
    value = value,
    wt = wt
    # more 'slots' to be added
  )

  # declare class 'tidypivot'
  class(tp) <- "tidypivot"

  # Return the created object
  invisible(tp)

}


return_specified_table = function(tp){

      out <- 'tidypivot::pivot_helper(thedata, rows, cols, value, wt, fun)'

      str_replace_or_null <- function(x, pattern, replacement){
        
        if(is.null(replacement)){x |> str_replace(pattern, 'NULL')}
        else{x |> str_replace(pattern, replacement)}
        
      }
      
      out <- str_replace_or_null(out, "rows",  tp$rows)
      out <- str_replace_or_null(out, "cols",  tp$cols)
      out <- str_replace_or_null(out, "value",  tp$value)
      out <- str_replace_or_null(out, "wt",  tp$wt)
      out <- str_replace_or_null(out, "fun",  tp$fun)

      
      eval(parse(text = out))         
      
}


print.tidypivot <- function(tp){
  
  print(return_specified_table(tp))
  invisible(tp)
  
}

#' @export
ggtable <- function(data){
  
  thedata <<- data # don't love this

  tp <- new_tidypivot(deparse(substitute(thedata)))
  
  last_tp <<- tp
  
  tp

}

#' @export
set_rows <- function(tp, rows = NULL){
  
  tp$rows <- deparse(substitute(rows))
  
  last_tp <<- tp
  
  tp

  
}

#' @export
set_cols <- function(tp, cols = NULL){
  
tp$cols <- deparse(substitute(cols))

  tp
  
  last_tp <<- tp
  
  tp
  
}

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
