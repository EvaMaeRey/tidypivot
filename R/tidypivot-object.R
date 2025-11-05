#' @export
new_tidypivot <- function(data = data.frame(),
                          rows = NULL,
                          cols = NULL,
                          value = NULL,
                          wt = NULL,
                          fun = NULL,
                          filter = TRUE,
                          prop = FALSE,
                          percent = FALSE,
                          round = NULL,
                          within = NULL,
                          pivot = TRUE) {

  # table specification components !
  tp <- list(
    data = data,
    rows = rows,
    cols = cols,
    value = value,
    wt = wt,
    fun = fun,
    prop = prop,
    percent = percent,
    round = round,
    within = within,
    pivot = pivot
    # more 'slots' to be added
  )

  # declare class 'tidypivot'
  class(tp) <- "tidypivot"

  # Return the created object
  invisible(tp)

}

#' @export
print.tidypivot <- function(tp){
  
  print(do.call(pivotr, tp))
  
  invisible(tp)
  
}
