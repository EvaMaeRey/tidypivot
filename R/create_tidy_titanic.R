#' Title
#'
#' @return
#' @export
#'
#' @examples
#' create_tidy_titanic()
create_tidy_titanic <- function(){

  Titanic %>%
    data.frame() %>%
    tidyr::uncount(Freq)

}
