#' A function that pretty much let's you do anything in the tidypivot space, is carefully crafted, and is adapted to make the other functions easy to use
#'
#' @inheritParams pivot_count
#'
#' @return
#' @export
#'
#' @examples
#' tidy_titanic %>% pivot_helper(rows = sex, cols = survived, fun = length) # pivot_count
#' flat_titanic %>% pivot_helper(rows = sex, value = freq, fun = mean) # pivot_calc
#' flat_titanic %>% pivot_helper(rows = sex, value = freq, fun = sum) # pivot_count (weighted sum)
#' nar <- function(x) return(NA)
#' flat_titanic %>% pivot_helper(rows = sex, cols = survived, fun = nar); #pivot_null
#' sample1 <- function(x) sample(x, 1)
#' flat_titanic %>% pivot_helper(rows = sex, cols = survived, fun = sample1, value = freq); #pivot_sample1
#' samplen <- function(x, n) paste(sample(x, 5, replace = F), collapse = ", ")
#' flat_titanic %>% pivot_helper(rows = sex, cols = survived, fun = samplen, value = freq); #pivot_samplen
#' paste_collapse <- function(x) paste (x, collapse = ", ")
#' flat_titanic %>% pivot_helper(rows = sex, fun = paste_collapse, value = freq) #pivot_list
pivot_helper <- function(data,
                       rows = NULL,
                       cols = NULL,
                       value = NULL,
                       wt = NULL,
                       within = NULL,
                       withinfun = NULL,
                       fun = NULL,
                       pivot = T,
                       wrap = F
){

    cols_quo <- rlang::enquo(cols)
    value_quo <- rlang::enquo(value)
    wt_quo <- rlang::enquo(wt)
    within_quo <- rlang::enquo(within)

    if(is.null(fun)){
    fun <- sum
    }

    grouped <- data %>%
      dplyr::group_by(dplyr::across(c({{cols}}, {{rows}})),
                      .drop = FALSE)

    if(rlang::quo_is_null(value_quo) ){

      summarized <- grouped %>%
        dplyr::mutate(value = 1) %>%
        dplyr::summarise(value = fun(value))

        # dplyr::summarize(value = dplyr::n())
    }else{

      summarized <- grouped %>%
        dplyr::summarise(value = fun({{value}}))

    }

    if(rlang::quo_is_null(within_quo) ){
    withined <- summarized
    }else{

      withined <- summarized %>%
        dplyr::group_by(dplyr::across(c({{within}})),
                        .drop = FALSE)
    }

    arranged <- withined


    ungrouped <- arranged %>%
      dplyr::ungroup()

    tidy <- ungrouped


    # do not pivot if argument pivot false or if no columns specified
    if(pivot == F | rlang::quo_is_null(cols_quo)){

      tidy
      # tidy %>%
      #   dplyr::rename(count = .data$value)

      # otherwise pivot by columns
    }else{

      tidy %>%
        tidyr::pivot_wider(names_from = {{cols}})

    }

  }
