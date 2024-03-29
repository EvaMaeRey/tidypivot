## piping demo

library(R6)


# library(tidytitanic)
# pivot_helper(data = passengers, rows = sex, cols = survived)

Tidypivot <- R6::R6Class("Tidypivot",
                     public = list(

                       # objects
                       data = NULL,
                       rows = NULL,
                       cols = NULL,
                       fun = NULL,
                       value = NULL,
                       wt = NULL,
                       within = NULL,
                       withinfun = NULL,
                       pivot = NULL,
                       wrap = NULL,
                       out = NULL
                       ,

                       # set_rows = function(value) {
                       #          self$x <- enquo(value)
                       # }

                       # functions
                       update = function(data = NULL, rows = NULL, cols = NULL,
                                         fun = NULL, value = NULL, wt = NULL, pivot_logical = NULL){ # a method

                         # updating

                         if(!is.null(data)){self$data <- data}
                         if(!is.null(rows)){self$rows <- rows}
                         if(!is.null(cols)){self$cols <- cols}
                         if(!is.null(fun)){self$fun <- fun}
                         if(!is.null(wt)){self$wt <- wt}
                         if(!is.null(value)){self$value <- value}
                         if(!is.null(pivot_logical)){self$pivot <- pivot_logical}

                         # displaying
                         self$out <- 'tidypivot::pivot_helper(data, rows, cols, fun, value, wt, pivot_logical)'

                         if(!is.null(self$data)) {self$out <- self$out %>% stringr::str_replace("data",  self$data)  }
                         if(!is.null(self$rows)) {self$out <- self$out %>% stringr::str_replace("rows",  self$rows)  }
                         if(!is.null(self$cols)) {self$out <- self$out %>% stringr::str_replace("cols",  self$cols)  }
                         if(!is.null(self$fun))  {self$out <- self$out %>% stringr::str_replace("fun",   self$fun)   }
                         if(!is.null(self$value)){self$out <- self$out %>% stringr::str_replace("value", self$value) }
                         if(!is.null(self$wt))   {self$out <- self$out %>% stringr::str_replace("wt",    self$wt)    }
                         if(!is.null(self$pivot)){self$out <- self$out %>% stringr::str_replace("pivot_logical", self$pivot) }

                         if(is.null(self$data)) {self$out <- self$out %>% stringr::str_replace("data",  "NULL")}
                         if(is.null(self$rows)) {self$out <- self$out %>% stringr::str_replace("rows",  "NULL")}
                         if(is.null(self$cols)) {self$out <- self$out %>% stringr::str_replace("cols",  "NULL")}
                         if(is.null(self$fun))  {self$out <- self$out %>% stringr::str_replace("fun",   "NULL")}
                         if(is.null(self$value)){self$out <- self$out %>% stringr::str_replace("value", "NULL")}
                         if(is.null(self$wt))   {self$out <- self$out %>% stringr::str_replace("wt",    "NULL")}
                         if(is.null(self$pivot)){self$out <- self$out %>% stringr::str_replace("pivot_logical", "NULL")}

                         invisible(self)          #returns

                       },

                       print = function() {  # print method; default is to print everything

                         # str(self)
                         self$out


                       }
                     )
)


# eval(parse(text = 'tidypivot::pivot_helper(data = tidy_titanic)'))
#
# library(tidytitanic)
# library(magrittr)
# tidy_titanic %>%
#   tidypivot::pivot_helper()
#
#
# flat_titanic %>%
#   tidypivot::pivot_helper(cols = sex, value = freq, fun = sum, pivot = F)
#
# my_tp <- Tidypivot$new()
#
# my_tp
#
# my_tp$update(data = "tidy_titanic")
# my_tp$out
# my_tp$update(rows = "class")
# my_tp$out
#
# my_tp$update(cols = "survived")
# my_tp$out
# my_tp$update(cols = "c(survived, sex)")
#
#
# tidypivot::pivot_helper(tidy_titanic, class, c(survived, sex), NULL, NULL, NULL, NULL)
#
#' Title
#'
#' @param data
#' @param rows
#' @param cols
#' @param fun
#' @param value
#' @param wt
#' @param pivot_logical
#'
#' @return
#' @export
#'
#' @examples
#' tp_init(data = "tidytitanic::tidy_titanic")
tp_init <- function(data = NULL, quietly = FALSE){

  if(exists("my_tp")){rm(my_tp)}

  # my_tp <- Tidypivot$new()
  my_tp <<- Tidypivot$new()


  my_tp$update(data)

  if(!quietly){
  eval(parse(text =  my_tp$out))
  }

}


tp_init_pipe <- function(data = NULL, quietly = FALSE){

  # if(exists("my_tp")){rm(my_tp)}

  my_tp_pipe <- Tidypivot$new()
  # my_tp <<- Tidypivot$new()


  my_tp_pipe$update(data)

  if(!quietly){
    eval(parse(text =  my_tp_pipe$out))
  }

}
#
#
#' Title
#'
#' @param nothing
#' @param data
#' @param rows
#' @param cols
#' @param fun
#' @param value
#' @param wt
#' @param pivot_logical
#'
#' @return
#' @export
#'
#' @examples
#'tp_init(data = "tidytitanic::tidy_titanic")
#'   tp_add(rows = "sex")
#'   tp_add(cols = "class")
#'
#'tp_init(data = "tidytitanic::tidy_titanic") |>
#'   tp_add(rows = "sex") |>
#'   tp_add(cols = "class")
#'
tp_add <- function(nothing = NULL, data = NULL, rows = NULL, cols = NULL,
                                   fun = NULL, value = NULL, wt = NULL,
                   pivot_logical = NULL, quietly = FALSE){



  my_tp$update(data,
               rows,
               cols,
               fun,
               value,
               wt,
               pivot_logical)

  if(!quietly){
  eval(parse(text =  my_tp$out))
  }

}


tp_add_pipe <- function(nothing = NULL, data = NULL, rows = NULL, cols = NULL,
                   fun = NULL, value = NULL, wt = NULL,
                   pivot_logical = NULL, quietly = FALSE){



  my_tp_pipe$update(data,
               rows,
               cols,
               fun,
               value,
               wt,
               pivot_logical)

  if(!quietly){
    eval(parse(text =  my_tp_pipe$out))
  }

}

#
#
#' Title
#'
#' @param vars
#'
#' @return
#' @export
#'
#' @examples
#'   tp_init(data = "tidytitanic::tidy_titanic")
#'
#' set_data(data = tidytitanic::tidy_titanic) %>%
#'  set_rows(age)
set_rows <- function(first, vars){

  var_names = deparse(substitute(vars))

  var_names
  tp_add(rows = deparse(substitute(vars)))

}


#
#
#' Title
#'
#' @param vars
#'
#' @return
#' @export
#'
#' @examples
#' tp_init(data = "tidytitanic::tidy_titanic")
#'
#' set_data(data = tidytitanic::tidy_titanic) |>
#' set_rows(age) |>
#' set_cols(c(survived, sex))
set_cols <- function(first, vars){

  var_names = deparse(substitute(vars))

  var_names
  tp_add(cols = var_names)

}


#' Title
#'
#' @param vars
#'
#' @return
#' @export
#'
#' @examples
#' set_data(tidytitanic::tidy_titanic)
#'
set_data <- function(data){

  my_tp <<- Tidypivot$new()
  return(my_tp)

  my_tp$update(data = data,
               rows,
               cols,
               fun,
               value,
               wt,
               pivot_logical)


  eval(parse(text =  my_tp$out))


  # tp_add(data = deparse(substitute(data)))

}




# tp_init()

#
# tp_init()

# rlang::quo_name(sex)
#
#
# my_function <- function(obj) {
#   deparse(substitute(obj))
# }
#
# # Example usage
# my_variable <- 42
# my_function(my_variable)
#
# my_function(sex)
#
# deparse(substitute(obj))



# my_function <- function(first, obj) {
#   deparse(substitute(obj))
# }
#
# my_function(first, second)
#
# my_function("hi", second)
# "hi" %>% my_function(second)
# "hi" %>% my_function(second)
# cars %>% my_function(second)

