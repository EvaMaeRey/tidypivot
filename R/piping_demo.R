## piping demo

library(R6)


library(tidytitanic)
pivot_helper(data = passengers, rows = sex, cols = survived)

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
                         self$out <- 'tidypivot::pivot_helper(data, rows, cols, fun, wt, value, pivot_logical)'

                         if(!is.null(self$data)) {self$out <- self$out %>% stringr::str_replace("data",  self$data)  }
                         if(!is.null(self$rows)) {self$out <- self$out %>% stringr::str_replace("rows",  self$rows)  }
                         if(!is.null(self$cols)) {self$out <- self$out %>% stringr::str_replace("cols",  self$cols)  }
                         if(!is.null(self$fun))  {self$out <- self$out %>% stringr::str_replace("fun",   self$fun)   }
                         if(!is.null(self$wt))   {self$out <- self$out %>% stringr::str_replace("wt",    self$wt)    }
                         if(!is.null(self$value)){self$out <- self$out %>% stringr::str_replace("value", self$value) }
                         if(!is.null(self$pivot)){self$out <- self$out %>% stringr::str_replace("pivot_logical", self$pivot) }

                         if(is.null(self$data)) {self$out <- self$out %>% stringr::str_replace("data",  "NULL")}
                         if(is.null(self$rows)) {self$out <- self$out %>% stringr::str_replace("rows",  "NULL")}
                         if(is.null(self$cols)) {self$out <- self$out %>% stringr::str_replace("cols",  "NULL")}
                         if(is.null(self$fun))  {self$out <- self$out %>% stringr::str_replace("fun",   "NULL")}
                         if(is.null(self$wt))   {self$out <- self$out %>% stringr::str_replace("wt",    "NULL")}
                         if(is.null(self$value)){self$out <- self$out %>% stringr::str_replace("value", "NULL")}
                         if(is.null(self$pivot)){self$out <- self$out %>% stringr::str_replace("pivot_logical", "NULL")}

                         invisible(self)          #returns

                       },

                       print = function() {  # print method; default is to print everything

                         # str(self)
                         self$out


                       }
                     )
)


eval(parse(text = 'tidypivot::pivot_helper(data = tidy_titanic)'))

library(tidytitanic)
library(magrittr)
tidy_titanic %>%
  tidypivot::pivot_helper()


flat_titanic %>%
  tidypivot::pivot_helper(cols = sex, value = freq, fun = sum, pivot = F)

my_tp <- Tidypivot$new()

my_tp

my_tp$update(data = "tidy_titanic")
my_tp$out
my_tp$update(rows = "class")
my_tp$out

my_tp$update(cols = "survived")
my_tp$out
my_tp$update(cols = "c(survived, sex)")


tidypivot::pivot_helper(tidy_titanic, class, c(survived, sex), NULL, NULL, NULL, NULL)

user_function_initiate <- function(data = NULL, rows = NULL, cols = NULL,
                          fun = NULL, value = NULL, wt = NULL, pivot_logical = NULL){

  my_tp <<- Tidypivot$new()


}


user_function_continue <- function(nothing, data = NULL, rows = NULL, cols = NULL,
                                   fun = NULL, value = NULL, wt = NULL, pivot_logical = NULL){

  my_tp$update(data,
               rows,
               cols,
               fun,
               value,
               wt,
               pivot_logical)
  eval(parse(text =  my_tp$out))

}

user_function_initiate() %>%
  user_function_continue(data = "tidy_titanic") %>%
  user_function_continue(rows = "sex") %>%
  user_function_continue(cols = "survived") %>%
  user_function_continue(cols = "c(survived, class)") %>%
  dplyr::select(sex)


rlang::quo_name(sex)


my_function <- function(obj) {
  deparse(substitute(obj))
}

# Example usage
my_variable <- 42
my_function(my_variable)

my_function(sex)

deparse(substitute(obj))
