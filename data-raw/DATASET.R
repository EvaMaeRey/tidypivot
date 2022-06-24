## code to prepare `DATASET` dataset goes here

  Titanic %>%
    data.frame() %>%
    janitor::clean_names(case = "snake") ->
flat_titanic

usethis::use_data(flat_titanic, overwrite = TRUE)


  flat_titanic %>%
    tidyr::uncount(freq) ->
tidy_titanic


usethis::use_data(tidy_titanic, overwrite = TRUE)
