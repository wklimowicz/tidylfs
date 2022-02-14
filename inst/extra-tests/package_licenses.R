pak <- c(
    "testthat",
    "curl",
    "janitor",
    "readxl",
    "tidyr",
    "vctrs",
    "knitr",
    "rmarkdown",
    "haven",
    "stringr",
    "rlang",
    "stats",
    "dplyr",
    "purrr",
    "jsonlite",
    "readr",
    "magrittr",
    "tibble",
    "forcats",
    "openssl",
    "lifecycle",
    "cli"
    )



dplyr::bind_cols(pak, purrr::map_chr(pak, ~ packageDescription(.x, fields="License"))) %>%
  dplyr::filter(...2 != "MIT + file LICENSE")



# dplyr
pak <- c(
    "generics",
    "glue",
    "lifecycle",
    "magrittr",
    "methods",
    "R6",
    "rlang",
    "tibble",
    "tidyselect",
    "utils",
    "vctrs",
    "pillar",
    "bench",
    "broom",
    "callr",
    "covr",
    "DBI",
    "dbplyr",
    "ggplot2",
    "knitr",
    "Lahman",
    "lobstr",
    "microbenchmark",
    "nycflights13",
    "purrr",
    "rmarkdown",
    "RMySQL",
    "RPostgreSQL",
    "RSQLite",
    "testthat",
    "tidyr")
