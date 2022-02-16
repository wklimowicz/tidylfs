#' Add a teacher flag column to LFS data
#'
#' @param lfs LFS dataset
#' @param column_name Name of the column - by default TEACHER
#'
#' @return tibble with extra teacher column
#' @export
lfs_flag_teacher <- function(lfs, column_name = "TEACHER") {
  rename <- c("column_name")
  names(rename) <- {{ column_name }}


teaching_occupations <- c(
"Further education teaching professionals",
# "Head teachers and principals",
# "Higher education teaching professionals",
"Higher level teaching assistants",
"Nursery education teaching professionals",
"Primary and nursery education teaching professionals",
"Primary education teaching professionals",
"Secondary education teaching professionals",
"Special needs education teaching professionals",
# "Teachers of English as a foreign language",
"Teaching and other educational professionals n.e.c.",
"Teaching assistants",
"Teaching professionals n.e.c."
)


  lfs <- lfs %>%
    dplyr::mutate(
      column_name = .data$OCCUPATION_DESCRIPTION %in% teaching_occupations
    ) %>%
    dplyr::rename(rename)
}


#' Add a graduate flag column to LFS data
#'
#' @param lfs LFS dataset
#' @param column_name Name of the column - by default GRADUATE
#'
#'
#' @return tibble with extra teacher column
#' @export
lfs_flag_graduate <- function(lfs, column_name = "GRADUATE") {
  rename <- c("column_name")
  names(rename) <- {{ column_name }}

  lfs <- lfs %>%
    dplyr::mutate(column_name = ifelse(
      (.data$HIQUAL %in% c(
        "First/Foundation degree",
        "First degree",
        "First degree/foundation degree"
      )) &
        (.data$DEGREE71 %in% c("A first degree") |
          .data$DEGREE72 %in% c("A first degree") |
          .data$DEGREE73 %in% c("A first degree") |
          .data$DEGREE74 %in% c("A first degree") |
          .data$DEGREE75 %in% c("A first degree")),
      1, 0
    )) %>%
    dplyr::rename(rename)
}

#' Add a post-gradaute flag column to LFS data
#'
#' @param lfs LFS dataset
#' @param column_name Name of the column - by default POST-GRADUATE
#'
#'
#' @return tibble with extra teacher column
#' @export
lfs_flag_postgraduate <- function(lfs, column_name = "POSTGRADUATE") {
  rename <- c("column_name")
  names(rename) <- {{ column_name }}

  lfs <- lfs %>%
    dplyr::mutate(column_name = ifelse(
      .data$HIQUALD == "Higher education" &
        .data$HIGHO != c("Don't Know"), 1, 0
    )) %>%
    dplyr::rename(rename)
}



#' Add an ONS graduate flag column to LFS data
#'
#' @param lfs LFS dataset
#' @param column_name Name of the column - by default GRADUATE_ONS
#'
#'
#' @return tibble with extra teacher column
#' @export
lfs_flag_graduate_ons <- function(lfs, column_name = "GRADUATE_ONS") {
  rename <- c("column_name")
  names(rename) <- {{ column_name }}

  lfs <- lfs %>%
    dplyr::mutate(column_name = ifelse(
      (.data$HIQUALD %in% c(
        "Degree or equivalent",
        "Higher education"
      )), 1, 0
    )) %>%
    dplyr::rename(rename)
}
