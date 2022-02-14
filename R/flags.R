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

  soc_2000 <- c(
    # "2311  'Higher educ teaching prfsnals'",
    # "2312  'Further educ teaching prfsnals'",
    "2314  'Secondary eductn teaching prfsnals'",
    "2315  'Prim & nurs eductn teaching profs'"
  )

  soc_2010 <- c(
    "2314  'Secondary education teaching professionals'",
    "2315  'Primary and nursery education teaching professionals'"
  )

  soc_2020 <- c(
    # "Further education teaching professionals",
    "Secondary education teaching professionals",
    "Primary education teaching professionals",
    "Nursery education teaching professionals"
    # "Special needs education teaching professionals"))
  )

  lfs <- lfs %>%
    dplyr::mutate(
      column_name = .data$OCCUPATION %in% c(soc_2000, soc_2010, soc_2020)
    ) %>%
    dplyr::rename(rename)
}


#' Add a gradaute flag column to LFS data
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



#' Add anm ONS graduate flag column to LFS data
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
