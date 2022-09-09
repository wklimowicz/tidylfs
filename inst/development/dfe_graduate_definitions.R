# Functions ----------------------------------------
lfs_flag_graduate2 <- function(lfs, column_name = "GRADUATE") {
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
    dplyr::rename(all_of(rename))
}


lfs_flag_postgraduate2 <- function(lfs, column_name = "POSTGRADUATE") {
  rename <- c("column_name")
  names(rename) <- {{ column_name }}

  lfs <- lfs %>%
    dplyr::mutate(column_name = ifelse(
      .data$HIQUAL == "Higher degree" &
        !.data$HIGHO %in% c("Dont know"), 1, 0
    )) %>%
    dplyr::rename(all_of(rename))
}

lfs_flag_notinscope <-  function(lfs, column_name = "NOTSCOPE") {
  rename <- c("column_name")
  names(rename) <- {{ column_name }}

  lfs %>%
    dplyr::mutate(column_name = case_when(
HIQUAL == "Level 8 Diploma" |
HIQUAL == "Level 8 Certificate" |
HIQUAL == "Level 8 Award" |
HIQUAL == "Level 7 Certificate" |
HIQUAL == "Level 7 Diploma" |
HIQUAL == 'Level 7 Award' |
HIQUAL == 'Level 6 Award' |
HIQUAL == 'Level 6 Certificate' |
HIQUAL == 'Level 6 Diploma' |
HIQUAL == "Don.t know" |
HIQUAL == "Don't know" |
HIQUAL == "Did not know" |
(HIQUAL == "Higher degree" & HIQUAL == "Dont know") |
(HIQUAL == "Higher degree" & is.na(HIGHO)) ~ 1,
TRUE ~ 0
    )) %>%
    dplyr::rename(all_of(rename))

}


