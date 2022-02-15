annotate_degree71 <- function(YEAR, degree_name) {
  dplyr::case_when(
    (YEAR >= 2004 & {{ degree_name }} == 1) ~ "A higher degree (including PGCE)",
    (YEAR >= 2004 & {{ degree_name }} == 2) ~ "A first degree",
    (YEAR >= 2004 & {{ degree_name }} == 3) ~ "A foundation degree",
    (YEAR >= 2004 & {{ degree_name }} == 4) ~ "A graduate membership of a professional institution",
    (YEAR >= 2004 & {{ degree_name }} == 5) ~ "Other",
    (YEAR >= 2004 & {{ degree_name }} == 6) ~ "Don't know",
    (YEAR < 2004 & {{ degree_name }} == 1) ~ "A higher degree (including PGCE)",
    (YEAR < 2004 & {{ degree_name }} == 2) ~ "A first degree",
    (YEAR < 2004 & {{ degree_name }} == 3) ~ "Other (e.g. graduate member of a professional institute or chartered accountant)",
    (YEAR < 2004 & {{ degree_name }} == 4) ~ "Don't know"
  )
}

annotate_hiquald <- function(data) {
  data %>%
    dplyr::mutate(HIQUALD = dplyr::case_when(
      .data$HIQUALD == 1 ~ "Degree or equivalent",
      .data$HIQUALD == 2 ~ "Higher education",
      .data$HIQUALD == 3 ~ "GCE, A-level or equivalent",
      .data$HIQUALD == 4 ~ "GCSE grades A*-C or equivalent",
      .data$HIQUALD == 5 ~ "Other qualifications",
      .data$HIQUALD == 6 ~ "No qualification",
      .data$HIQUALD == 7 ~ "Don't know"
    )) %>%
    dplyr::mutate(HIQUALD = forcats::as_factor(.data$HIQUALD)) %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::starts_with("DEGREE7"),
        ~ annotate_degree71(YEAR, .x)
      )
    ) %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::starts_with("DEGREE7"),
        forcats::as_factor
      )
    )
}


annotate_occupation <- function(lfs) {

  cols <- names(lfs)

  soc_coding <- lfs_pick_column(c("SOC2KM", "SOC10M", "SOC20M"), cols)
  parent_soc_coding <- lfs_pick_column(c("SMSOC204", "SMSOC104"), cols)



  if (is.na(soc_coding)) {
  return(lfs)
  }

  # Find correct coding file
    file_name <- paste0(system.file("coding_frames", package = "tidylfs"),
                        "/coding_", soc_coding, ".csv")

  # Read in relevant coding file
  soc_mapping <- readr::read_csv(file_name,
    col_types = readr::cols(
      SOC = readr::col_double(),
      OCCUPATION_DESCRIPTION = readr::col_character()
  ))

  # Match main OCCUPATION variable
  join_vector <- "SOC"
  names(join_vector) <- soc_coding
  output <- dplyr::left_join(lfs, soc_mapping, join_vector)


  if (!is.na(parent_soc_coding)) {
    # Match Parental occupation variable
    join_vector <- "SOC"
    names(join_vector) <- parent_soc_coding
    colnames(soc_mapping) <- c("SOC", "PARENTAL_OCCUPATION_DESCRIPTION")
    output <- dplyr::left_join(output, soc_mapping, join_vector)
  }

  output
}
