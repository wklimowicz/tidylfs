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

  # Find correct coding file
  read_occupation_coding <- function(soc) {
    readr::read_csv(
      paste0(system.file("coding_frames", package = "tidylfs"), "/occupation_", soc, ".csv"),
      col_types = readr::cols(
        SOC = readr::col_double(),
        OCCUPATION_DESCRIPTION = readr::col_character()
      )
    ) %>%
      dplyr::mutate(SOC_TYPE = soc)
  }
  soc2km <- read_occupation_coding("SOC2KM")
  soc10m <- read_occupation_coding("SOC10M")
  soc20m <- read_occupation_coding("SOC20M")

  soc_coding <- dplyr::bind_rows(soc2km, soc10m, soc20m) %>%
    dplyr::rename(OCCUPATION = .data$SOC)

  soc_coding_parental <- dplyr::bind_rows(soc2km, soc10m, soc20m) %>%
    dplyr::rename(
      PARENTAL_OCCUPATION = .data$SOC,
      PARENTAL_OCCUPATION_DESCRIPTION = .data$OCCUPATION_DESCRIPTION
    )

  lfs1 <- lfs %>%
    dplyr::mutate(SOC_TYPE = dplyr::case_when(
      .data$YEAR >= 2001 & .data$YEAR < 2011 ~ "SOC2KM",
      .data$YEAR >= 2011 & .data$YEAR < 2021 ~ "SOC10M",
      .data$YEAR >= 2021 ~ "SOC20M"
    )) %>%
    dplyr::left_join(soc_coding, by = c("SOC_TYPE", "OCCUPATION"))

  if ("PARENTAL_OCCUPATION" %in% names(lfs)) {
    lfs1 <- lfs1 %>%
      dplyr::left_join(soc_coding_parental, by = c("SOC_TYPE", "PARENTAL_OCCUPATION"))
  }



  # QA
  # dplyr::count(lfs1, QUARTER, PARENTAL_OCCUPATION_DESCRIPTION, PARENTAL_OCCUPATION) %>%sie()

  lfs1 %>%
    dplyr::select(-.data$SOC_TYPE)
}
