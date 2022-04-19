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

  soc_coding_last <- dplyr::bind_rows(soc2km, soc10m, soc20m) %>%
    dplyr::rename(
      LAST_OCCUPATION = .data$SOC,
      LAST_OCCUPATION_DESCRIPTION = .data$OCCUPATION_DESCRIPTION
    )

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

  if ("LAST_OCCUPATION" %in% names(lfs)) {
    lfs1 <- lfs1 %>%
        dplyr::left_join(soc_coding_last, by = c("SOC_TYPE", "LAST_OCCUPATION"))
  }

  if ("PARENTAL_OCCUPATION" %in% names(lfs)) {
    lfs1 <- lfs1 %>%
      dplyr::left_join(soc_coding_parental, by = c("SOC_TYPE", "PARENTAL_OCCUPATION"))
  }



  # QA
  # dplyr::count(lfs1, QUARTER, PARENTAL_OCCUPATION_DESCRIPTION, PARENTAL_OCCUPATION)

  lfs1 %>%
    dplyr::select(-.data$SOC_TYPE)
}

annotate_industry <- function(lfs) {

  # Find correct coding file
  read_industry_coding <- function(sic) {
    readr::read_csv(
      paste0(system.file("coding_frames", package = "tidylfs"), "/industry_", sic, ".csv"),
      col_types = readr::cols(
        SIC = readr::col_character(),
        INDUSTRY_DESCRIPTION = readr::col_character()
      )
    ) %>%
      dplyr::mutate(SIC_TYPE = sic)
  }

  sic07 <- read_industry_coding("SIC07")

  # SIC92 is complicated...
  sic92 <- readr::read_csv(
      paste0(system.file("coding_frames", package = "tidylfs"), "/industry_SIC92.csv"),
      col_types = readr::cols(
        SIC = readr::col_character(),
        Industry_Code = readr::col_character(),
        LFS_Description = readr::col_character(),
        ONS_Description = readr::col_character()
      )
    ) %>%
      dplyr::mutate(SIC_TYPE = "SIC92") %>%
      dplyr::select(-.data$Industry_Code, -.data$LFS_Description) %>%
      dplyr::rename(INDUSTRY_DESCRIPTION = .data$ONS_Description)

  sic_coding <- dplyr::bind_rows(sic07, sic92) %>%
    dplyr::rename(INDUSTRY = .data$SIC)

  lfs1 <- lfs %>%
    dplyr::mutate(SIC_TYPE = dplyr::case_when(
      .data$YEAR >= 1992 & .data$YEAR < 2009 ~ "SIC92",
      .data$YEAR >= 2009 ~ "SIC07"
    )) %>%
    dplyr::left_join(sic_coding, by = c("SIC_TYPE", "INDUSTRY"))

  lfs1 %>%
    dplyr::select(-.data$SIC_TYPE)


}
