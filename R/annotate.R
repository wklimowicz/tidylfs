annotate_degree71 <- function(degree_name, YEAR) {

  data.table::fcase(
    YEAR >= 2004 & c(degree_name) == 1, "A higher degree (including PGCE)",
    YEAR >= 2004 & c(degree_name) == 2, "A first degree",
    YEAR >= 2004 & c(degree_name) == 3, "A foundation degree",
    YEAR >= 2004 & c(degree_name) == 4, "A graduate membership of a professional institution",
    YEAR >= 2004 & c(degree_name) == 5, "Other",
    YEAR >= 2004 & c(degree_name) == 6, "Don't know",
    YEAR < 2004 & c(degree_name) == 1, "A higher degree (including PGCE)",
    YEAR < 2004 & c(degree_name) == 2, "A first degree",
    YEAR < 2004 & c(degree_name) == 3, "Other (e.g. graduate member of a professional institute or chartered accountant)",
    YEAR < 2004 & c(degree_name) == 4, "Don't know"
  )
}

annotate_hiquald <- function(lfs) {

    names_vector <- lfs[, names(.SD), .SDcols = patterns("DEGREE7")]

    lfs[, HIQUALD := data.table::fcase(
      HIQUALD == 1, "Degree or equivalent",
      HIQUALD == 2, "Higher education",
      HIQUALD == 3, "GCE, A-level or equivalent",
      HIQUALD == 4, "GCSE grades A*-C or equivalent",
      HIQUALD == 5, "Other qualifications",
      HIQUALD == 6, "No qualification",
      HIQUALD == 7, "Don't know"
      )][, HIQUALD := forcats::as_factor(HIQUALD)
       ][, (names_vector) := lapply(.SD, annotate_degree71, YEAR), .SDcols = patterns("DEGREE7")
       ][, (names_vector) := lapply(.SD, forcats::as_factor), .SDcols = patterns("DEGREE7")]

    return(lfs)

}


annotate_degree <- function(lfs) {

  # Find correct coding file
  read_degree_coding <- function(degree) {
    readr::read_csv(
      paste0(system.file("coding_frames", package = "tidylfs"), "/degree_", degree, ".csv"),
      col_types = readr::cols(
        DEGREE_SUBJECT2 = readr::col_character(),
        DEGREE_DESCRIPTION = readr::col_character()
      )
    ) %>%
      dplyr::mutate(DEGREE_TYPE = degree)
  }
  sngdeg <- read_degree_coding("SNGDEG")
  fdsngdeg <- read_degree_coding("FDSNGDEG")

  degree_coding <- dplyr::bind_rows(sngdeg, fdsngdeg)
    # dplyr::rename(OCCUPATION = .data$SOC)

  first_two_digits <- stringr::regex("(\\d{1,2}\\.\\d{1,2}|\\d{1,2})")
  first_one_digit <- stringr::regex("(\\d{1,2})")

degree_coding1 <- degree_coding
    # dplyr::filter(stringr::str_detect(DEGREE_SUBJECT2, first_one_digit))


degree_coding2 <- degree_coding %>%
    dplyr::rename(DEGREE_DESCRIPTION_FULL = DEGREE_DESCRIPTION)


# TODO: Investigate NA's in FDSNGDEG

lfs <- lfs %>%
    dplyr::mutate(DEGREE_TYPE = dplyr::case_when(
      lubridate::yq(.data$QUARTER) >= lubridate::yq("2012 Q1") ~ "FDSNGDEG",
      lubridate::yq(.data$QUARTER) < lubridate::yq("2012 Q1") ~ "SNGDEG",
    )) %>%
    dplyr::mutate(DEGREE_SHORT2 = stringr::str_extract(.data$DEGREE_SUBJECT, first_two_digits)) %>%
    dplyr::mutate(DEGREE_SHORT1 = stringr::str_extract(.data$DEGREE_SUBJECT, first_one_digit)) %>%
    dplyr::mutate(CMBDEGREE = dplyr::coalesce(.data$CMBDEG_MAIN, .data$CMBDEG1)) %>%
    dplyr::mutate(DEGREE1 = dplyr::coalesce(.data$DEGREE_SHORT1, .data$CMBDEGREE)) %>%
    dplyr::left_join(degree_coding1, by = c("DEGREE_TYPE" = "DEGREE_TYPE",
                                           "DEGREE1" = "DEGREE_SUBJECT2")) %>%
    dplyr::left_join(degree_coding2, by = c("DEGREE_TYPE" = "DEGREE_TYPE",
                                           "DEGREE_SHORT2" = "DEGREE_SUBJECT2")) %>%
    dplyr::select(-DEGREE_TYPE, -DEGREE_SHORT2, -DEGREE_SHORT1)

    return(lfs)

  #   dplyr::left_join(degree_coding, by = c("DEGREE_SUBJECT2", "DEGREE_SUBJECT2"))


  # QA
  # dplyr::count(lfs1, QUARTER, PARENTAL_OCCUPATION_DESCRIPTION, PARENTAL_OCCUPATION)

  # lfs1 %>%
  #   dplyr::select(-.data$SOC_TYPE)


# lfs1[,.N,HIQUALD]

# lfs[HIQUALD == "Degree or equivalent",.N,DEGREE_DESCRIPTION_FULL
# ][,prop := N/sum(N) * 100][DEGREE_DESCRIPTION_FULL |> is.na()]


# lfs[HIQUALD == "Degree or equivalent",.N,DEGREE_DESCRIPTION
# ][,prop := N/sum(N) * 100][DEGREE_DESCRIPTION|> is.na()]

# lfs1[HIQUALD == "Degree or equivalent" & is.na(DEGREE_DESCRIPTION), CASENO]

# lfs[CASENO == 6007330510102]

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
    dplyr::rename(OCCUPATION = .data$SOC)|>
    data.table::setDT()

    # dplyr::mutate(OCCUPATION_DESCRIPTION = forcats::as_factor(OCCUPATION_DESCRIPTION))

  soc_coding_last <- dplyr::bind_rows(soc2km, soc10m, soc20m) %>%
    dplyr::rename(
      LAST_OCCUPATION = .data$SOC,
      LAST_OCCUPATION_DESCRIPTION = .data$OCCUPATION_DESCRIPTION
    ) |>
    data.table::setDT()

    # dplyr::mutate(LAST_OCCUPATION_DESCRIPTION = forcats::as_factor(LAST_OCCUPATION_DESCRIPTION))

  soc_coding_parental <- dplyr::bind_rows(soc2km, soc10m, soc20m) %>%
    dplyr::rename(
      PARENTAL_OCCUPATION = .data$SOC,
      PARENTAL_OCCUPATION_DESCRIPTION = .data$OCCUPATION_DESCRIPTION
    )  |>
    data.table::setDT()
    # dplyr::mutate(PARENTAL_OCCUPATION_DESCRIPTION = forcats::as_factor(PARENTAL_OCCUPATION_DESCRIPTION))



  lfs <- lfs[, SOC_TYPE := data.table::fcase(
      YEAR >= 2001 & YEAR < 2011, "SOC2KM",
      YEAR >= 2011 & YEAR < 2021, "SOC10M",
      YEAR >= 2021, "SOC20M"
      )][soc_coding, on = c("SOC_TYPE", "OCCUPATION"),
        OCCUPATION_DESCRIPTION := i.OCCUPATION_DESCRIPTION
        ][, OCCUPATION_DESCRIPTION := as.factor(OCCUPATION_DESCRIPTION)]

  if ("LAST_OCCUPATION" %in% names(lfs)) {
    lfs <- lfs[soc_coding_last, on = c("SOC_TYPE", "LAST_OCCUPATION"),
        LAST_OCCUPATION_DESCRIPTION := i.LAST_OCCUPATION_DESCRIPTION
        ][, LAST_OCCUPATION_DESCRIPTION := as.factor(LAST_OCCUPATION_DESCRIPTION)]
  }

  if ("PARENTAL_OCCUPATION" %in% names(lfs)) {
    lfs <- lfs[soc_coding_parental, on = c("SOC_TYPE", "PARENTAL_OCCUPATION"),
        PARENTAL_OCCUPATION_DESCRIPTION := i.PARENTAL_OCCUPATION_DESCRIPTION
        ][, PARENTAL_OCCUPATION_DESCRIPTION := as.factor(PARENTAL_OCCUPATION_DESCRIPTION)]
  }

  lfs[, SOC_TYPE := NULL]

  return(lfs)

  # QA
  # dplyr::count(lfs1, QUARTER, PARENTAL_OCCUPATION_DESCRIPTION, PARENTAL_OCCUPATION)

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
    dplyr::rename(INDUSTRY = .data$SIC) |>
    data.table::setDT()
    # dplyr::mutate(INDUSTRY_DESCRIPTION = forcats::as_factor(INDUSTRY_DESCRIPTION))


    lfs <- lfs[, SIC_TYPE := data.table::fcase(
      YEAR >= 1992 & YEAR < 2009, "SIC92",
      YEAR >= 2009, "SIC07"
      )][sic_coding, on = c("SIC_TYPE", "INDUSTRY"),
    INDUSTRY_DESCRIPTION := i.INDUSTRY_DESCRIPTION
    ][, SIC_TYPE := NULL
       ][, INDUSTRY_DESCRIPTION := forcats::as_factor(INDUSTRY_DESCRIPTION)]

    return(lfs)

}


annotate_economic_activity <- function(lfs) {

  # Find correct coding file
  read_economic_activity <- function(inecac) {
    readr::read_csv(
      paste0(system.file("coding_frames", package = "tidylfs"), "/economic_activity_", inecac, ".csv"),
      col_types = readr::cols(
        INECAC = readr::col_integer(),
        INECAC_DESCRIPTION = readr::col_character()
      ), progress = FALSE
    ) %>%
      dplyr::mutate(INECAC_VAR = inecac)
  }

  inecac05 <- read_economic_activity("INECAC05")
  inecacr <- read_economic_activity("INECACR")

  economic_activity_coding <- dplyr::bind_rows(inecac05, inecacr) |>
    dplyr::rename(INECAC05 = .data$INECAC) |>
    data.table::setDT()

lfs[, INECAC_VAR := data.table::fcase(
      QUARTER <= "2005 Q1", "INECACR",
      QUARTER > "2005 Q1", "INECAC05"
    )]

lfs[economic_activity_coding,
    on = c("INECAC_VAR", "INECAC05"),
    INECAC_DESCRIPTION := i.INECAC_DESCRIPTION
    ][, `:=`(INECAC_VAR = NULL, INECAC05 = NULL)
      ][, data.table::setnames(.SD, "INECAC_DESCRIPTION", "INECAC05")
    ][, INECAC05 := forcats::as_factor(INECAC05)]

}
