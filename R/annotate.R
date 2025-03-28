create_HSERIAL <- function(lfs) {

# Pad with 0's appropriately
withr::with_options(
c(scipen = 999), #So you always get 10 and not 1e1
{
lfs[, QUOTA := stringr::str_pad(QUOTA,
                                3,
                                pad = "0")]
lfs[, WEEK := stringr::str_pad(WEEK,
                                2,
                                pad = "0")]
lfs[, ADD := stringr::str_pad(ADD,
                                2,
                                pad = "0")]
lfs[, HHLD := stringr::str_pad(HHLD,
                                2,
                                pad = "0")]
lfs[, PERSNO := stringr::str_pad(PERSNO,
                                2,
                                pad = "0")]
}
)

lfs[, HSERIAL := paste0(
    QUOTA,
    WEEK,
    W1YR,
    QRTR,
    ADD,
    WAVFND,
    HHLD
    )]

lfs[, CASENO := paste0(HSERIAL, PERSNO)]

# lfs[, HSERIAL := as.factor(HSERIAL)]

lfs[,
    `:=`(
    QUOTA = NULL,
    WEEK = NULL,
    W1YR = NULL,
    QRTR = NULL,
    ADD = NULL,
    WAVFND = NULL,
    HHLD = NULL,
    PERSNO = NULL
    )
    ]


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

    if (!("HIQUALD" %in% colnames(lfs))) {
      return(lfs)
    }

    lfs[, HIQUALD := data.table::fcase(
      HIQUALD == 1, "Degree or equivalent",
      HIQUALD == 2, "Higher education",
      HIQUALD == 3, "GCE, A-level or equivalent",
      HIQUALD == 4, "GCSE grades A*-C or equivalent",
      HIQUALD == 5, "Other qualifications",
      HIQUALD == 6, "No qualification",
      HIQUALD == 7, "Don't know"
      )][, HIQUALD := forcats::as_factor(HIQUALD)
       ]

}

annotate_degree7 <- function(lfs) {

    if (!("DEGREE7" %in% colnames(lfs))) {
      return(lfs)
    }

    names_vector <- lfs[, names(.SD), .SDcols = patterns("DEGREE7")]

    lfs[, (names_vector) := lapply(.SD, annotate_degree71, YEAR), .SDcols = patterns("DEGREE7")
         ][, (names_vector) := lapply(.SD, forcats::as_factor), .SDcols = patterns("DEGREE7")]

    return(lfs)

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

  socmain <- read_occupation_coding("SOCMAIN")
  soc2km <- read_occupation_coding("SOC2KM")
  soc10m <- read_occupation_coding("SOC10M")
  soc20m <- read_occupation_coding("SOC20M")

  list_of_socs <- list(socmain, soc2km, soc10m, soc20m)

  soc_coding <- dplyr::bind_rows(list_of_socs) %>%
    dplyr::rename(OCCUPATION = "SOC")|>
    data.table::setDT()

    # dplyr::mutate(OCCUPATION_DESCRIPTION = forcats::as_factor(OCCUPATION_DESCRIPTION))

  soc_coding_last <- dplyr::bind_rows(list_of_socs) %>%
    dplyr::rename(
      LAST_OCCUPATION = "SOC",
      LAST_OCCUPATION_DESCRIPTION = "OCCUPATION_DESCRIPTION"
    ) |>
    data.table::setDT()

    # dplyr::mutate(LAST_OCCUPATION_DESCRIPTION = forcats::as_factor(LAST_OCCUPATION_DESCRIPTION))

  soc_coding_parental <- dplyr::bind_rows(list_of_socs) %>%
    dplyr::rename(
      PARENTAL_OCCUPATION = "SOC",
      PARENTAL_OCCUPATION_DESCRIPTION = "OCCUPATION_DESCRIPTION"
    )  |>
    data.table::setDT()
    # dplyr::mutate(PARENTAL_OCCUPATION_DESCRIPTION = forcats::as_factor(PARENTAL_OCCUPATION_DESCRIPTION))



  lfs <- lfs[, SOC_TYPE := data.table::fcase(
      YEAR >= 1992 & YEAR < 2001, "SOCMAIN",
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

  if (!any(c("INDUSTRY") %in% colnames(lfs))) {
    return(lfs)
  }

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
      dplyr::select(-"Industry_Code", -"LFS_Description") %>%
      dplyr::rename(INDUSTRY_DESCRIPTION = "ONS_Description")

  sic_coding <- dplyr::bind_rows(sic07, sic92) %>%
    dplyr::rename(INDUSTRY = "SIC") |>
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


annotate_economic_activity <- function(lfs, aps = FALSE) {

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
    dplyr::rename(INECAC05 = "INECAC") |>
    data.table::setDT()

  if (aps == TRUE) {
    lfs[, INECAC_VAR := data.table::fcase(
          YEAR <= 2005, "INECACR",
          YEAR > 2005, "INECAC05"
        )]
  } else {
    lfs[, INECAC_VAR := data.table::fcase(
          QUARTER <= "2005 Q1", "INECACR",
          QUARTER > "2005 Q1", "INECAC05"
        )]
  }

lfs[economic_activity_coding,
    on = c("INECAC_VAR", "INECAC05"),
    INECAC_DESCRIPTION := i.INECAC_DESCRIPTION
    ][, `:=`(INECAC_VAR = NULL, INECAC05 = NULL)
      ][, data.table::setnames(.SD, "INECAC_DESCRIPTION", "INECAC05")
    ][, INECAC05 := forcats::as_factor(INECAC05)]

}


annotate_subject <- function(lfs) {

  # Trim WS because 1990's files have leading space
  lfs[, DEGREE_SUBJECT := trimws(DEGREE_SUBJECT)]

  # Find correct coding file
  read_subject_coding <- function(year) {
    readr::read_csv(
      paste0(system.file("coding_frames", package = "tidylfs"), "/sngdeg_", year, "_2_digit.csv"),
      col_types = readr::cols(
        DEGREE = readr::col_character(),
        DEGREE_DESCRIPTION = readr::col_character()
      ), progress = FALSE
    ) %>%
      dplyr::mutate(SUBJECT_VAR = year)
  }

  subject_1992 <- read_subject_coding("1992")
  subject_1997 <- read_subject_coding("1997")
  subject_2004 <- read_subject_coding("2004")

  subject_coding <- dplyr::bind_rows(subject_1992, subject_1997, subject_2004) |>
    data.table::setDT()

lfs[, SUBJECT_VAR := data.table::fcase(
      QUARTER < "1997 Q3", "1992",
      QUARTER <= "2003 Q4", "1997",
      QUARTER >= "2004 Q1", "2004"
    )]


lfs[, two_digit_code := stringr::str_extract(DEGREE_SUBJECT, stringr::regex("(\\d{1,2}\\.\\d{1,2}|\\d{1,2})"))]

lfs[subject_coding,
    on = c("SUBJECT_VAR", "two_digit_code" = "DEGREE"),
    DEGREE_DESCRIPTION := i.DEGREE_DESCRIPTION
    ][, `:=`(SUBJECT_VAR = NULL, two_digit_code = NULL)
      ][, DEGREE_DESCRIPTION := forcats::as_factor(DEGREE_DESCRIPTION)
      ][, DEGREE_SUBJECT := forcats::as_factor(DEGREE_SUBJECT)]



}

