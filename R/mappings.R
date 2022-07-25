#' Picks column names in order of appearance
#'
#' When column names change over time, picks the first
#' one that appears from the list in order of appearance.
#' If it doesn't appear, returns a character NA.
#'
#' @param vector_of_names character vector of column names
#' @param column_names vector of columns from lfs
#'
#' @return First name that appears in cols
#'
#' @export
lfs_pick_column <- function(vector_of_names, column_names) {
  index <- vector_of_names %in% column_names

  if (sum(index) > 0) {
    vector_of_names[which.max(index == T)]
  } else {
    NA_character_
  }
}

lfs_default_mappings <- function(cols) {

  # Choose variables in order of priority when found

    caseno <- lfs_pick_column(c("CASENO", "CASE"), cols)

  # Core - INECACR is older, but replicates ONS numbers more precisely in 2000's
  # - see variales_report
  ilo_status <- lfs_pick_column(c("INECACR", "INECAC05"), cols)

  weight_income <- lfs_pick_column(
    c(
      "PIWT20",
      "PIWT18",
      "PIWT14",
      # "PWT07", 
      # The ONS doesn't include an Income weighting in 2001 Q1. 
      # Replacing it with the non-income weight causes a bug because of duplicate
      # variable names in this script.
      # The ONS omits 2001 Q1 in official pay publications (eg. EARN07)
      "PIWT07"
    ),
    cols
  )

  weight <- lfs_pick_column(c("PWT20", "PWT18", "PWT14", "PWT07"), cols)

  # Education
  cured <- lfs_pick_column(c("CURED8", "CURED"), cols)

  hiquald <- lfs_pick_column(c(
    "HIQUL15D", "HIQUL11D", "HIQUAL8D",
    "HIQUAL5D", "HIQUAL4D", "HIQUALD"
  ), cols)

  hiqual <- lfs_pick_column(c(
    "HIQUAL15", "HIQUAL11", "HIQUAL8",
    "HIQUAL5", "HIQUAL4", "HIQUAL"
  ), cols)

  degree71 <- lfs_pick_column(c("DEGREE71", "DEGREE4", "DEGREE"), cols)

  fdsico <- lfs_pick_column(c("FDSICO", "FDSINCOM", "SINCOM"), cols)

  degree_subject <- lfs_pick_column(c("FDSNGDEG", "SNGDEG"), cols)

  combined_degree_subject <- lfs_pick_column(c(
    "FDCMBMA", "UNCOMBMA",
    "CMBDEG01"
  ), cols)

  teaching_qualification <- lfs_pick_column(c("TEACH41", "TEACH1"), cols)

  # Occupation
  occupation <- lfs_pick_column(c("SOC20M", "SOC10M", "SOC2KM"), cols)

  occupation_major <- lfs_pick_column(c(
    "SC2010MMJ", "SC10MMJ",
    "SC2KMMJ"
  ), cols)

  last_occupation <- lfs_pick_column(c("SOC20O", "SOC10O", "SOC2KO"), cols)

  parental_occupation <- lfs_pick_column(c("SMSOC204", "SMSOC104"), cols)

  parental_occupation_major <- lfs_pick_column(c("SMSOC201", "SMSOC101"), cols)

  # Other
  industry_major <- lfs_pick_column(c("INDS07M", "INDS92M", "SICMAIN"), cols)

  industry <- lfs_pick_column(c("ICDM", "INDM92M"), cols)

  # "INDS92M" %in% cols ~ "INDS92M",INDS92M
  ethnicity <- lfs_pick_column(c("ETHUKEUL", "ETH01"), cols)

  health <- lfs_pick_column(c("HEALTH20", "HEALTH"), cols)

  lnglst <- lfs_pick_column(c("LNGLST", "LNGLIM"), cols)


  variables <- tibble::tribble(
    ~lfs_name, ~new_name, ~type,
    # ID Variables --------------------
    caseno, "CASENO", "character",
    "THISWV", "THISWV", "numeric",
    "SEX", "SEX", "factor",
    "COUNTRY", "COUNTRY", "factor",
    "GOVTOR", "GOVTOR", "factor",
    "AGE", "AGE", "numeric",
    "FTPTWK", "FTPTWK", "factor",
    "GRSSWK", "GRSSWK", "numeric",
    "HOURPAY", "HOURPAY", "numeric",
    "EDAGE", "EDAGE", "numeric",
    "DISEA", "DISABILITY", "factor",
    "GOVTOF", "GOVTOF", "factor",
    "LAUA", "LAUA", "unlabelled_factor",
    "PCON9D", "PCON9D", "unlabelled_factor",
    "MSOA11", "MSOA11", "unlabelled_factor",
    "WARD", "WARD", "unlabelled_factor",
    "AGES", "AGES", "factor",
    "PUBLICR", "PUBLIC", "factor",
    # Hours ----------------------
    "BUSHR", "BUSHR", "numeric",
    # "TTUSHR", "TTUSHR", "numeric",
    # "BACTHR", "BACTHR", "numeric",
    "TTACHR", "TTACHR", "numeric",
    # "TOTUS1", "TOTUS1", "numeric",
    # "TOTAC1", "TOTAC1", "numeric",
    "ACTHR", "ACTHR", "numeric",
    # "TOTAC2", "TOTAC2", "numeric",
    # "SUMHRS", "SUMHRS", "numeric",
    # "TOTHRS", "TOTHRS", "numeric",
    # Union ----------------------------------------
    "UNION", "UNION", "factor",
    "TUPRES", "TUPRES", "factor",
    "TUCOV", "TUCOV", "factor",
    # Life Satisfaction - only in APS ---------------
    "SATIS", "SATIS", "numeric",
    "WORTH", "WORTH", "numeric",
    "HAPPY", "HAPPY", "numeric",
    "ANXIOUS", "ANXIOUS", "numeric",
    # Variable changes over time ----------
    degree_subject, "DEGREE_SUBJECT", "factor",
    combined_degree_subject, "CMBDEGREE", "factor",
    "ILODEFR", "ILODEFR", "factor",
    "OYCIRC", "OYCIRC", "factor",
    occupation, "OCCUPATION", "numeric",
    occupation_major, "OCCUPATION_MAJOR", "numeric",
    last_occupation, "LAST_OCCUPATION", "numeric",
    parental_occupation, "PARENTAL_OCCUPATION", "numeric",
    parental_occupation_major, "PARENTAL_OCCUPATION_MAJOR", "numeric",
    weight_income, "WEIGHT_INCOME", "numeric",
    weight, "WEIGHT", "numeric",
    ilo_status, "INECAC05", "numeric",
    health, "HEALTH", "factor",
    lnglst, "LNGLST", "factor",
    fdsico, "FDSICO", "factor",
    industry_major, "INDUSTRY_MAJOR", "factor",
    industry, "INDUSTRY", "character",
    ethnicity, "ETHNICITY", "factor",
    # Education -------------------------
    cured, "CURED", "factor",
    hiquald, "HIQUALD", "numeric",
    hiqual, "HIQUAL", "factor",
    degree71, "DEGREE71", "numeric",
    "HIGHO", "HIGHO", "factor",
    "DEGREE72", "DEGREE72", "numeric",
    "DEGREE73", "DEGREE73", "numeric",
    "DEGREE74", "DEGREE74", "numeric",
    "DEGREE75", "DEGREE75", "numeric",
    teaching_qualification, "TEACH1", "factor"
  )

  # Exclude missing
  variables <- variables %>%
    dplyr::mutate(lfs_name = ifelse(.data$lfs_name %in% cols,
      .data$lfs_name,
      NA
    ))


  return(variables)
}
