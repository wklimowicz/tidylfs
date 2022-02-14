lfs_default_mappings <- function(cols) {

  # Choose variables in order of priority when found

  deg_var <- dplyr::case_when(
    "FDSNGDEG" %in% cols ~ "FDSNGDEG",
    "SNGDEG" %in% cols ~ "SNGDEG",
  )

  deg_var2 <- dplyr::case_when(
    "FDCMBMA" %in% cols ~ "FDCMBMA",
    "UNCOMBMA" %in% cols ~ "UNCOMBMA",
    "CMBDEG01" %in% cols ~ "CMBDEG01",
  )

  deg_var3 <- dplyr::case_when(
    "CMBDEG" %in% cols ~ "CMBDEG",
    "COMDEG" %in% cols ~ "COMDEG",
    "CMBDEG01" %in% cols ~ "CMBDEG01",
    "CMBDEGN01" %in% cols ~ "CMBDEGN01",
    "COMDEG01" %in% cols ~ "COMDEG01",
  )

  ilo_var <- dplyr::case_when(
    "ILODEFA" %in% cols ~ "ILODEFA", # For 1992
    "ILODEFR" %in% cols ~ "ILODEFR",
  )


  weight_income <- dplyr::case_when(
    "PIWT20" %in% cols ~ "PIWT20",
    "PIWT18" %in% cols ~ "PIWT18",
    "PIWT17" %in% cols ~ "PIWT17",
    "PIWT14" %in% cols ~ "PIWT14",
    "PIWT07" %in% cols ~ "PIWT07",
    "PWT07" %in% cols ~ "PWT07_MI",
    "NEWIWT" %in% cols ~ "NEWIWT",
    "WEIGHT2" %in% cols ~ "WEIGHT2",
  )

  weight <- dplyr::case_when(
    "PWT20" %in% cols ~ "PWT20",
    "PWT18" %in% cols ~ "PWT18",
    "PWT17" %in% cols ~ "PWT17",
    "PWT14" %in% cols ~ "PWT14",
    "PWT07" %in% cols ~ "PWT07",
    "NEWWT" %in% cols ~ "NEWWT",
    "WEIGHT1" %in% cols ~ "WEIGHT1",
  )

  ilo_status <- dplyr::case_when(
    "INECACA" %in% cols ~ "INECACA",
    "INECACR" %in% cols ~ "INECACR",
    "INECAC05" %in% cols ~ "INECAC05",
  )

  cured_var <- dplyr::case_when(
    "CURED8" %in% cols ~ "CURED8",
    "CURED" %in% cols ~ "CURED",
  )


  hiquld_var <- dplyr::case_when(
    "HIQUL15D" %in% cols ~ "HIQUL15D",
    "HIQUL11D" %in% cols ~ "HIQUL11D",
    "HIQUAL8D" %in% cols ~ "HIQUAL8D",
    "HIQUAL5D" %in% cols ~ "HIQUAL5D",
    "HIQUAL4D" %in% cols ~ "HIQUAL4D",
    "HIQUALD" %in% cols ~ "HIQUALD",
  )

  hiqul_var <- dplyr::case_when(
    "HIQUAL15" %in% cols ~ "HIQUAL15",
    "HIQUAL11" %in% cols ~ "HIQUAL11",
    "HIQUAL8" %in% cols ~ "HIQUAL8",
    "HIQUAL5" %in% cols ~ "HIQUAL5",
    "HIQUAL4" %in% cols ~ "HIQUAL4",
    "HIQUAL" %in% cols ~ "HIQUAL",
  )

  DEGREE71 <- dplyr::case_when(
    "DEGREE71" %in% cols ~ "DEGREE71",
    "DEGREE4" %in% cols ~ "DEGREE4",
    "DEGREE" %in% cols ~ "DEGREE",
  )

  DEGREE72 <- dplyr::case_when(
    "DEGREE72" %in% cols ~ "DEGREE72",
  )


  DEGREE73 <- dplyr::case_when(
    "DEGREE73" %in% cols ~ "DEGREE73",
  )

  DEGREE74 <- dplyr::case_when(
    "DEGREE74" %in% cols ~ "DEGREE74",
  )


  DEGREE75 <- dplyr::case_when(
    "DEGREE75" %in% cols ~ "DEGREE75",
  )


  fdsico_var <- dplyr::case_when(
    "FDSICO" %in% cols ~ "FDSICO",
    "FDSINCOM" %in% cols ~ "FDSINCOM",
    "SINCOM" %in% cols ~ "SINCOM",
  )

  #  qual1_var <- dplyr::case_when(
  #    "QUAL_1" %in% cols ~ "QUAL_1",
  #    "QUALCH1" %in% cols ~ "QUALCH1",
  #    )
  #

  occupation <- dplyr::case_when(
    "SOC10M" %in% cols ~ "SOC10M",
    "SOC2KM" %in% cols ~ "SOC2KM",
    "SOC20M" %in% cols ~ "SOC20M",
  )

  occupation_major <- dplyr::case_when(
    "SC2KMMJ" %in% cols ~ "SC2KMMJ",
    "SC10MMJ" %in% cols ~ "SC10MMJ",
    "SC2010MMJ" %in% cols ~ "SC2010MMJ",
    "SC20MMJ" %in% cols ~ "SC20MMJ",
  )

  parental_occupation <- dplyr::case_when(
    "SMSOC20" %in% cols ~ "SMSOC20",
    "SMSOC10" %in% cols ~ "SMSOC10",
    "SMSOC2K" %in% cols ~ "SMSOC2K",
  )

  parental_occupation_major <- dplyr::case_when(
    "SMSOC201" %in% cols ~ "SMSOC201",
    "SMSOC101" %in% cols ~ "SMSOC101",
    "SMSOC2k1" %in% cols ~ "SMSOC2k1",
  )



  industry_var <- dplyr::case_when(
    "INDS07M" %in% cols ~ "INDS07M",
    "IN9207SM" %in% cols ~ "IN0792SM",
    # "INDS92M" %in% cols ~ "INDS92M",
  )

  ethnicity_var <- dplyr::case_when(
    "ETH01" %in% cols ~ "ETH01",
    "ETH11" %in% cols ~ "ETH11",
    "ETH11EW" %in% cols ~ "ETH11EW",
  )


  longitudinal_variables <- tibble::tribble(
    ~lfs_name, ~new_name, ~type,
    "QUOTA", "QUOTA", "numeric",
    "WEEK", "WEEK", "numeric",
    "W1YR", "W1YR", "numeric",
    "QRTR", "QRTR", "numeric",
    "ADD", "ADD", "numeric",
    "WAVFND", "WAVFND", "numeric",
    "HHLD", "HHLD", "numeric",
    "RECNO", "RECNO", "numeric"
  )


  variables <- tibble::tribble(
    ~lfs_name, ~new_name, ~type,
    # ID Variables --------------------
    "SEX", "SEX", "factor",
    "COUNTRY", "COUNTRY", "factor",
    "GOVTOR", "GOVTOR", "factor",
    "AGE", "AGE", "numeric",
    "FTPTWK", "FTPTWK", "factor",
    # "FTPT", "FTPT", "numeric",
    "GRSSWK", "GRSSWK", "numeric",
    "HOURPAY", "HOURPAY", "numeric",
    "EDAGE", "EDAGE", "numeric",
    "DISEA", "DISABILITY", "factor",
    "GOVTOF", "GOVTOF", "factor",
    "AGES", "AGES", "factor",
    "PUBLICR", "PUBLIC", "factor",
    # Hours ----------------------
    "BUSHR", "BUSHR", "numeric",
    "TTUSHR", "TTUSHR", "numeric",
    "BACTHR", "BACTHR", "numeric",
    "TTACHR", "TTACHR", "numeric",
    "TOTUS1", "TOTUS1", "numeric",
    "TOTAC1", "TOTAC1", "numeric",
    "ACTHR", "ACTHR", "numeric",
    "TOTAC2", "TOTAC2", "numeric",
    "SUMHRS", "SUMHRS", "numeric",
    "TOTHRS", "TOTHRS", "numeric",
    # Union ----------------------------------------
    "UNION", "UNION", "factor",
    "TUPRES", "TUPRES", "factor",
    "TUCOV", "TUCOV", "factor",
    # Variable changes over time ----------
    deg_var, "DEGREE", "factor",
    deg_var2, "CMBDEGREE", "factor",
    ilo_var, "ILODEFR", "factor",
    occupation, "OCCUPATION", "factor",
    occupation_major, "OCCUPATION_MAJOR", "numeric",
    parental_occupation, "PARENTAL_OCCUPATION", "factor",
    parental_occupation_major, "PARENTAL_OCCUPATION_MAJOR", "factor",
    weight_income, "WEIGHT_INCOME", "numeric",
    weight, "WEIGHT", "numeric",
    ilo_status, "INECAC05", "numeric",
    fdsico_var, "FDSICO", "factor",
    industry_var, "INDUSTRY", "factor",
    ethnicity_var, "ETHNICITY", "factor",
    # Education -------------------------
    "HIGHO", "HIGHO", "factor",
    cured_var, "CURED", "factor",
    hiquld_var, "HIQUALD", "numeric",
    hiqul_var, "HIQUAL", "factor",
    DEGREE71, "DEGREE71", "numeric",
    DEGREE72, "DEGREE72", "numeric",
    DEGREE73, "DEGREE73", "numeric",
    DEGREE74, "DEGREE74", "numeric",
    DEGREE75, "DEGREE75", "numeric"
  )

  variables <- dplyr::bind_rows(variables, longitudinal_variables)

  # Exclude missing
  variables <- variables %>%
    dplyr::mutate(lfs_name = ifelse(.data$lfs_name %in% cols, .data$lfs_name, NA))


  return(variables)
}
