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
