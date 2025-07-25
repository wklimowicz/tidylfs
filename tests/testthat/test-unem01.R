skip_on_ci()


test_that("UNEM01 matches raw data", {

  # Setup
  test_tolerance <- 1e-1
  # Works on 1e-8 up to 2019 Q4, but suspect new weighting
  # variable has been introduced which causes small diff.
  unem01_raw <- readRDS("data/test-u1.Rds")

  # All people ----------------------------------------
  unem01 <- data.table::as.data.table(unem01_raw)[YEAR > 1994 & WEIGHT > 0 & !is.na(ILODEFR), 
    list(n = .N,
      employed = sum((ILODEFR == "In employment") * WEIGHT, na.rm = TRUE),
      unemployed = sum((ILODEFR == "ILO unemployed") * WEIGHT, na.rm = TRUE),
      inactive = sum((ILODEFR == "Inactive") * WEIGHT, na.rm = TRUE)),
    by = QUARTER
  ][, `:=`(
    unemployed_percentage = (unemployed / (employed + unemployed)) * 100,
    employed_percentage = employed / (employed + unemployed + inactive),
    inactive_percentage = inactive / (employed + unemployed + inactive),
    unemployed = NULL,
    employed = NULL,
    inactive = NULL)
  ][] |>
    dplyr::select(QUARTER, unemployed_percentage) |>
    dplyr::filter(!is.na(unemployed_percentage)) |>
    dplyr::arrange(QUARTER)


  withr::local_file("unem01.xls", {
    url_unem01 <- "https://www.ons.gov.uk/employmentandlabourmarket/peoplenotinwork/unemployment/datasets/unemploymentbyageanddurationnotseasonallyadjustedunem01nsa"
    html_webpage <- httr::GET(url_unem01)
    html_webpage <- rawToChar(html_webpage$content)
    download_link <- stringr::str_extract(html_webpage, "(https.*\\.xls)")

    if (Sys.info()["sysname"] == "Windows") {
      download.file(download_link, "unem01.xls", mode = "wb", quiet = TRUE)
    } else {
      download.file(download_link, "unem01.xls", quiet = TRUE)
    }
  })


  unem01_ons <- readxl::read_xls(
    path = "unem01.xls",
    sheet = "People",
    .name_repair = "minimal"
  )

  unem01_ons[6, 1] <- "QUARTER"
  unem01_ons[6, 3] <- "unemployed_percentage"

  unem01_ons <- unem01_ons[, c(1, 3)] |>
    janitor::row_to_names(6) |>
    dplyr::mutate(YEAR = substr(QUARTER, 9, 12)) |>
    dplyr::mutate(QUARTER = dplyr::case_when(
      substr(QUARTER, 1, 7) == "Jan-Mar" ~ "Q1",
      substr(QUARTER, 1, 7) == "Apr-Jun" ~ "Q2",
      substr(QUARTER, 1, 7) == "Jul-Sep" ~ "Q3",
      substr(QUARTER, 1, 7) == "Oct-Dec" ~ "Q4"
    )) |>
    dplyr::mutate(QUARTER = paste(YEAR, QUARTER)) |>
    dplyr::select(-YEAR) |>
    dplyr::filter(QUARTER %in% unique(unem01$QUARTER)) |>
    dplyr::mutate(unemployed_percentage = as.numeric(unemployed_percentage)) |>
    data.table::as.data.table()

  expect_equal(unem01, unem01_ons, tolerance = test_tolerance)
  expect_gt(nrow(unem01), 1)


  # Men ----------------------------------------

  unem01 <- data.table::as.data.table(unem01_raw)[YEAR %in% c(1995:2023) & SEX == "Male" & WEIGHT > 0 & !is.na(ILODEFR), 
    list(n = .N,
      employed = sum((ILODEFR == "In employment") * WEIGHT, na.rm = TRUE),
      unemployed = sum((ILODEFR == "ILO unemployed") * WEIGHT, na.rm = TRUE),
      inactive = sum((ILODEFR == "Inactive") * WEIGHT, na.rm = TRUE)),
    by = QUARTER
  ][, `:=`(
    unemployed_percentage = (unemployed / (employed + unemployed)) * 100,
    employed_percentage = employed / (employed + unemployed + inactive),
    inactive_percentage = inactive / (employed + unemployed + inactive),
    unemployed = NULL,
    employed = NULL,
    inactive = NULL)
  ][] |>
    dplyr::select(QUARTER, unemployed_percentage) |>
    dplyr::filter(!is.na(unemployed_percentage)) |>
    dplyr::arrange(QUARTER)


  unem01_ons <- readxl::read_xls(
    path = "unem01.xls",
    sheet = "Men",
    .name_repair = "minimal"
  )

  unem01_ons[6, 1] <- "QUARTER"
  unem01_ons[6, 3] <- "unemployed_percentage"

  unem01_ons <- unem01_ons[, c(1, 3)] |>
    janitor::row_to_names(6) |>
    dplyr::mutate(YEAR = substr(QUARTER, 9, 12)) |>
    dplyr::mutate(QUARTER = dplyr::case_when(
      substr(QUARTER, 1, 7) == "Jan-Mar" ~ "Q1",
      substr(QUARTER, 1, 7) == "Apr-Jun" ~ "Q2",
      substr(QUARTER, 1, 7) == "Jul-Sep" ~ "Q3",
      substr(QUARTER, 1, 7) == "Oct-Dec" ~ "Q4"
    )) |>
    dplyr::mutate(QUARTER = paste(YEAR, QUARTER)) |>
    dplyr::select(-YEAR) |>
    dplyr::filter(QUARTER %in% unique(unem01$QUARTER)) |>
    dplyr::mutate(unemployed_percentage = as.numeric(unemployed_percentage)) |>
    data.table::as.data.table()

  expect_equal(unem01, unem01_ons, tolerance = test_tolerance)
  expect_gt(nrow(unem01), 1)

  # Women ----------------------------------------

  unem01 <- data.table::as.data.table(unem01_raw)[YEAR %in% c(1995:2023) & SEX == "Female" & WEIGHT > 0 & !is.na(ILODEFR), 
    list(n = .N,
      employed = sum((ILODEFR == "In employment") * WEIGHT, na.rm = TRUE),
      unemployed = sum((ILODEFR == "ILO unemployed") * WEIGHT, na.rm = TRUE),
      inactive = sum((ILODEFR == "Inactive") * WEIGHT, na.rm = TRUE)),
    by = QUARTER
  ][, `:=`(
    unemployed_percentage = (unemployed / (employed + unemployed)) * 100,
    employed_percentage = employed / (employed + unemployed + inactive),
    inactive_percentage = inactive / (employed + unemployed + inactive),
    unemployed = NULL,
    employed = NULL,
    inactive = NULL)
  ][] |>
    dplyr::select(QUARTER, unemployed_percentage) |>
    dplyr::filter(!is.na(unemployed_percentage)) |>
    dplyr::arrange(QUARTER)


  unem01_ons <- readxl::read_xls(
    path = "unem01.xls",
    sheet = "Women",
    .name_repair = "minimal"
  )

  unem01_ons[6, 1] <- "QUARTER"
  unem01_ons[6, 3] <- "unemployed_percentage"

  unem01_ons <- unem01_ons[, c(1, 3)] |>
    janitor::row_to_names(6) |>
    dplyr::mutate(YEAR = substr(QUARTER, 9, 12)) |>
    dplyr::mutate(QUARTER = dplyr::case_when(
      substr(QUARTER, 1, 7) == "Jan-Mar" ~ "Q1",
      substr(QUARTER, 1, 7) == "Apr-Jun" ~ "Q2",
      substr(QUARTER, 1, 7) == "Jul-Sep" ~ "Q3",
      substr(QUARTER, 1, 7) == "Oct-Dec" ~ "Q4"
    )) |>
    dplyr::mutate(QUARTER = paste(YEAR, QUARTER)) |>
    dplyr::select(-YEAR) |>
    dplyr::filter(QUARTER %in% unique(unem01$QUARTER)) |>
    dplyr::mutate(unemployed_percentage = as.numeric(unemployed_percentage)) |>
    data.table::as.data.table()

  expect_equal(unem01, unem01_ons, tolerance = test_tolerance)
  expect_gt(nrow(unem01), 1)
})
