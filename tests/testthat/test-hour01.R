skip_on_ci()


test_that("HOUR01 matches raw data", {

  # From Raw LFS Data --------------------
  hour01_raw <- readRDS("data/test-h1.Rds")

  hour01 <- hour01_raw |>
    # dplyr::mutate(FTPT = as.numeric(FTPT)) |>
    dplyr::mutate(FT = !FTPTWK == "Full-time") |>
    dplyr::mutate(ILODEFR = as.numeric(ILODEFR)) |>
    dplyr::filter(
      ILODEFR == 1,
      WEIGHT > 0
    ) |>
    dplyr::filter(!is.na(TTACHR)) |>
    dplyr::group_by(QUARTER, FT) |>
    dplyr::summarise(
      n = dplyr::n(),
      total_hours = sum(TTACHR),
      hours = sum(TTACHR * WEIGHT, na.rm = TRUE),
      weight = sum(WEIGHT, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      hours = hours / weight
    ) |>
    dplyr::select(-weight) |>
    tidyr::pivot_wider(
      id_cols = QUARTER,
      names_from = FT,
      values_from = hours
    ) |>
    dplyr::arrange(QUARTER)

  # From ONS HOUR01 Publication --------------------
  withr::local_file(list("hour01.xls"), {
    url_hour01 <- "https://www.ons.gov.uk/employmentandlabourmarket/peopleinwork/earningsandworkinghours/datasets/actualweeklyhoursworkednotseasonallyadjustedhour01nsa"
    html_webpage <- httr::GET(url_hour01)
    html_webpage <- rawToChar(html_webpage$content)
    download_link <- stringr::str_extract(html_webpage, "(https.*\\.xls)")

    if (Sys.info()["sysname"] == "Windows") {
      download.file(download_link, "hour01.xls", mode = "wb", quiet = TRUE)
    } else {
      download.file(download_link, "hour01.xls", quiet = TRUE)
    }
  })

  hour01_ons <- readxl::read_xls(
    path = "hour01.xls",
    sheet = "People",
    .name_repair = "minimal"
  )

  hour01_ons[6, 1] <- "QUARTER"

  hour01_ons <- hour01_ons |>
    janitor::row_to_names(6) |>
    dplyr::select(1, 4:5) |>
    dplyr::slice(-c(1:2))



  hour01_ons <- hour01_ons |>
    dplyr::mutate(YEAR = substr(QUARTER, 9, 12)) |>
    dplyr::mutate(QUARTER = dplyr::case_when(
      substr(QUARTER, 1, 7) == "Jan-Mar" ~ "Q1",
      substr(QUARTER, 1, 7) == "Apr-Jun" ~ "Q2",
      substr(QUARTER, 1, 7) == "Jul-Sep" ~ "Q3",
      substr(QUARTER, 1, 7) == "Oct-Dec" ~ "Q4"
    )) |>
    dplyr::mutate(QUARTER = paste(YEAR, QUARTER)) |>
    dplyr::filter(QUARTER %in% unique(hour01$QUARTER)) |>
    # dplyr::filter(QUARTER != "2001 Q2") |> # ONS didn't publish this quarter
    dplyr::mutate(dplyr::across(2:4, as.numeric)) |>
    dplyr::select(-YEAR)

  # Take only rows that ONS Published as well
  hour01 <- hour01 |>
    dplyr::filter(QUARTER %in% unique(hour01_ons$QUARTER))

  hour01_ons <- stats::setNames(hour01_ons, names(hour01))


  expect_equal(hour01, hour01_ons, tolerance = 1e-3)
  expect_gt(nrow(hour01), 1)
})
