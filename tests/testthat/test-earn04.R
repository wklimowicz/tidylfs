skip_on_ci()



test_that("EARN04 matches raw data", {

  # From Raw LFS Data --------------------

  earn04 <- readRDS("data/test-e1.Rds")

  earn04 <- earn04 %>%
    dplyr::filter(FTPTWK == "Full-time") %>%
    lfs_summarise_salary(QUARTER) %>%
    dplyr::select(QUARTER, mean_weekly_pay, median_weekly_pay)
  # dplyr::mutate(QUARTER = "Apr-Jun 2021") %>%
  # tidyr::pivot_wider(
  #   id_cols = QUARTER,
  #   values_from = "mean_weekly_pay"
  # )


  # From ONS EARN04 Publication --------------------

  withr::local_file("earn04.xls", {
    url_earn04 <- "https://www.ons.gov.uk/employmentandlabourmarket/peopleinwork/earningsandworkinghours/datasets/grossweeklyearningsoffulltimeemployeesearn04"
    html_webpage <- httr::GET(url_earn04)
    html_webpage <- rawToChar(html_webpage$content)
    download_link <- stringr::str_extract(html_webpage, "(https.*\\.xls)")


    if (Sys.info()["sysname"] == "Windows") {
      download.file(download_link, "earn04.xls", mode = "wb", quiet = TRUE)
    } else {
      download.file(download_link, "earn04.xls", quiet = TRUE)
    }
  })


  earn04_ons <- readxl::read_xls(
    path = "earn04.xls",
    sheet = "People",
    .name_repair = "minimal"
  )

  earn04_ons[5, 1] <- "QUARTER"

  earn04_ons <- earn04_ons %>%
    janitor::clean_names() %>%
    dplyr::select(1, 3, 4) %>%
    janitor::row_to_names(5)

  earn04_ons <- earn04_ons %>%
    dplyr::mutate(YEAR = substr(QUARTER, 9, 12)) %>%
    dplyr::mutate(QUARTER = dplyr::case_when(
      substr(QUARTER, 1, 7) == "Jan-Mar" ~ "Q1",
      substr(QUARTER, 1, 7) == "Apr-Jun" ~ "Q2",
      substr(QUARTER, 1, 7) == "Jul-Sep" ~ "Q3",
      substr(QUARTER, 1, 7) == "Oct-Dec" ~ "Q4"
    )) %>%
    dplyr::mutate(QUARTER = paste(YEAR, QUARTER)) %>%
    dplyr::filter(QUARTER %in% unique(earn04$QUARTER)) %>%
    dplyr::select(-YEAR) %>%
    dplyr::filter(dplyr::if_all(2:3, ~ .x != "..")) %>%
    dplyr::mutate(dplyr::across(2:3, as.numeric)) %>%
    dplyr::filter(dplyr::if_all(2:3, ~ !is.na(.x))) %>%
    data.table::as.data.table()

  # Take only rows that ONS Published as well
  earn04 <- earn04 %>%
    dplyr::filter(QUARTER %in% unique(earn04_ons$QUARTER))

  earn04_ons <- stats::setNames(earn04_ons, names(earn04))


  expect_equal(earn04, earn04_ons, tolerance = 1e-3)
  expect_gt(nrow(earn04), 1)
})
