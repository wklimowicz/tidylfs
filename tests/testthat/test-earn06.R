skip_on_ci()



test_that("EARN06 matches raw data", {

  # From Raw LFS Data --------------------

  earn06 <- readRDS("data/test-e1.Rds")

  earn06 <- earn06 %>%
    dplyr::filter(FTPTWK == "Full-time") %>%
    dplyr::filter(!is.na(OCCUPATION_MAJOR)) %>%
    lfs_summarise_salary(QUARTER, OCCUPATION_MAJOR) %>%
    # dplyr::mutate(QUARTER = "Apr-Jun 2021") %>%
    tidyr::pivot_wider(
      id_cols = QUARTER,
      names_from = OCCUPATION_MAJOR,
      values_from = "mean_weekly_pay"
    )


  # From ONS EARN06 Publication --------------------

  withr::local_file(list("earn06.xls"), {
    url_earn06 <- "https://www.ons.gov.uk/employmentandlabourmarket/peopleinwork/earningsandworkinghours/datasets/grossweeklyearningsbyoccupationearn06"
    html_webpage <- httr::GET(url_earn06)
    html_webpage <- rawToChar(html_webpage$content)
    download_link <- stringr::str_extract(html_webpage, "(https.*\\.xls)")

    if (Sys.info()["sysname"] == "Windows") {
      download.file(download_link, "earn06.xls", mode = "wb", quiet = TRUE)
    } else {
      download.file(download_link, "earn06.xls", quiet = TRUE)
    }
  })


  earn06_ons <- readxl::read_xls(
    path = "earn06.xls",
    sheet = "Weekly SOC2010",
    .name_repair = "minimal"
  )

  earn06_ons[5, 1] <- "QUARTER"

  earn06_ons <- earn06_ons %>%
    janitor::row_to_names(5)

  earn06_ons <- earn06_ons %>%
    dplyr::mutate(YEAR = substr(QUARTER, 9, 12)) %>%
    dplyr::mutate(QUARTER = dplyr::case_when(
      substr(QUARTER, 1, 7) == "Jan-Mar" ~ "Q1",
      substr(QUARTER, 1, 7) == "Apr-Jun" ~ "Q2",
      substr(QUARTER, 1, 7) == "Jul-Sep" ~ "Q3",
      substr(QUARTER, 1, 7) == "Oct-Dec" ~ "Q4"
    )) %>%
    dplyr::mutate(QUARTER = paste(YEAR, QUARTER)) %>%
    dplyr::filter(QUARTER %in% unique(earn06$QUARTER)) %>%
    dplyr::filter(QUARTER != "2001 Q2") %>% # ONS didn't publish this quarter
    dplyr::mutate(dplyr::across(2:11, as.numeric)) %>%
    dplyr::select(-2, -YEAR)

  # Take only rows that ONS Published as well
  earn06 <- earn06 %>%
    dplyr::filter(QUARTER %in% unique(earn06_ons$QUARTER))

  earn06_ons <- stats::setNames(earn06_ons, c("QUARTER", 1:9))


  expect_equal(earn06, earn06_ons, tolerance = 1e-4)
  expect_gt(nrow(earn06), 1)
})
