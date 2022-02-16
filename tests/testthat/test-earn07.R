skip_on_ci()



test_that("EARN07 matches raw data", {

  earn07_raw <- readRDS("data/test-e1.Rds")

  earn07 <- earn07_raw %>%
    dplyr::filter(YEAR > 2009) %>% # Only works for 2009 + so far
    dplyr::filter(!is.na(INDUSTRY)) %>%
    dplyr::mutate(GROUPED_INDUSTRY = dplyr::case_when(
      substr(INDUSTRY, 1, 1) == "A" ~ "A",
      substr(INDUSTRY, 1, 1) %in% c("B", "D", "E") ~ "BDE",
      substr(INDUSTRY, 1, 1) %in% "C" ~ "C",
      substr(INDUSTRY, 1, 1) %in% "F" ~ "F",
      substr(INDUSTRY, 1, 1) %in% "G" ~ "G",
      substr(INDUSTRY, 1, 1) %in% "H" ~ "H",
      substr(INDUSTRY, 1, 1) %in% "I" ~ "I",
      substr(INDUSTRY, 1, 1) %in% "J" ~ "J",
      substr(INDUSTRY, 1, 1) %in% c("K", "L") ~ "KL",
      substr(INDUSTRY, 1, 1) %in% "M" ~ "M",
      substr(INDUSTRY, 1, 1) %in% "N" ~ "N",
      substr(INDUSTRY, 1, 1) %in% "O" ~ "O",
      substr(INDUSTRY, 1, 1) %in% "P" ~ "P",
      substr(INDUSTRY, 1, 1) %in% "Q" ~ "Q",
      substr(INDUSTRY, 1, 1) %in% c("R", "S", "T") ~ "RST"
      # substr(INDUSTRY, 1, 1) %in% "U" ~ "U"
    )) %>%
    dplyr::filter(!is.na(GROUPED_INDUSTRY)) %>%
    # dplyr::mutate(FTPT = as.numeric(.data$FTPT)) %>%
    dplyr::filter(FTPTWK == "Full-time") %>%
    # dplyr::filter(QUARTER == "2005 Q1") %>%
    lfs_summarise_salary(QUARTER, GROUPED_INDUSTRY) %>%
    tidyr::pivot_wider(
      id_cols = QUARTER,
      names_from = GROUPED_INDUSTRY,
      values_from = "mean_weekly_pay"
    )


  # From ONS EARN07 Publication --------------------

  withr::local_file(list("earn07.xls"), {

    url_earn07 <- "https://www.ons.gov.uk/employmentandlabourmarket/peopleinwork/earningsandworkinghours/datasets/grossweeklyearningsbyindustryearn07"
    html_webpage <- httr::GET(url_earn07)
    html_webpage <- rawToChar(html_webpage$content)
    download_link <- stringr::str_extract(html_webpage, "(https.*\\.xls)")

    if (Sys.info()["sysname"] == "Windows") {
      download.file(download_link, "earn07.xls", mode = "wb", quiet = TRUE)
    } else {
      download.file(download_link, "earn07.xls", quiet = TRUE)
    }

  })

  earn07_ons <- readxl::read_xls(
    path = "earn07.xls",
    sheet = "People_weekly",
    .name_repair = "minimal"
  )

  earn07_ons[5, 1] <- "QUARTER"

  earn07_ons <- earn07_ons %>%
    janitor::row_to_names(5) %>%
    dplyr::select(1, 5:19) %>%
    dplyr::slice(-c(1:4))



  earn07_ons <- earn07_ons %>%
    dplyr::mutate(YEAR = substr(QUARTER, 9, 12)) %>%
    dplyr::mutate(QUARTER = dplyr::case_when(
      substr(QUARTER, 1, 7) == "Jan-Mar" ~ "Q1",
      substr(QUARTER, 1, 7) == "Apr-Jun" ~ "Q2",
      substr(QUARTER, 1, 7) == "Jul-Sep" ~ "Q3",
      substr(QUARTER, 1, 7) == "Oct-Dec" ~ "Q4"
    )) %>%
    dplyr::mutate(QUARTER = paste(YEAR, QUARTER)) %>%
    dplyr::filter(QUARTER %in% unique(earn07$QUARTER)) %>%
    # dplyr::filter(QUARTER != "2001 Q2") %>% # ONS didn't publish this quarter
    dplyr::mutate(dplyr::across(2:17, as.numeric)) %>%
    dplyr::select(-YEAR)

  # Take only rows that ONS Published as well
  earn07 <- earn07 %>%
    dplyr::filter(QUARTER %in% unique(earn07_ons$QUARTER))

  earn07_ons <- stats::setNames(earn07_ons, names(earn07))


  expect_equal(earn07, earn07_ons, tolerance = 1e-4)
  expect_gt(nrow(earn07), 1)
})
