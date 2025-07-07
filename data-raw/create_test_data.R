devtools::load_all()

con <- DBI::dbConnect(
  odbc::databricks(),
  HTTPPath = Sys.getenv("DATABRICKS_SQL_PATH"),
)

lfs <- tbl(
  con,
  I("catalog_40_copper_tad_share.lfs.lfs")
)

df <- lfs %>%
  dplyr::filter(YEAR > 2001) %>%
  dplyr::mutate(QUARTER = as.character(QUARTER))

directory_path <- paste0(
  system.file("tests", "testthat", package = "tidylfs"),
  "/data"
)

dir.create(directory_path, showWarnings = FALSE)

# Occupation Salary Test Data
earn06 <- df %>%
  dplyr::select(
    YEAR, QUARTER, FTPTWK, WEIGHT_INCOME,
    OCCUPATION_MAJOR, INDUSTRY_MAJOR,
    GRSSWK, HOURPAY, INECAC05
  ) |>
  collect()

save_file_path <- paste0(
  system.file("tests", "testthat", package = "tidylfs"),
  "/data/test-e1.Rds"
)

saveRDS_compressed(earn06, save_file_path)

# Unemployment Test Data
unem01 <- df %>%
  dplyr::filter(YEAR > 1996) %>%
  dplyr::select(YEAR, QUARTER, ILODEFR, WEIGHT, SEX, INECAC05) %>%
  na.exclude() |>
  collect()

save_file_path <- paste0(
  system.file("tests", "testthat", package = "tidylfs"),
  "/data/test-u1.Rds"
)

saveRDS_compressed(unem01, save_file_path)

# Hours Test Data
hour01 <- df %>%
  dplyr::select(
    QUARTER, ILODEFR, WEIGHT, FTPTWK, TTACHR
  ) %>%
  na.exclude() |>
  collect()

save_file_path <- paste0(
  system.file("tests", "testthat", package = "tidylfs"),
  "/data/test-h1.Rds"
)

saveRDS_compressed(hour01, save_file_path)
