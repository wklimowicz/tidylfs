devtools::load_all()

df <- lfs_load() %>%
  dplyr::filter(YEAR > 2001)
# dplyr::filter(
#   QUARTER %in% c(
#     "1992 Q4",
#     "1995 Q4",
#     "1999 Q4",
#     "2003 Q2",
#     "2008 Q2",
#     "2011 Q2",
#     "2017 Q1",
#     "2017 Q2",
#     "2017 Q3",
#     "2017 Q4",
#     "2021 Q3"
#   )
# )

directory_path <- paste0(
  system.file("tests", "testthat", package = "tidylfs"),
  "/data"
)

dir.create(directory_path, showWarnings = FALSE)

# Occupation Salary Test Data
earn06 <- df %>%
  dplyr::select(
    YEAR, QUARTER, FTPTWK, WEIGHT_INCOME,
    OCCUPATION_MAJOR, INDUSTRY,
    GRSSWK, HOURPAY, INECAC05
  )
# dplyr::filter(FTPTWK == "Full-time") %>%
# dplyr::filter(WEIGHT_INCOME > 0, HOURPAY <= 100, HOURPAY >= 0, INECAC05 == 1)

save_file_path <- paste0(
  system.file("tests", "testthat", package = "tidylfs"),
  "/data/test-e1.Rds"
)


saveRDS_compressed(earn06, save_file_path)

# Unemployment Test Data

unem01 <- df %>%
  dplyr::filter(YEAR > 1996) %>%
  dplyr::select(YEAR, QUARTER, ILODEFR, WEIGHT, SEX, INECAC05) %>%
  na.exclude()

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
  na.exclude()
# dplyr::filter(
#   QUARTER %in% c(
#     "1992 Q4",
#     "1995 Q4",
#     "1999 Q4",
#     "2003 Q2",
#     "2008 Q2",
#     "2011 Q2",
#     "2017 Q2",
#     "2021 Q3"
#   )
# )

save_file_path <- paste0(
  system.file("tests", "testthat", package = "tidylfs"),
  "/data/test-h1.Rds"
)

saveRDS_compressed(hour01, save_file_path)
