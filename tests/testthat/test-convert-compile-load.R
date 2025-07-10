test_that("Converting and compiling runs succesfully", {
  test_convert_compile <- function() {
    lfs_convert("test", "test_rds_data/")
    lfs <- lfs_compile("test_rds_data", dataset = "lfs")
  }

  withr::with_file(
    list(
      fs::dir_create("test"),
      fs::dir_create("test_rds_data"),
      "test/2003 Q4.sav" = haven::write_sav(lfs_test_data(), "test/2003 Q4.sav"),
      "test/2010 Q4.sav" = haven::write_sav(lfs_test_data(), "test/2010 Q4.sav")
    ),
    { expect_no_error(suppressMessages(test_convert_compile())) }
  )

  # APS
  test_convert_compile_aps <- function() {
    lfs_convert("test", "test_aps_data/")
    aps <- lfs_compile("test_aps_data/", dataset = "aps")
  }

  withr::with_file(
    list(
      fs::dir_create("test"),
      fs::dir_create("test_aps_data"),
      "test/APS 2003.sav" = haven::write_sav(lfs_test_data(), "test/APS 2003.sav"),
      "test/APS 2010.sav" = haven::write_sav(lfs_test_data(), "test/APS 2010.sav")
    ),
    { expect_no_error(suppressMessages(test_convert_compile_aps())) }
  )
})


test_that("Year filter works", {
  # Ye
  test_convert_compile <- function() {
    lfs_convert("test", "test_rds_data/")
    lfs <- lfs_compile("test_rds_data", dataset = "lfs", years = 2003)
  }

  withr::with_file(
    list(
      fs::dir_create("test"),
      fs::dir_create("test_rds_data"),
      "test/2003 Q4.sav" = haven::write_sav(lfs_test_data(), "test/2003 Q4.sav"),
      "test/2010 Q4.sav" = haven::write_sav(lfs_test_data(), "test/2010 Q4.sav")
    ),
    {
      suppressMessages({
        lfs_convert("test", "test_rds_data/")
        lfs <- lfs_compile("test_rds_data", dataset = "lfs", years = 2003)
        # 1 Row in the mapping
        expect_equal(variable_mapping(lfs) |> nrow(), 1L)
      })
    }
  )

})

test_that("Variable mapping works", {

  # Variable Mapping
  withr::with_file(
    list(
      fs::dir_create("test"),
      fs::dir_create("test_rds_data"),
      "test/2003 Q4.sav" = haven::write_sav(lfs_test_data(), "test/2003 Q4.sav"),
      "test/2010 Q4.sav" = haven::write_sav(lfs_test_data(), "test/2010 Q4.sav")
    ),
    {
      suppressMessages({
        lfs_convert("test", "test_rds_data/")
        lfs <- lfs_compile("test_rds_data", dataset = "lfs")
        # 2 Rows in the mapping
        expect_equal(variable_mapping(lfs) |> nrow(), 2L)
      })
    }
  )
})
