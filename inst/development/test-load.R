test_that("load_lfs  works", {

lfs <- lfs_load()

expect_equal(class(lfs)[[1]], "tbl_df")


expect_gt(nrow(lfs), 2)

})
