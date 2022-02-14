#' Load LFS Data
#'
#' @return A tibble with lfs data
#'
#' @export
lfs_load <- function() {
  readRDS(paste0(system.file(package = "tidylfs"), "/lfs_data.Rds"))
}
