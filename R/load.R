#' Load LFS Data
#'
#' @return A tibble with lfs data
#'
#' @param data.table Import as data.table?
#'
#' @export
lfs_load <- function(data.table = TRUE) {
    fst::read_fst(paste0(system.file(package = "tidylfs"), "/lfs_data.fst"),
    as.data.table = data.table)
}
