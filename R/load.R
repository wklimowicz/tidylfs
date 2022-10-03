#' Load LFS Data
#'
#' This works only with the `DATA_DIRECTORY` environment variable set to a folder.
#' You can either manually save `lfs_data.fst` there, or it will optionally be
#' saved during compilation if the `DATA_DIRECTORY` variable is present while running
#' `lfs_compile()`
#'
#' @return A tibble with lfs data
#'
#' @param data.table Import as data.table?
#'
#' @export
lfs_load <- function(data.table = TRUE) {

  # Check if DATA_DIRECTORY environment variable is present
  if (Sys.getenv("DATA_DIRECTORY") == "") {
    cli::cli_alert_danger("You don't have the DATA_DIRECTORY environment variable set: see the help for `lfs_compile`.")

    stop()
  }

    lfs <- fst::read_fst(paste0(Sys.getenv("DATA_DIRECTORY"), "/lfs_data.fst"), as.data.table = data.table)

    return(lfs)

}


#' Load APS Data
#'
#' This works only with the `DATA_DIRECTORY` environment variable set to a folder.
#' You can either manually save `aps_data.fst` there, or it will optionally be
#' saved during compilation if the `DATA_DIRECTORY` variable is present while running
#' `lfs_compile(..., aps = TRUE)`
#'
#' @inheritParams lfs_load
#'
#' @export
aps_load <- function(data.table = TRUE) {

  # Check if DATA_DIRECTORY environment variable is present
  if (Sys.getenv("DATA_DIRECTORY") == "") {
    cli::cli_alert_danger("You don't have the DATA_DIRECTORY environment variable set: see the help for `lfs_compile`.")

    stop()
  }

    aps <- fst::read_fst(paste0(Sys.getenv("DATA_DIRECTORY"), "/aps_data.fst"), as.data.table = data.table)

    return(aps)


}
