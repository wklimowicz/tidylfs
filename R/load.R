#' Load LFS Data
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function is deprecated and will be removed in a future version of the package.
#'
#' It is recommended to explicitly save the compiled data to persist it.
#'
#' @return A tibble with lfs data
#'
#' @param data.table Import as data.table?
#'
#' @export
#' @keywords internal
lfs_load <- function(data.table = TRUE) {

  lifecycle::deprecate_warn(
    when = "0.1.0",
    what = "lfs_load()",
    details = "Write the compiled data explicitly to persist it."
  )

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
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function is deprecated and will be removed in a future version of the package.
#'
#' It is recommended to explicitly save the compiled data to persist it.
#'
#' @inheritParams lfs_load
#'
#' @export
aps_load <- function(data.table = TRUE) {

  lifecycle::deprecate_warn(
    when = "0.1.0",
    what = "aps_load()",
    details = "Write the compiled data explicitly to persist it."
  )

  # Check if DATA_DIRECTORY environment variable is present
  if (Sys.getenv("DATA_DIRECTORY") == "") {
    cli::cli_alert_danger("You don't have the DATA_DIRECTORY environment variable set: see the help for `lfs_compile`.")

    stop()
  }

    aps <- fst::read_fst(paste0(Sys.getenv("DATA_DIRECTORY"), "/aps_data.fst"), as.data.table = data.table)

    return(aps)


}
