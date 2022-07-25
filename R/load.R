#' Load LFS Data
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
#' @return A tibble with lfs data
#'
#' @param data.table Import as data.table?
#'
#' @export
aps_load <- function(data.table = TRUE) {

    tryCatch(
        {
    aps <- fst::read_fst(paste0(system.file(package = "tidylfs"), "/aps_data.fst"), as.data.table = data.table)
        },
        error = function(error_message) {
  cli::cli_div(theme = list(span.emph = list(color = "blue")))
            cli::cli_alert_danger("Can't find the {.emph aps_data.fst} file - have you ran {.emph lfs_convert} and {.emph lfs_compile} on the raw data?")
  message(error_message)
  cli::cli_end()
        },
  finally = aps
    )

# return(lfs)

}
