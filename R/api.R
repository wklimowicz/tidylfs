#' Get time series from ONS Api
#'
#' Get a list with ONS API output
#'
#' @param dataset_id Name of ONS dataset
#' @param timeseries_id Name of ONS time series
#' @return A Jsonlite(?) object from the ONS API
#' @keywords internal
#' @examples
#'
#' # CPIH Inflation
#' ons_time_series("MM23", "L55O") %>%
#'   purrr::pluck("years") %>%
#'   dplyr::select(date, value)
#'
#' # Unemployment Rate
#' ons_time_series("LMS", "MGSX") %>%
#'   purrr::pluck("months") %>%
#'   dplyr::select(date, value)
#' @export
ons_time_series <- function(dataset_id, timeseries_id) {
  api_endpoint <- "https://api.ons.gov.uk/"

  url <- paste(
    sep = "/",
    api_endpoint,
    "timeseries", timeseries_id,
    "dataset", dataset_id,
    "data"
  )

  return(jsonlite::fromJSON(url))
}
