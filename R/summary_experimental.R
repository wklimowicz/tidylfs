#' Group by and Summarise for unionisation rates
#'
#' `r lifecycle::badge("experimental")`
#'
#' @param data LFS Dataset
#' @param ... Variables to group by
#'
#' @return Tibble of outputs
#'
#' @importFrom rlang .data
#'
#'
#' @export
lfs_summarise_union <- function(data, ...) {
  df <- data %>%
    dplyr::filter(.data$INECAC05 == 1) %>%
    dplyr::filter(.data$WEIGHT > 0) %>%
    # dplyr::filter(.data$WEIGHT_INCOME > 0,
    #               # .data$HOURPAY <= 100,
    #               # .data$HOURPAY >= 0,
    #               .data$INECAC05 == 1) %>%
    dplyr::group_by(...) %>%
    dplyr::summarize(
      n = dplyr::n(),
      union = sum((as.numeric(.data$UNION) == 1) * .data$WEIGHT, na.rm = TRUE),
      outside_union = sum((as.numeric(.data$UNION) == 2) * .data$WEIGHT, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      union_percentage = .data$union /
        (.data$union + .data$outside_union),
    )

  return(df)
}


#' Group by and Summarise for hours worked
#'
#' `r lifecycle::badge("experimental")`
#'
#' @param data LFS Dataset
#' @param ... Variables to group by
#'
#' @return Tibble of outputs
#'
#' @importFrom rlang .data
#'
#' @export
lfs_summarise_hours <- function(data, ...) {
  df <- data %>%
    dplyr::mutate(ILODEFR = as.numeric(.data$ILODEFR)) %>%
    dplyr::filter(
      .data$ILODEFR == 1,
      .data$WEIGHT > 0
    ) %>%
    dplyr::filter(!is.na(.data$TTACHR)) %>%
    dplyr::group_by(...) %>%
    dplyr::summarise(
      n = dplyr::n(),
      total_hours = sum(.data$TTACHR),
      hours = sum(.data$TTACHR * .data$WEIGHT, na.rm = TRUE),
      weight = sum(.data$WEIGHT, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      hours = .data$hours / .data$weight
    ) %>%
    dplyr::select(-.data$weight)

  return(df)
}
