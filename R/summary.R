#' Group by and Summarise into salary ONS Salary
#'
#'
#' @param data LFS Dataset
#' @param ... Variables to group by
#'
#' @importFrom rlang .data
#'
#' @return Tibble of outputs
#'
#' @export
lfs_summarise_salary <- function(data, ...) {
  df <- data %>%
    dplyr::filter(
      .data$WEIGHT_INCOME > 0,
      .data$HOURPAY <= 100,
      .data$HOURPAY >= 0,
      .data$INECAC05 == 1
    ) %>%
    dplyr::group_by(...) %>%
    dplyr::summarize(
      n = dplyr::n(),
      median_weekly_pay = matrixStats::weightedMedian(.data$GRSSWK,
        w = .data$WEIGHT_INCOME,
        na.rm = TRUE
      ),
      median_hourly_pay = matrixStats::weightedMedian(.data$HOURPAY,
        w = .data$WEIGHT_INCOME,
        na.rm = TRUE
      ),
      paidweight = sum(.data$GRSSWK * .data$WEIGHT_INCOME, na.rm = TRUE),
      paidweight2 = sum(.data$WEIGHT_INCOME, na.rm = TRUE),
      paidweighthrly = sum(.data$HOURPAY * .data$WEIGHT_INCOME, na.rm = TRUE),
      paidweighthrly2 = sum(.data$WEIGHT_INCOME, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      mean_weekly_pay = .data$paidweight / (.data$paidweight2),
      mean_hourly_pay = .data$paidweighthrly / (.data$paidweighthrly2)
    ) %>%
    dplyr::select(
      -.data$paidweight,
      -.data$paidweighthrly,
      -.data$paidweight2,
      -.data$paidweighthrly2
    ) %>%
    dplyr::relocate(...)

  return(df)
}


#' Group by and Summarise for ONS unemployment
#'
#'
#' @param data LFS Dataset
#' @param ... Variables to group by
#'
#' @return Tibble of outputs
#'
#' @importFrom rlang .data
#'
#' @export
lfs_summarise_unemployment <- function(data, ...) {
  df <- data %>%
    dplyr::filter(.data$WEIGHT > 0) %>%
    dplyr::filter(!is.na(.data$WEIGHT), !is.na(.data$ILODEFR)) %>%
    dplyr::group_by(...) %>%
    dplyr::summarize(
      n = dplyr::n(),
      employed = sum((.data$ILODEFR == "In employment") * .data$WEIGHT, na.rm = TRUE),
      unemployed = sum((.data$ILODEFR == "ILO unemployed") * .data$WEIGHT, na.rm = TRUE),
      inactive = sum((.data$ILODEFR == "Inactive") * .data$WEIGHT, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      unemployed_percentage = .data$unemployed /
        (.data$employed + .data$unemployed),
      employed_percentage = .data$employed /
        (.data$employed + .data$unemployed + .data$inactive),
      inactive_percentage = .data$inactive /
        (.data$employed + .data$unemployed + .data$inactive),
    ) %>%
    dplyr::select(
      -.data$unemployed,
      -.data$employed,
      -.data$inactive
    )

  return(df)
}
