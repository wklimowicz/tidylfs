#' Group by and Summarise into salary ONS Salary
#'
#'
#' @param lfs LFS Dataset
#' @param ... Variables to group by
#'
#' @importFrom rlang .data
#' @importFrom data.table .N
#'
#' @return Tibble or data.table with salary data
#'
#' @export
lfs_summarise_salary <- function(lfs, ...) {


    # data.table version
    if(any(class(lfs) == "data.table") == TRUE) {

        lfs_summarise_salary_dt(lfs, ...)

    } else { # Tibble version

        lfs_summarise_salary_df(lfs, ...)

    }

}


#' Group by and Summarise for ONS unemployment
#'
#'
#' @param lfs LFS Dataset
#' @param ... Variables to group by
#'
#' @return Tibble or data.table with unemployment data
#'
#' @importFrom rlang .data
#' @importFrom data.table .N
#'
#' @export
lfs_summarise_unemployment <- function(lfs, ...) {


    if(any(class(lfs) == "data.table") == TRUE) {

    lfs_summarise_unemployment_dt(lfs, ...)

    } else {

    lfs_summarise_unemployment_df(lfs, ...)

    }

}

lfs_summarise_salary_dt <- function(lfs, ...) {

      vars <- eval(substitute(alist(...)))
      vars <- sapply(vars, deparse)


   lfs[WEIGHT_INCOME > 0 &
          HOURPAY <= 100 &
          HOURPAY >= 0 &
          INECAC05 == "Employee",
        list(n = .N,
          median_weekly_pay = matrixStats::weightedMedian(GRSSWK,
            w = WEIGHT_INCOME,
            na.rm = TRUE
          ),
          median_hourly_pay = matrixStats::weightedMedian(HOURPAY,
            w = WEIGHT_INCOME,
            na.rm = TRUE
          ),
          paidweight = sum(GRSSWK * WEIGHT_INCOME, na.rm = TRUE),
          paidweight2 = sum(WEIGHT_INCOME, na.rm = TRUE),
          paidweighthrly = sum(HOURPAY * WEIGHT_INCOME, na.rm = TRUE),
          paidweighthrly2 = sum(WEIGHT_INCOME, na.rm = TRUE)),
          by = vars
        ][, `:=`(
          mean_weekly_pay = paidweight / (paidweight2),
          mean_hourly_pay = paidweighthrly / (paidweighthrly2),
          paidweight = NULL,
          paidweighthrly = NULL,
          paidweight2 = NULL,
          paidweighthrly2 = NULL)
        ][]

}


lfs_summarise_salary_df <- function(lfs, ...) {
  lfs %>%
    dplyr::filter(
      .data$WEIGHT_INCOME > 0,
      .data$HOURPAY <= 100,
      .data$HOURPAY >= 0,
      .data$INECAC05 == "Employee"
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
}


lfs_summarise_unemployment_dt <- function(lfs, ...) {

  vars <- eval(substitute(alist(...)))
  vars <- sapply(vars, deparse)


    lfs[WEIGHT > 0 & !is.na(ILODEFR),
        list(n = .N,
        employed = sum((ILODEFR == "In employment") * WEIGHT, na.rm = TRUE),
        unemployed = sum((ILODEFR == "ILO unemployed") * WEIGHT, na.rm = TRUE),
        inactive = sum((ILODEFR == "Inactive") * WEIGHT, na.rm = TRUE)),
        by = vars
        ][, `:=`(
          unemployed_percentage = unemployed / (employed + unemployed),
          employed_percentage = employed / (employed + unemployed + inactive),
          inactive_percentage = inactive / (employed + unemployed + inactive),
          unemployed = NULL,
          employed = NULL,
          inactive = NULL)]

}

lfs_summarise_unemployment_df <- function(lfs, ...) {
  lfs %>%
    dplyr::filter(
    .data$WEIGHT > 0,
    !is.na(.data$ILODEFR)
    ) %>%
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

}
