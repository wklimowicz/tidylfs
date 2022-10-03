#' Group by and Summarise for unionisation rates
#'
#' `r lifecycle::badge("experimental")`
#'
#' @param lfs LFS Dataset
#' @param ... Variables to group by
#'
#' @return Tibble of outputs
#'
#' @importFrom rlang .data
lfs_summarise_union <- function(lfs, ...) {

    # data.table version
    if(any(class(lfs) == "data.table") == TRUE) {

        lfs_summarise_union_dt(lfs, ...)

    } else { # Tibble version

        lfs_summarise_union_df(lfs, ...)

    }

}

#' Group by and Summarise for hours worked
#'
#' `r lifecycle::badge("experimental")`
#'
#' @param lfs LFS Dataset
#' @param ... Variables to group by
#'
#' @return Tibble of outputs
#'
#' @importFrom rlang .data
lfs_summarise_hours <- function(lfs, ...) {

    # data.table version
    if(any(class(lfs) == "data.table") == TRUE) {

        lfs_summarise_hours_dt(lfs, ...)

    } else { # Tibble version

        lfs_summarise_hours_df(lfs, ...)

    }

}


lfs_summarise_hours_df <- function(lfs, ...) {

  lfs %>%
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

}


lfs_summarise_hours_dt <- function(lfs, ...) {

      vars <- eval(substitute(alist(...)))
      vars <- sapply(vars, deparse)

   lfs[, ILODEFR := as.numeric(ILODEFR)
       ][ILODEFR == 1 &
         WEIGHT > 0 &
         !is.na(TTACHR),
     list(
      n = .N,
      total_hours = sum(TTACHR),
      hours = sum(TTACHR * WEIGHT, na.rm = TRUE),
      weight = sum(WEIGHT, na.rm = TRUE)
      ), by = vars
     ][, `:=`(hours = hours / weight,
              weight = NULL)]

}




lfs_summarise_union_df <- function(lfs, ...) {

  lfs %>%
    dplyr::filter(
      .data$INECAC05 == "Employee",
      .data$WEIGHT > 0
      ) %>%
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

}



lfs_summarise_union_dt <- function(lfs, ...) {

      vars <- eval(substitute(alist(...)))
      vars <- sapply(vars, deparse)

   lfs[INECAC05 == "Employee" &
       WEIGHT > 0,
     list(
      n = .N,
      union = sum((as.numeric(UNION) == 1) * WEIGHT, na.rm = TRUE),
      outside_union = sum((as.numeric(UNION) == 2) * WEIGHT, na.rm = TRUE)
      ), by = vars
     ][, union_percentage := (union / (union + outside_union))]

}
