#' Selects and renames columns from LFS File
#'
#' Checks which variables are present and picks them in order of priority
#' eg. PWT20 > PWT18 etc. Renames them so that they can be binded to one dataset
#'
#' To automatically save the compiled file in a directory ready to be loaded with
#' `lfs_load()`, set the `DATA_DIRECTORY` environment variable to point at a folder.
#'
#' @param file Index of file
#' @param total_files Vector of file names - helper function for cli
#' @param file_format File extension: sav csv or rds
#' @param extra_mappings Either NULL (use default) or an R script with the lfs_extra_mappings function
#'
#' @return List of output
#' @keywords internal
#'
#' @importFrom haven labelled
lfs_tidy_file <- function(file,
                          total_files,
                          file_format,
                          extra_mappings = NULL) {

# Note: importing labelled from haven is required to import haven when
# tests are run - otherwise it can't use the haven labelled format.
# (imported functions causes tests to run library(haven))

  cli::cli_div(theme = list(span.emph = list(color = "blue")))
  cli::cli_progress_step("Processing {.emph {total_files[[file]]} } {file}/{length(total_files)}.")
  cli::cli_end()

  df2 <- read_correct_format(file_format, total_files[[file]])

  # Some filenames have lower case columns - capitalise for consistency
  colnames(df2) <- toupper(colnames(df2))

  cols <- colnames(df2)

  complete_mappings <- lfs_default_mappings(cols)

  if (!is.null(extra_mappings)) {
    # lfs_extra_mappings <- NULL # R CMD Check note

    vars_extra <- extra_mappings(cols)

    vars_extra <- vars_extra %>%
      dplyr::mutate(lfs_name = ifelse(.data$lfs_name %in% cols,
                                      .data$lfs_name,
                                      NA_character_))

    # Remove existing mappings if overwritten
    complete_mappings <- complete_mappings %>%
      dplyr::filter(!.data$new_name %in% vars_extra[[2]])

    # TODO: more verification if user writes the same variable to a different name
    # eg. UNION -> union_rep breaks the code

    complete_mappings <- dplyr::bind_rows(complete_mappings, vars_extra)
  }



  # Only take the variables that are present - eg. "UNION" is only in Q4's
  vars_present <- complete_mappings$lfs_name[complete_mappings$lfs_name %in% cols]

  df3 <- df2 %>%
    dplyr::select(dplyr::all_of(vars_present)) %>%
    dplyr::distinct()

  new_names <- complete_mappings$new_name[complete_mappings$lfs_name %in% vars_present]

  colnames(df3) <- new_names
  stopifnot(length(colnames(df3)) == sum(complete_mappings$lfs_name %in% cols))

  # Investigate how to integrate this
  # Try as_factor(level = "both")
  # dplyr::mutate(across(haven::is.labelled, haven::as_factor)) # Fix or keep current?

  factor_variables <- ifelse(complete_mappings$type == "factor", complete_mappings$new_name, NA)
  factor_variables <- factor_variables[!is.na(factor_variables)]

  numeric_variables <- ifelse(complete_mappings$type == "numeric", complete_mappings$new_name, NA)
  numeric_variables <- numeric_variables[!is.na(numeric_variables)]

  character_variables <- ifelse(complete_mappings$type == "character", complete_mappings$new_name, NA)
  character_variables <- character_variables[!is.na(character_variables)]

    unlabelled_factor_variables <- ifelse(complete_mappings$type == "unlabelled_factor", complete_mappings$new_name, NA)
    unlabelled_factor_variables <- unlabelled_factor_variables[!is.na(unlabelled_factor_variables)]



  # Explicitly convert problem columns to correct type, for vctrs compatibility

  # if ("PARENTAL_OCCUPATION" %in% vars_present) {

  columns_coerce <- c("PARENTAL_OCCUPATION", "PARENTAL_OCCUPATION_MAJOR")

  df3 <- df3 %>%
    dplyr::mutate(dplyr::across(
      dplyr::any_of(columns_coerce),
      ~ as.integer(as.character(.x))
    ))

  #   }


  df3 <- df3 %>%
    dplyr::mutate(dplyr::across(
      dplyr::any_of(factor_variables),
      haven::as_factor
    )) %>%
    dplyr::mutate(dplyr::across(
      dplyr::any_of(numeric_variables),
      as.numeric
    )) %>%
    dplyr::mutate(dplyr::across(
      dplyr::any_of(character_variables),
      as.character
    )) %>%
    dplyr::mutate(dplyr::across(
      dplyr::any_of(unlabelled_factor_variables),
      ~ .x |> as.character() |> as.factor()
        ))


  return(list(df3, complete_mappings))
}



#' Compile to single data.frame
#'
#' Compiles seperate fst files into one, picking relevant columns
#'
#' @param lfs_directory Path to directory with LFS files
#' @param filter_years Vector of years to filter down to
#' @param extra_mappings Either NULL (use default) or a file
#' which has the custom mapping function.
#' See
#' \code{vignette("Adding_Variables", package = "tidylfs")}
#' @param save_to_folder If TRUE, will save to the `DATA_DIRECTORY` environment variable if present
#' @param save_variables_report Save a csv with the list of picked variables?
#' @param aps Annual Population Survey flag, files called eg. "APS 2012.sav"
#'
#' @return Nothing - saves the fst file only.
#'
#' @export
lfs_compile <- function(lfs_directory,
                        filter_years = NULL,
                        extra_mappings = NULL,
                        save_to_folder = FALSE,
                        save_variables_report = TRUE,
                        aps = FALSE) {

  # Get list of files ----------------------------------------

  files_in_directory <- list.files(lfs_directory)

  if (length(files_in_directory) == 0) {
    cli::cli_alert_danger("No Files Found")
    invokeRestart("abort")
  }

  # Filter down if filter_years isn't null
  if (!is.null(filter_years)) {

    # Collapse vector into numbers seperated by "|", then grepl
    filtered_vector <- filter_years |>
      paste(collapse = "|") |>
      grep(files_in_directory)

    files_in_directory <- files_in_directory[filtered_vector]
  }

  cli::cli_alert_info("Found {length(files_in_directory)} file{?s} in directory")

  correct_file_index <- stringr::str_detect(
    files_in_directory,
    "^\\d{4}( |_)Q\\d\\.(sav|csv|Rds)$|APS \\d{4}.(sav|csv|Rds)$\\d{4} APS.(sav|csv|Rds)"
  )

  # Take only files which match "4 digits Q digit" pattern
  lfs_files <- files_in_directory[correct_file_index]

  cli_warn_files(correct_file_index, files_in_directory)


  # Get file extension
  file_format <- tools::file_ext(files_in_directory)[correct_file_index]

  # Check all file extensions are the same
  if (length(unique(file_format)) != 1) {
    cli::cli_alert_danger("File extensions should all be the same")
    invokeRestart("abort")
  }


  # Make a full path variable
  lfs_files_w_path <- file.path(lfs_directory, lfs_files)

  cli::cli_h1("Renaming and Tidying LFS Columns")


  # Tidy each dataset ----------------------------------------

  # Maps along each file and tidies it - using seq_along to make
  # cli bar work properly

  lfs_data <- purrr::map(seq_along(lfs_files_w_path),
    lfs_tidy_file,
    total_files = lfs_files_w_path,
    file_format = file_format[[1]], # Should all be the same
    extra_mappings = extra_mappings
  )

  # Make record of what variables chosen if picked ---------------------------
  cli::cli_alert_info("Combining...")

  if (save_variables_report == TRUE) {
    final_mapping <- purrr::map(lfs_data, 2)[[1]][[2]]
    variables_report <- purrr::map(lfs_data, 2) %>%
      purrr::map("lfs_name")

    if (aps == TRUE) {
    names(variables_report) <- substr(lfs_files, 1, 8)
    } else {
    names(variables_report) <- substr(lfs_files, 1, 7)
    }

    variables_report <- dplyr::bind_rows(variables_report, .id = "QUARTER")
    variables_report <- base::t(variables_report)
    variables_report <- as.data.frame(variables_report)

    variables_report <- variables_report %>%
      dplyr::mutate(QUARTER = row.names(variables_report)) %>%
      dplyr::relocate("QUARTER")

    colnames(variables_report) <- c("QUARTER", final_mapping)

    # If file is open, give warning and continue
    tryCatch(
      {
        if (aps == 0) {
        readr::write_csv(variables_report, file = "lfs_variables_report.csv")
        cli::cli_alert_info("Load LFS data with {.emph lfs <- lfs_load()}\n
                             To check ONS definitions see {.emph lfs_variables_report.csv}\n")
        } else {
        readr::write_csv(variables_report, file = "aps_variables_report.csv")
        cli::cli_alert_info("Load APS data with {.emph aps <- aps_load()}\n
                             To check ONS definitions see {.emph aps_variables_report.csv}\n")
        }
      },
      error = function(cond) {
        cli::cli_alert_danger("lfs_variables_report.csv is open - close it and rerun to get the report")
      }
    )
  }

  # Add Quarter Year and Save ----------------------------------------
  lfs_data_frame <- purrr::map(lfs_data, 1)

  # Rename for quarters
  lfs_quarter_names <- tools::file_path_sans_ext(lfs_files)

  lfs_data_frame <- stats::setNames(lfs_data_frame, lfs_quarter_names)

  cli::cli_alert_info("Merging descriptions into the main dataset")


  # Assign annotations to columns
  if (aps == TRUE) {

  lfs_data_frame <- data.table::rbindlist(lfs_data_frame, idcol = "YEAR", fill = TRUE)

  lfs_data_frame[, `:=`(YEAR = as.integer(substr(YEAR, 5, 8)))]


  lfs_data_frame <- lfs_data_frame |>
    annotate_hiquald() |>
    annotate_degree7() |>
    annotate_occupation() |>
    create_HSERIAL()
    # annotate_industry() |>
    # annotate_economic_activity()

  data.table::setcolorder(lfs_data_frame, c("YEAR", "CASENO"))

  } else {

  lfs_data_frame <- data.table::rbindlist(lfs_data_frame, idcol = "QUARTER", fill = TRUE)

  lfs_data_frame[, `:=`(YEAR = as.integer(substr(QUARTER, 1, 4)))]

  # TODO: For generality, this should only run conditional on the relevant variables being included
  # E.g. UKDS version doesn't have SIC07/SIC92, so `annotate_industry` errors out.
  lfs_data_frame <- lfs_data_frame |>
    annotate_hiquald() |>
    annotate_degree7() |>
    annotate_occupation() |>
    annotate_industry() |>
    annotate_economic_activity() |>
    annotate_subject() |>
    create_HSERIAL()

  data.table::setcolorder(lfs_data_frame, c("YEAR", "QUARTER", "CASENO"))

  # Convert QUARTER to factor
  lfs_data_frame[, QUARTER := as.factor(QUARTER)]

  }



  if (aps == TRUE) {
  save_name <- "aps_data.fst"
  } else {
  save_name <- "lfs_data.fst"
  }

  # If enabled
  if (save_to_folder) {
      # If DATA_DIRECTORY environment variable is present, save there.
      if (Sys.getenv("DATA_DIRECTORY") != "") {

        cli::cli_alert_info("Saving as fst")

        fst::write_fst(
          lfs_data_frame,
          paste0(Sys.getenv("DATA_DIRECTORY"), "/", save_name)
        )

      }
  }

  # Print complete message
  cli_compiling_complete(file_format = file_format, aps = aps)

  return(lfs_data_frame)

}
