#' Selects and renames columns from LFS File
#'
#' Checks which variables are present and picks them in order of priority
#' eg. PWT20 > PWT18 etc. Renames them so that they can be binded to one dataset
#'
#' Note: importing labelled from haven is required to import haven when
#' tests are run - otherwise it can't use the haven labelled format.
#' (imported functions causes tests to run library(haven))
#'
#' @param file Index of file
#' @param total_files Vector of file names - helper function for cli
#' @param file_format File extension: sav csv or Rds
#' @param extra_mappings Either NULL (use default) or an R script with the lfs_extra_mappings function
#' @param file_names To Save intermediate Rds files.
#'
#' @importFrom rlang .data
#'
#' @return List of output
#' @keywords internal
#'
#' @importFrom haven labelled
lfs_tidy_file <- function(file,
                          total_files,
                          file_format,
                          file_names,
                          extra_mappings = NULL) {

  cli::cli_div(theme = list(span.emph = list(color = "blue")))
  cli::cli_progress_step("Processing {.emph {total_files[[file]]} } {file}/{length(total_files)}.")
  cli::cli_end()

  df2 <- read_correct_format(file_format, total_files[[file]])

  colnames(df2) <- toupper(colnames(df2))

  cols <- colnames(df2)

  complete_mappings <- lfs_default_mappings(cols)

  if (!is.null(extra_mappings)) {
    # lfs_extra_mappings <- NULL # R CMD Check note

    vars_extra <- extra_mappings(cols)

    vars_extra <- vars_extra %>%
      dplyr::mutate(lfs_name = ifelse(.data$lfs_name %in% cols, .data$lfs_name, NA))

    complete_mappings <- dplyr::bind_rows(complete_mappings, vars_extra)
  }

  # Only take the variables that are present - eg. "UNION" is only in Q4's
  vars_present <- complete_mappings$lfs_name[complete_mappings$lfs_name %in% cols]

  df3 <- df2 %>%
    dplyr::select(dplyr::all_of(vars_present)) %>%
    dplyr::distinct()

  colnames(df3) <- complete_mappings$new_name[complete_mappings$lfs_name %in% vars_present]

  stopifnot(length(colnames(df3)) == sum(complete_mappings$lfs_name %in% cols))

  # Investigate how to integrate this
  # Try as_factor(level = "both")
  # dplyr::mutate(across(haven::is.labelled, haven::as_factor)) # Fix or keep current?

  factor_variables <- ifelse(complete_mappings$type == "factor", complete_mappings$new_name, NA)
  factor_variables <- factor_variables[!is.na(factor_variables)]

  numeric_variables <- ifelse(complete_mappings$type == "numeric", complete_mappings$new_name, NA)
  numeric_variables <- numeric_variables[!is.na(numeric_variables)]

  df3 <- df3 %>%
    dplyr::mutate(dplyr::across(
      dplyr::any_of(factor_variables),
      haven::as_factor
    )) %>%
    dplyr::mutate(dplyr::across(
      dplyr::any_of(numeric_variables),
      as.numeric
    ))

  return(list(df3, complete_mappings))
}



#' Compile to 1 R file
#'
#' Compiles seperate Rds files into one, picking relevant columns
#'
#' @param lfs_directory Path to directory with LFS files
#' @param extra_mappings Either NULL (use default) or a file
#' which has the custom mapping function.
#' See \code{\link{lfs_extra_mappings}}
#' @param save_location File path for `Rds` file to save. By default, saves the
#' output in the package directory, so it's always accessible with `lfs_load`.
#' @param save_variables_report Save a csv with the list of picked variables?
#'
#' @return Nothing - saves the Rds file only.
#' 
#' @export
lfs_compile <- function(lfs_directory,
                        extra_mappings = NULL,
                        save_location = "package",
                        save_variables_report = TRUE) {

  # Get list of files ----------------------------------------

  files_in_directory <- list.files(lfs_directory)

  if (length(files_in_directory) == 0) {
    cli::cli_alert_danger("No Files Found")
    invokeRestart("abort")
  }


  cli::cli_alert_info("Found {length(files_in_directory)} file{?s} in directory")

  correct_file_index <- stringr::str_detect(
    files_in_directory,
    "^\\d{4}( |_)Q\\d\\.(sav|csv|Rds)$"
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
    file_names = lfs_files,
    extra_mappings = extra_mappings
  )

  # Make record of what variables chosen if picked ---------------------------

  if (save_variables_report == TRUE) {
    final_mapping <- purrr::map(lfs_data, 2)[[1]][[2]]
    variables_report <- purrr::map(lfs_data, 2) %>%
      purrr::map("lfs_name")

    names(variables_report) <- substr(lfs_files, 1, 7)
    variables_report <- dplyr::bind_rows(variables_report, .id = "QUARTER")
    variables_report <- base::t(variables_report)
    variables_report <- as.data.frame(variables_report)

    variables_report <- variables_report %>%
      dplyr::mutate(QUARTER = row.names(variables_report)) %>%
      dplyr::relocate(.data$QUARTER)


    colnames(variables_report) <- c("QUARTER", final_mapping)

    tryCatch(
      {
        readr::write_csv(variables_report, file = "variables_report.csv")
      },
      error = function(cond) {
        cli::cli_alert_danger("variables_report.csv is open - close it and rerun to get the report")
      }
    )
  }

  # Add Quarter Year and Save ----------------------------------------
  lfs_data_frame <- purrr::map(lfs_data, 1)

  # Rename for quarters
  lfs_quarter_names <- tools::file_path_sans_ext(lfs_files)

  lfs_data_frame <- stats::setNames(lfs_data_frame, lfs_quarter_names)

  lfs_data_frame <- dplyr::bind_rows(lfs_data_frame, .id = "QUARTER") %>%
    dplyr::mutate(YEAR = as.integer(substr(.data$QUARTER, 1, 4))) %>%
    dplyr::relocate(.data$YEAR) %>%
    annotate_hiquald()



  if (save_location == "package") {
    save_file_path <- paste0(
      system.file(package = "tidylfs"),
      "/lfs_data.Rds"
    )
  } else {
    save_file_path <- save_location
  }

  connection <- gzfile(save_file_path, compression = 1)
  on.exit(close(connection))

  saveRDS(lfs_data_frame, connection)

  # Print complete message
  cli_compiling_complete(file_format = file_format)
}


#' Add Custom Column Mappings
#'
#' Create file with function that can be used to add columns
#'
#' @param file_name Name of mapping file
#'
#' @export
lfs_extra_mappings <- function(file_name = "extra_lfs_mappings.R") {
  if (file.exists(file_name)) {
    stop("You already have this file in your directory - move or rename it")
  }

  cli::cli_div(theme = list(span.emph = list(color = "orange")))
  cli::cli_alert_info("Pass this function to {.emph lfs_compile} to add more columns to the compiled lfs dataset")
  cli::cli_alert_info("If a variable appears under different names, use a {.epmh case_when} statement to select variables")
  cli::cli_end()

  invisible(readline(prompt = "Press [enter] to continue"))

  file_contents <- readr::read_file(system.file("templates/lfs_extra_mappings.R", package = "tidylfs"))

  readr::write_file(file_contents, file = file_name)

  utils::file.edit(file_name)
}