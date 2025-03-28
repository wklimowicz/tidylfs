#' Convert files to Rds for faster compiling in future
#'
#' Converts a directory of .sav files to a new folder with .Rds files
#'
#' Note: importing labelled from haven is required to import haven when
#' tests are run - otherwise it can't use the haven labelled format.
#'
#' @param lfs_directory Directory with raw LFS files
#' @param output_directory Target directory to save Rds files
#' @param filter_files Limit to certain files, using a named vector
#' @param incremental Only convert files which don't exist in output directory
#'
#' @return List of output
#'
#' @importFrom haven labelled
#' @export
lfs_convert <- function(lfs_directory,
                        output_directory,
                        filter_files = NULL,
                        incremental = FALSE) {

  files_in_directory <- list.files(lfs_directory)

  . <- NULL # Fix R CMD Check

  if (!is.null(filter_files)) {
    files_in_directory <- files_in_directory %>%
      .[. %in% filter_files]
  }

  if (incremental == TRUE) {
    # Check what files exist and diff
   exist_in_output <- tools::file_path_sans_ext(list.files(output_directory))
   exist_in_input <- tools::file_path_sans_ext(list.files(lfs_directory))
   files_in_directory <- setdiff(exist_in_input, exist_in_output)

   # Reattach extension
   files_in_directory <- paste0(files_in_directory, ".sav")
  }



  if (length(files_in_directory) == 0) {
    cli::cli_alert_danger("No Files Found")
    invokeRestart("abort")
  }

  dir.create(file.path(output_directory), showWarnings = FALSE)

  cli::cli_alert_info("Found {length(files_in_directory)} file{?s} in directory")


  correct_file_index <- stringr::str_detect(
    files_in_directory,
    "^\\d{4}( |_)Q\\d\\.(sav|csv)$|^APS \\d{4}\\.(sav|csv)$"
  )

  # Take only files which match "4 digits Q digit" pattern
  lfs_files <- files_in_directory[correct_file_index]

  cli_warn_files(correct_file_index, files_in_directory)

  file_format <- tools::file_ext(files_in_directory)[correct_file_index]

  if ("Rds" %in% file_format) {
    cli::cli_alert_danger("Rds file format indicates you've already converted these files")
    invokeRestart("abort")
  }

  # Add path
  lfs_files_w_path <- file.path(lfs_directory, lfs_files)

  cli::cli_h1("Converting to Rds")

  # Tidy each dataset ----------------------------------------

  # null <- purrr::map(seq_along(lfs_files_w_path),
  #   lfs_convert_file,
  #   total_files = lfs_files_w_path,
  #   file_format = file_format[[1]], # simple
  #   output_directory = output_directory,
  #   file_names = lfs_files
  # )

  purrr::map(cli::cli_progress_along(lfs_files_w_path),
    lfs_convert_file,
    total_files = lfs_files_w_path,
    file_format = file_format[[1]], # simple
    output_directory = output_directory,
    file_names = lfs_files
  )

  # Print complete message
  cli_converting_complete(
    file_format = file_format,
    output_directory = output_directory
  )

  cli::stop_app()
}


lfs_convert_file <- function(file,
                             total_files,
                             file_format,
                             file_names,
                             output_directory) {

  # cli::cli_div(theme = list(span.emph = list(color = "blue")))
  # cli::cli_progress_step("{.emph {file_names[[file]]}} to Rds {file}/{length(file_names)}")
  # cli::cli_end()

  # cli_convert_progress(file_names, file)
  # cli_progress(file_names, file)


  file_path <- total_files[[file]]

  df2 <- read_correct_format(file_format, file_path)

  save_file_path <- paste0(
    output_directory,
    "/",
    tools::file_path_sans_ext(file_names[[file]]),
    ".Rds"
  )

  connection <- gzfile(save_file_path, compression = 1)
  on.exit(close(connection))

  saveRDS(df2, connection)

  return(NULL)
}
