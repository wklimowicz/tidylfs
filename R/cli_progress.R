cli_compiling_complete <- function(file_format) {
  cli::cli_div(theme = list(span.emph = list(color = "orange")))

  cli::cli_alert_success("Compiling complete")

  cli::cli_alert_info("Load LFS data with {.emph lfs <- lfs_load()}\n
                       To check ONS definitions see {.emph variables_report.csv}\n")

  if (file_format[[1]] == "sav") {
    cli::cli_alert_info("You can view column labels with {.emph haven::print_labels}")
  }

  cli::cli_end()
}


cli_converting_complete <- function(file_format, output_directory) {
  cli::cli_div(theme = list(span.emph = list(color = "orange")))

  cli::cli_alert_success("Converting complete")

  cli::cli_alert_info("Run compiling on new folder with {.emph lfs_compile('{output_directory}')}\n")

  cli::cli_end()
}


cli_warn_files <- function(file_pattern, files_in_directory) {
  cli::cli_div(theme = list(span.emph = list(color = "orange")))


  if (sum(file_pattern) != length(files_in_directory)) {
    cli::cli_alert_warning(
      "{sum(file_pattern == FALSE)} file{?s} not matching [Year Quarter.ext]
      (eg. {.emph 2020 Q1.csv} or {.emph 2003 Q3.sav}):

      {files_in_directory[!file_pattern]}

      Ignoring it."
    )
  }

  cli::cli_end()
}


cli_progress <- function(total_files, file) {
  cli::cli_div(theme = list(span.emph = list(color = "blue")))
  cli::cli_progress_step("Processing {.emph {total_files[[file]]} } {file}/{length(total_files)}.")
  cli::cli_end()
}

cli_convert_progress <- function(file_names, file) {
  cli::cli_div(theme = list(span.emph = list(color = "blue")))
  cli::cli_progress_step("{.emph {file_names[[file]]}} to Rds {file}/{length(file_names)}")
  cli::cli_end()
}
