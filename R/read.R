read_correct_format <- function(file_format, file_path) {
  if (file_format == "sav") {
    df <- haven::read_sav(
      file = file_path,
      user_na = FALSE,
      encoding = "latin1"
    )
  }

  if (file_format == "csv") {
    df <- readr::read_csv(file = file_path)
  }


  if (file_format == "Rds") {
    df <- readRDS(file = file_path)
  }

  return(df)
}
