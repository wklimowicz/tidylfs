#' Retrieve variable mapping for LFS/APS compilation
#'
#' @param lfs A data.frame containing the LFS/APS data.
#'
#' @return A data.frame with the variable mapping.
#' @export
#' @examples
#' \dontrun{
#' lfs <- lfs_compile("lfs_data/")
#' variable_mapping(lfs)
#' }
variable_mapping <- function(lfs) {
  if (!inherits(lfs, "data.frame")) {
    cli::cli_abort(c(
      "The `lfs` argument must be a data.frame outputed by `lfs_compile()`."
    ))
  }

  mapping <- attr(lfs, "variable_mapping")
  
  if (is.null(mapping)) {
    cli::cli_abort(c(
      "x" = "No variable mapping attribute found.",
      "i" = "This can only be done on LFS data after running {.fn lfs_compile}.",
      "!" = "Possible causes:",
      " " = "* The data was saved to a format that doesn't preserve attributes (e.g., CSV)",
      " " = "* The data was processed with functions that strip attributes",
      " " = "* The data was not created with {.fn lfs_compile}",
      "i" = "Consider using {.strong parquet} format when saving intermediate outputs to preserve attributes."
    ))
  }

  tibble::as_tibble(mapping)
}
