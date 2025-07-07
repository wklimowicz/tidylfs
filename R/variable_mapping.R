#' Retrieve variable mapping for LFS/APS compilation
#'
#' @param lfs A data.frame containing the LFS/APS data.
#'
#' @return A data.frame with the variable mapping.
#' @export
variable_mapping <- function(lfs) {
  if (!inherits(lfs, "data.frame")) {
    cli::cli_abort(c(
      "The `lfs` argument must be a data.frame outputed by `lfs_compile()`."
    ))
  }

  tibble::as_tibble(attr(lfs, "variable_mapping"))
}
