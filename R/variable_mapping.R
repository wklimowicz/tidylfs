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

  tibble::as_tibble(attr(lfs, "variable_mapping"))
}
