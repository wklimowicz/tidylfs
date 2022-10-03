saveRDS_compressed <- function(df, path) {
  connection <- xzfile(path, compression = 1)
  saveRDS(df, connection)
  on.exit(close(connection))
}
