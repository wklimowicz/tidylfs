readRDS_encrypted <- function(path) {
  passphrase <- charToRaw(Sys.getenv("LFS_ENCRYPT"))
  key <- openssl::sha256(passphrase)
  encrypted_y <- readRDS(path)
  y <- unserialize(openssl::aes_cbc_decrypt(encrypted_y, key = key))
  y
}


saveRDS_encrypted <- function(df, path) {
  df <- serialize(df, NULL)

  passphrase <- charToRaw(Sys.getenv("LFS_ENCRYPT"))
  key <- openssl::sha256(passphrase)

  encrypted_df <- openssl::aes_cbc_encrypt(df, key = key)

  connection <- xzfile(path, compression = 1)
  saveRDS(encrypted_df, connection)
  on.exit(close(connection))
}



saveRDS_compressed <- function(df, path) {
  connection <- xzfile(path, compression = 1)
  saveRDS(df, connection)
  on.exit(close(connection))
}
