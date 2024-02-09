try_download_file <- function(remote_file, local_file_path, username, password) {
  try({
    h <- new_handle()
    handle_setopt(h, userpwd = paste0(username, ":", password))
    handle_setopt(h, verbose = FALSE)
    # Capturing the output of curl_fetch_disk and not returning it
    invisible(curl_fetch_disk(remote_file, local_file_path, handle = h))
  }, silent = TRUE)
}
