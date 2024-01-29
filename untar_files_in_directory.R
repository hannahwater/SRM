untar_files_in_directory <- function(directory) {
  tgz_files <- list.files(directory, pattern = "\\.tgz$", full.names = TRUE)
  for (tgz_file in tgz_files) {
    untar(tgz_file, exdir = directory)
  }
  return()
}
