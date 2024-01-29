unzip_files_in_parallel <- function(local_directory) {

  no_cores <- detectCores() - 1
  registerDoParallel(cores = no_cores)

  subdirectories_y <- list.dirs(local_directory, full.names = TRUE, recursive = FALSE)
  foreach(directory_y = subdirectories_y, .packages = c("utils","foreach","doParallel"),.export = c("untar_files_in_directory")) %dopar% {
    subdirectories_m <- list.dirs(directory_y, full.names = TRUE, recursive = FALSE)
    foreach(directory = subdirectories_m) %dopar% {
    untar_files_in_directory(directory)
    }
  }
  stopImplicitCluster()
}
