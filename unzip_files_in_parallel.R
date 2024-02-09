#' Unzip Files in Parallel
#' Unzips all files in a given directory and its subdirectories using parallel processing.
#'
#' @param local_directory The path to the directory containing the zipped files.
#' @param start_year The starting year of the file download period.
#' @param start_month The starting month of the file download period.
#' @param end_year The ending year of the file download period.
#' @param end_month The ending month of the file download period.
#' @return None
#' @export
#'
#' @examples
#' unzip_files_in_parallel("path/to/local_directory") # Example usage
unzip_files_in_parallel <- function(local_directory, start_year, start_month, end_year, end_month) {
  require(doParallel)
  require(foreach)

  no_cores <- detectCores() - 1
  cl <- makeCluster(no_cores)

  # Get immediate subdirectories (assuming zipped files are organized by year/month)
  subdirectories_y <- list.dirs(local_directory, full.names = TRUE, recursive = FALSE)

  for(y in seq(start_year,end_year,1)) {
    directory_y=paste0(local_directory,"/",y)
    months <- seq(
      from = if(y == start_year) start_month else 1,
      to = if(y == end_year) end_month else 12,
      by = 1
    )
    for (m in months){
    directory_m=paste0(local_directory,"/",y,"/",sprintf("%02d", as.integer(m)))
      untar_files_in_directory(directory_m)
    }
  }
  stopImplicitCluster()
}
