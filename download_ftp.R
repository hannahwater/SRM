#' Download Files from FTP
#'
#' Downloads files from an FTP server for a specified range of dates, storing them in a local directory.
#' Note: Users must have valid credentials and set them securely outside of this function.
#'
#' @param local_directory The local directory where files will be stored.
#' @param start_year The starting year of the file download period.
#' @param start_month The starting month of the file download period.
#' @param end_year The ending year of the file download period.
#' @param end_month The ending month of the file download period.
#'
#' @return None
#' @export
#'
#' @examples
#' download_ftp("path/to/local_directory", 2021, 1, 2021, 12) # Example usage
  download_ftp <- function(local_directory, start_year, start_month, end_year, end_month) {
  require(doParallel)
  require(foreach)

  # Inform users about credential requirement
  message("Ensure you have set the necessary credentials securely as environment variables or through another secure method. The necessary credentials, including the username and password, are accessible at https://sextant.ifremer.fr/Donnees/Catalogue#/metadata/8db7102b-1b22-4db3-949d-e51269417aae")

  # Set up parallel processing
  no_cores <- detectCores() - 1
  cl <- makeCluster(no_cores)
  registerDoParallel(cl)

  # Create a sequence of dates
  date_seq <- seq(as.Date(paste0(start_year, "-", start_month, "-01")),
                  as.Date(paste0(end_year, "-", end_month, "-01")),
                  by = "month")

  # Parallel loop for each month
  foreach(d = date_seq, .packages = c("RCurl", "curl","foreach", "doParallel"),
          .export = c("set_ftp","download_files_from_folder")) %do% {

            year <- format(d, "%Y")
            month <- format(d, "%m")
            local_dir <- file.path(local_directory, year, month)
            if (!dir.exists(local_dir)) {
              folder <- month
              connection_details <- set_ftp(year, month)
              base_url <- connection_details$base_url
              username <- connection_details$username
              password <- connection_details$password

              download_files_from_folder(folder, base_url, local_directory, year, month, username, password)
            }
          }

  stopCluster(cl)
  stopImplicitCluster()
}
