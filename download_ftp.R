download_ftp <- function(local_directory, start_year, start_month, end_year, end_month) {
  message("The necessary credentials, including the username and password, are accessible at https://sextant.ifremer.fr/Donnees/Catalogue#/metadata/8db7102b-1b22-4db3-949d-e51269417aae")

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
    folder <- month
    connection_details <- set_ftp(year, month)
    base_url <- connection_details$base_url
    username <- connection_details$username
    password <- connection_details$password

    download_files_from_folder(folder, base_url, local_directory, year, month, username, password)

 }

  stopCluster(cl)
  stopImplicitCluster()
}


#d=date_seq[1]
#year="2021"
#month="06"
#d=date_seq[12]
