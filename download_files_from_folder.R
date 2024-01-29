download_files_from_folder <- function(folder, base_url, local_directory, year, month, username, password) {
  opts <- curlOptions(userpwd = paste0(username, ":", password), ftp.use.epsv = FALSE, dirlistonly = TRUE)
  folder_path <- file.path(base_url, folder, "/")

  file_list <- getURL(folder_path, .opts = opts)
  file_list <- unlist(strsplit(file_list, "\r*\n"))
  file_list <- file_list[file_list != ""] # Remove any empty entries

  # Download each file
  foreach(file_name = file_list, .packages = "curl", .export="try_download_file") %dopar% {
    remote_file <- paste0(folder_path,file_name)
    local_dir <- file.path(local_directory, year, sprintf("%02d", as.integer(month)))
    if (!dir.exists(local_dir)) {
      dir.create(local_dir, recursive = TRUE, showWarnings = FALSE)
    }

    local_file_path <- file.path(local_dir, file_name)

    try_download_file(remote_file, local_file_path, username, password)
  }

}


#file_name=file_list[1]
