extract_file_name <- function(ftp_line) {
  parts <- strsplit(ftp_line, " ")[[1]]
  parts <- parts[nzchar(parts)]  # Remove empty strings
  if (length(parts) >= 9) {
    return(paste(parts[9:length(parts)], collapse = " "))
  } else {
    return(NULL)
  }
}
