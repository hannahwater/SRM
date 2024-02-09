folders_in_month <- function(year, month) {
  # Create a date object for the first day of the specified month and year
  start_of_year <- as.Date(paste(year, "01", "01", sep = "-"))
  start_of_month<- as.Date(paste(year, month, "01", sep = "-"))
  start_folder <- as.numeric(difftime(start_of_month, start_of_year, units = "days"))+1
  end_of_month<- as.Date(paste(year, month+1, "01", sep = "-"))
  end_folder <- as.numeric(difftime(end_of_month, start_of_year, units = "days"))
  folder_list <- sprintf("%03d", start_folder:end_folder)
  return(folder_list)
}
