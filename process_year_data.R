process_year_data <- function(year, base_path, a1, a2, b1, b2) {
  # Calculate the number of days in the year
  daysInYear <- sum(sapply(1:12, function(m) get_days_in_month(m, year)))

  # Initialize the final time series array
  final_dls <- array(NA, dim = c(daysInYear, a2 - a1 + 1, b2 - b1 + 1))

  # Initialize a day counter
  day_count <- 1

  # Loop over each month
  for (m in 1:12) {
    month_folder <- sprintf("%02d", m)
    path_m <- file.path(base_path, month_folder)

    file_list <- list.files(path_m, pattern = "\\.nc$", full.names = TRUE)
    file_names <- vector("list", length(file_list))
    for (k in seq_along(file_list)) {
      if (!file.info(file_list[k])$isdir) {
        file_names[[k]] <- basename(file_list[k])
      }
    }

    num_days_in_month <- get_days_in_month(m, year)
    for (day in 1:num_days_in_month) {
      if (day_count <= daysInYear) {
        file_path <- file.path(path_m, file_names[day])
        if (file.exists(file_path)) {
          nc_file <- nc_open(file_path)
          dl <- ncvar_get(nc_file, "Dielect_Const", start = c(1, a1, b1), count = c(1, a2 - a1 + 1, b2 - b1 + 1))
          nc_close(nc_file)

          final_dls[day_count, , ] <- dl
        }
        day_count <- day_count + 1
      }
    }
  }

  return(final_dls)
}
