#' Process Dielectric Constant from NetCDF Files
#'
#' Extracts and aggregates dielectric constant values from .nc files within a specified date range and spatial extent.
#'
#' @param local_directory Directory containing subdirectories of .nc files organized by year and month.
#' @param start_year Start year of the data extraction period.
#' @param start_month Start month of the data extraction period.
#' @param end_year End year of the data extraction period.
#' @param end_month End month of the data extraction period.
#' @param a1 Starting index for longitude.
#' @param a2 Ending index for longitude.
#' @param b1 Starting index for latitude.
#' @param b2 Ending index for latitude.
#'
#' @return A 3D array of dielectric constant values over the specified period and spatial extent.
#' @importFrom ncdf4 nc_open ncvar_get nc_close
#' @export
#'
#' @examples
#' process_DC("path/to/local_directory", 2020, 1, 2020, 12, 1, 100, 1, 100)
process_DC <- function(local_directory, start_year, start_month, end_year, end_month, a1, a2, b1, b2) {
  if (!requireNamespace("ncdf4", quietly = TRUE)) {
    stop("Package 'ncdf4' is required but is not installed.")
  }

  # Initialize the final time series array
  total_days <- as.integer(difftime(as.Date(sprintf("%04d-%02d-01",
                                                    ifelse(end_month == 12, end_year + 1, end_year),
                                                    ifelse(end_month == 12, 1, end_month + 1))),
                                    as.Date(sprintf("%04d-%02d-01", start_year, start_month)),
                                    units = "days"))
  final_dls <- array(NA, dim = c(total_days, a2 - a1 + 1, b2 - b1 + 1))

  # Initialize a day counter
  day_count <- 1

  # Loop over each year and month within the start and end dates
  for (year in start_year:end_year) {
    for (m in 1:12) {
      # Skip months outside the specified range
      if (year == start_year && m < start_month) next
      if (year == end_year && m > end_month) break

      month_folder <- sprintf("%02d", m)
      path_m <- file.path(local_directory,year, month_folder)

      file_list <- list.files(path_m, pattern = "\\.nc$", full.names = TRUE)
      file_names <- vector("list", length(file_list))
      for (k in seq_along(file_list)) {
        if (!file.info(file_list[k])$isdir) {
          file_names[[k]] <- basename(file_list[k])
        }
      }

      num_days_in_month <- get_days_in_month(m, year)
      for (day in 1:num_days_in_month) {
        if (day_count <= total_days) {
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
  }

  return(final_dls)
}
