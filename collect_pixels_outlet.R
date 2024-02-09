#' Collect Pixel Coordinates Closest to a Given Outlet Point
#'
#' This function identifies the pixel coordinates within .nc files that are closest to a specified outlet point given by latitude and longitude.
#'
#' @param sample_directory Directory containing subdirectories of .nc files organized by year and month.
#' @param lat_out The latitude of the outlet point.
#' @param lon_out The longitude of the outlet point.
#'
#' @return A list containing the closest longitude and latitude values within the .nc files to the specified outlet, and their indices.
#' @importFrom ncdf4 nc_open ncvar_get nc_close
#' @export
#'



collect_pixels_outlet <- function(sample_directory, lat_out, lon_out) {

  # List subdirectories
  subdirectories <- list.dirs(sample_directory, full.names = TRUE, recursive = FALSE)

   # Find .nc files in the first subdirectory
  nc_files <- list.files(sample_directory, pattern = "\\.nc$", full.names = TRUE)
  if (length(nc_files) == 0) {
    stop("No .nc files found in the subdirectory.")
  }

  # Open the first .nc file to extract latitude and longitude
  nc_data <- nc_open(nc_files[1])
  lat_smos <- ncvar_get(nc_data, "lat")
  lon_smos <- ncvar_get(nc_data, "lon")
  nc_close(nc_data)


  # Processing for longitude
  filtered_lon <- lon_smos[lon_smos <= lon_out]
  a <- which(lon_smos == max(filtered_lon)) + 1  # Min column
  lon <- lon_smos[a]  # Right

  # Processing for latitude
  filtered_lat <- lat_smos[lat_smos <= lat_out]
  b <- which(lat_smos == max(filtered_lat)) + 1  # Min row
  lat <- lat_smos[b]  # North

  # Return a list of results (or modify as needed)
  return(list(lon_target = lon, lat_target = lat, lon_num=a, lat_num=b))
}
