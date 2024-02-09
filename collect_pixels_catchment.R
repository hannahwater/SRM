#' Collect Pixel Coordinates within a Catchment from .nc Files
#'
#' This function processes NetCDF (.nc) files in a specified directory and extracts pixel coordinates
#' that fall within a specified shapefile catchment area.
#'
#' @param sample_directory Directory containing .nc files.
#' @param shapefile Path to the shapefile used for catchment area.
#'
#' @return A list containing longitude and latitude ranges and indices within the catchment.
#' @import ncdf4
#' @importFrom sf st_read
#' @export
#'
#' @examples
#' collect_pixels_catchment("path/to/sample_directory", "path/to/shapefile.shp")
collect_pixels_catchment <- function(sample_directory, shapefile) {
  # Ensure the 'ncdf4' and 'sf' packages are available
  if (!requireNamespace("ncdf4", quietly = TRUE)) {
    stop("Package 'ncdf4' is required but is not installed.")
  }
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("Package 'sf' is required but is not installed.")
  }

  # Load required libraries
  library(ncdf4)
  library(sf)

  # Find .nc files in the first subdirectory
  nc_files <- list.files(sample_directory, pattern = "\\.nc$", full.names = TRUE)
  if (length(nc_files) == 0) {
    stop("No .nc files found in the subdirectory.")
  }

  # Open the first .nc file and extract latitude and longitude
  nc_data <- nc_open(nc_files[1])
  on.exit(nc_close(nc_data))
  lat_smos <- ncvar_get(nc_data, "lat")
  lon_smos <- ncvar_get(nc_data, "lon")

  # Extract the bounding box from the shapefile
  bbox <- st_bbox(shapefile)

  # Filter longitude and latitude based on the bounding box
  lon_index <- which(lon_smos >= bbox["xmin"] & lon_smos <= bbox["xmax"])
  lon_index <- seq(min(lon_index),max(lon_index)+1,1)

  lat_index <- which(lat_smos >= bbox["ymin"] & lat_smos <= bbox["ymax"])
  lat_index <- seq(min(lat_index),max(lat_index)+1,1)
  lon <- lon_smos[lon_index] # Right
  lat <- lat_smos[lat_index]

  # Return a list of results
  return(list(lon_target = lon, lat_target = lat, lon_num = lon_index, lat_num = lat_index))
}
