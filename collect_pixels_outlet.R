collect_pixels_outlet <- function(local_directory, lat_out, lon_out) {

  # List subdirectories
  subdirectories <- list.dirs(local_directory, full.names = TRUE, recursive = FALSE)

  # Find .nc files in the first subdirectory
  nc_files <- list.files(subdirectories[1], pattern = "\\.nc$", full.names = TRUE)

  # Open the first .nc file and extract latitude and longitude
  nc_data <- nc_open(nc_files[1])
  lat_smos <- nc_data$dim$lat$vals
  lon_smos <- nc_data$dim$lon$vals
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
