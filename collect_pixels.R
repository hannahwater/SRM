collect_pixels <- function(root_directory, shapefile) {

  # List subdirectories
  subdirectories <- list.dirs(root_directory, full.names = TRUE, recursive = FALSE)

  # Find .nc files in the first subdirectory
  nc_files <- list.files(subdirectories[1], pattern = "\\.nc$", full.names = TRUE)

  # Open the first .nc file and extract latitude and longitude
  nc_data <- nc_open(nc_files[1])
  lat_smos <- nc_data$dim$lat$vals
  lon_smos <- nc_data$dim$lon$vals
  nc_close(nc_data)

  S1 <- shapefile

  # Processing for longitude
  filtered_lon <- lon_smos[lon_smos <= min(S1$geometry[[1]][[1]][[1]][,1])]
  a1 <- which(lon_smos == max(filtered_lon)) + 1  # Min column

  filtered_lon <- lon_smos[lon_smos >= max(S1$geometry[[1]][[1]][[1]][,1])]
  a2 <- which(lon_smos == min(filtered_lon))  # Max column

  lon <- lon_smos[a1:a2]  # Right

  # Processing for latitude
  filtered_lat <- lat_smos[lat_smos <= min(S1$geometry[[1]][[1]][[1]][,2])]
  b1 <- which(lat_smos == max(filtered_lat)) + 1  # Min row

  filtered_lat <- lat_smos[lat_smos >= max(S1$geometry[[1]][[1]][[1]][,2])]
  b2 <- which(lat_smos == min(filtered_lat))  # Max column
  lat <- lat_smos[b1:b2]  # North

  # Return a list of results (or modify as needed)
  return(list(lon_target = lon, lat_target = lat, lon_num=a1:a2, lat_num=b1:b2))
}
