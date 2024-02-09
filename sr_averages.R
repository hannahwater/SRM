#' Calculate Daily Averages of SR
#'
#' Computes the daily average SR from a 3D array where each "slice" along the third dimension represents a day.
#'
#' @param sr A 3D array of SR indices with dimensions for latitude, longitude, and day.
#'
#' @return A vector containing the daily average SR index for each day.
#' @export


sr_averages <- function(sr) {
  # Initialize an array to store the daily averages
  daily_averages <- array(dim = c(1, 1, dim(sr)[3]))

  # Loop over each day
  for (day in 1:dim(sr)[3]) {
    # Calculate the average for the current day
    daily_averages[1, 1, day] <- mean(sr[,,day],na.rm = TRUE)
  }

  return(daily_averages)
}
