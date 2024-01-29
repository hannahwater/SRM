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
