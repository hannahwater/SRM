days_in_month <- function(year, month) {
  # Create a date object for the first day of the specified month and year
  date_start <- as.Date(paste(year, month, "01", sep = "-"))

  # Calculate the first day of the next month
  next_month <- date_start %m+% months(1)

  # Calculate the number of days in the month
  num_days <- as.integer(next_month - date_start)

  return(num_days)
}
