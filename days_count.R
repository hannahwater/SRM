days_count <- function(year) {
  # Start of the given year
  start_of_year <- as.Date(paste(year, "01", "01", sep = "-"))

  # Current date
  current_date <- Sys.Date()

  # Calculate difference in days
  days_difference <- as.numeric(difftime(current_date, start_of_year, units = "days"))

  return(days_difference)
}
