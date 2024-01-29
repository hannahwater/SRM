get_days_in_month <- function(month, year) {
  days_in_month <- c(31, ifelse(is_leap_year(year), 29, 28), 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  return(days_in_month[month])
}
