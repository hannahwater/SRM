#' Calculates the yearly Qfu values based on provided precipitation, evapotranspiration, for a specific range of months within each year,

#'
#' @param start_year The starting year of the calculation period.
#' @param end_year The ending year of the calculation period.
#' @param start_month The starting month of the calculation period within each year.
#' @param end_month The ending month of the calculation period within each year.
#' @param p A numeric vector of daily precipitation values over the period.
#' @param et A numeric vector of daily evapotranspiration values over the period.
#' @param a A parameter used in the Qfu calculation.
#'
#' @return A numeric vector of yearly Qfu values.
#' @export
#'
#' @examples
#'
#'
calculate_qfu <- function(start_year, end_year, start_month, end_month, p, pet, alpha) {
  years <- seq(from = start_year, to = end_year)

  get_index <- function(year, month, day = 1) {
    date <- as.Date(sprintf("%04d-%02d-%02d", year, month, day))
    start_date <- as.Date(sprintf("%04d-%02d-01", start_year,start_month))
    as.integer(date - start_date) + 1
  }

  days_in_month <- function(year, month) {
    if (month == 2 && is_leap_year(year)) {
      return(29)
    }
    days_in_month <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
    days_in_month[month]
  }

  Q_fu <- sapply(years, function(year) {
    start_idx <- get_index(year, start_month)
    end_day <- days_in_month(year, end_month)
    end_idx <- get_index(year, end_month, end_day)

    mp <- mean(p[start_idx:end_idx], na.rm = TRUE)
    met <- mean(pet[start_idx:end_idx], na.rm = TRUE)

    ai <- met / mp
    qfu_value <- mp * (1 + ai^alpha)^(1 / alpha) - met
    return(qfu_value)
  })

  return(Q_fu)
}
