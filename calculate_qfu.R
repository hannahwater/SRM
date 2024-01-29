calculate_qfu <-function(start_year, end_year, p, et, a){

    years <- seq(from = start_year, to = end_year)

    # Function to calculate qfu for a given year
      calculate_qfu <- function(year, p, et, a) {
      start_idx <- (year - start_year) * 365 + 1
      end_idx <- start_idx + 364

      mq <- mean(q[start_idx:end_idx], na.rm = TRUE)
      mp <- mean(p[start_idx:end_idx], na.rm = TRUE)
      met <- mean(et[start_idx:end_idx], na.rm = TRUE)

      ai <- met / mp
      mp * (1 + ai^a)^(1 / a) - met
    }

    # Apply the function to each year
    Q_fu <- sapply(years, calculate_qfu, p, et, a)

      return(Q_fu)
}
