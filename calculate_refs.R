#' Calculate reflectance from Dielectric Constants
#'
#' This function calculates reflectance based on dielectric constants (dc) using a specified formula.
#'
#' @param dc A 3D array of dielectric constants with dimensions corresponding to time, latitude, and longitude.
#'
#' @return A 3D array of reflectance with the first and second dimensions swapped compared to the input dc array.
#' @export
#'
#' @examples
#' dc <- array(runif(1000, min = -10, max = 100), dim = c(10, 10, 10))
#' refs <- calculate_refs(dc)
calculate_refs <- function(dc) {
  # Define constants for the calculations
  cosu <- cos(42.5 / 180 * pi)
  sinu <- sin(42.5 / 180 * pi)

  # Initialize the refs array
  refs <- array(NA, dim = c(dim(dc)[2], dim(dc)[3], dim(dc)[1]))

  # Process each element in dc to calculate reflectance indices
  for (i in 1:dim(dc)[2]) {
    for (j in 1:dim(dc)[3]) {
      # Extract the slice of dc for current i, j
      dl_ <- dc[, i, j]

      # Apply condition and calculate r1
      dl_[dl_ < 0 | dl_ > 80] <- NA  # Apply validity range for dl_
      r1 <- ((cosu - sqrt(abs(dl_) - sinu^2)) / (cosu + sqrt(abs(dl_) - sinu^2)))^2
      r1 <- abs(r1)  # Ensure r1 is non-negative
      r1[r1 == 1] <- NA  # Exclude values where r1 equals 1

      # Store the calculated r1 in refs
      refs[i, j, ] <- r1
    }
  }

  return(refs)
}
