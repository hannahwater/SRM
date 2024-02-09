#' Calculate Surrogate River discharge (SR)
#'
#' This function computes SR from a 3D array of reflectance
#'
#' @param refs A 3D array of reflectance
#'
#' @return A 3D array of SR with the same dimensions as the input array.
#' @export
#'
#' @examples

#' SR <- calculate_SR(refs)
calculate_SR <- function(refs) {
  # Internal function to calculate the new SR
  cal_srnew <- function(ref) {
    e <- 10^-1
    srnew <- (ref - min(ref, na.rm = TRUE)) / (max(ref, na.rm = TRUE) - ref + e)
    srnew[srnew < 0] <- 0  # Ensure srnew values are non-negative
    return(srnew)
  }

  # Initialize the SR array with the same dimensions as refs
  SR <- array(NA, dim = dim(refs))

  # Iterate over the first and second dimensions of refs to apply the cal_srnew function
  for (i in 1:dim(refs)[1]) {
    for (j in 1:dim(refs)[2]) {
      SR[i, j, ] <- cal_srnew(refs[i, j, ])
    }
  }

  return(SR)
}
