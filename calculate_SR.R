calculate_SR <- function(refs) {

  cal_srnew <- function(ref) {
    e <- 10^-1
    srnew <- (ref - min(ref, na.rm = TRUE)) / (max(ref, na.rm = TRUE) - ref + e)
    srnew[srnew < 0] <- 0
    return(srnew)
  }

  # Initialize the SR array with the same dimensions as refs
  SR <- array(NA, dim = dim(refs))

  # Iterate over the dimensions of refs and apply calculate_srnew
  for (i in 1:dim(refs)[1]) {
    for (j in 1:dim(refs)[2]) {
      SR[i, j, ] <- cal_srnew(refs[i, j, ])
    }
  }

  return(SR)
}
