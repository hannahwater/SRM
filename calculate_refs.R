calculate_refs <- function(dc) {
  # Calculate cos and sin values
  cosu <- cos(42.5 / 180 * pi)
  sinu <- sin(42.5 / 180 * pi)

  # Initialize the refs array with the same dimensions as dc but with the first and second dimensions swapped
  refs <- array(NA, dim = c(dim(dc)[2], dim(dc)[3], dim(dc)[1]))

  # Iterate over the 2nd and 3rd dimensions of dc
  for (i in 1:dim(dc)[2]) {
    for (j in 1:dim(dc)[3]) {
      dl_ <- dc[, i, j]

      # Apply the condition and calculate r1
      dl_[dl_ < 0 | dl_ > 80] <- NA
      r1 <- ((cosu - sqrt(abs(dl_) - sinu^2)) / (cosu + sqrt(abs(dl_) - sinu^2)))^2
      r1 <- abs(r1)
      r1[r1 == 1] <- NA

      # Assign the result to refs
      refs[i, j, ] <- r1
    }
  }

  return(refs)
}
