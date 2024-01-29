nash <- function(model, obs) {
  # Calculate NSE
  nse <- 1 - sum((model - obs)^2, na.rm = TRUE) / sum((obs - mean(obs, na.rm = TRUE))^2, na.rm = TRUE)
  return(nse)
}
