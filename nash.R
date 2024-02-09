#' Calculate Nash-Sutcliffe Efficiency
#'
#' Computes the Nash-Sutcliffe efficiency to assess the accuracy of hydrological models.
#'
#' @param model Numeric vector of modelled values.
#' @param obs Numeric vector of observed values.
#'
#' @return Nash-Sutcliffe efficiency value.
#' @export


nash <- function(model, obs) {
  # Calculate NSE
  nse <- 1 - sum((model - obs)^2, na.rm = TRUE) / sum((obs - mean(obs, na.rm = TRUE))^2, na.rm = TRUE)
  return(nse)
}
