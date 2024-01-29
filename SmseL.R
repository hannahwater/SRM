SmseL <- function(pars, InputsModel, RunOptions, sr, qm){


  output<- RunModel_GR4J(InputsModel,RunOptions,Param=pars[1:4])
  yh=output$Qsim
  dates=InputsModel$DatesR
  n <- length(dates)

  # rating curve model (or surrogate streamflow model)
  yrc <- pars[5] * sr + pars[6]

  # errors
  e1 <- 1 - mean(yh, na.rm=TRUE) / qm
  e2 <- 1 - mean(yrc, na.rm=TRUE) / qm
  e3 <- scale(yrc, center=FALSE, scale=TRUE) - scale(yh, center=FALSE, scale=TRUE)
  e4 <- yrc - yh

  # variance
  vh <- pars[7]
  vrc <- pars[8]
  v1 <- 1 / (qm^2 * (n - 1)) * vh
  v2 <- 1 / (qm^2 * (n - 1)) * vrc
  v3 <- 1
  r <- cor(yh, yrc, use="complete.obs")
  v4 <- vh + vrc - 2 * r * sqrt(vh * vrc)
  n <- length(dates)
  # likelihood

  L1 <- -(1/2)*log(2*pi*v1)-(e1^2)/(2*v1)
  L2 <-  -(1/2)*log(2*pi*v2)-(e2^2)/(2*v2)
  L3 <-  -(n/2)*log(2*pi*v3) - sum(e3^2, na.rm=TRUE) / (2 * v3)
  L4 <-  -(n/2)*log(2*pi*v4) - sum(e4^2, na.rm=TRUE) / (2 * v4)
  L5 <- log(dexp(vh, rate=1000)) + log(dexp(vrc, rate=1))

  # maximizing the likelihood
  u <- -(L1 + L2 + L3 + L4 + L5)

  return(u)
}


