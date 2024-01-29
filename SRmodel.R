SRmodel <- function(InputSR, Indicator, CalPer) {

  caldata <- dataPer(InputSR, CalPer)
  InputsModel<-CreateInputsModel(RunModel_GR4J, caldata$dates,caldata$p$p,TRUE,caldata$pet$pet)
  sr=caldata$sr
  date_diff <- as.Date(CalPer[2]) - as.Date(CalPer[1])
  Ndays<- (1:(as.integer(date_diff)+1))
  suppressWarnings(suppressMessages({
    RunOptions <- CreateRunOptions(RunModel_GR4J, InputsModel = InputsModel,
                                   IndPeriod_WarmUp = NULL, IndPeriod_Run = Ndays)
  }))

  qm <- Indicator
  a0 <-max(0.001, qm / (mean(sr, na.rm = TRUE) - min(sr, na.rm = TRUE)))
  a1 <- -min(sr, na.rm = TRUE) * a0

  upper<-c(1200, 3,  300,  2.9,  10,   10, 1,  100);
  lower<-c(1,   -5,  20,  1.1, 0,  -10,  1E-5, 1E-2);
  x0<-c(350,  0,  90,  1.7, a0,   a1, 1E-3,  1);

  objSR <- function(pars) {
    return(SmseL(pars, InputsModel, RunOptions, sr, qm))
  }

  sink("nul")
  result_SR <- sceua(objSR, pars = x0, lower = lower, upper = upper, mings = 8, iniflg = 0)
  sink()

  return(result_SR)
}
