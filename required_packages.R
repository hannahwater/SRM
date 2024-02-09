required_packages <- function() {
  packages <- c('foreach', 'doParallel', 'RCurl', 'curl','sf','ncdf4','lubridate','airGR','airGRteaching','rtop','dplyr')
  for (pkg in packages) {
    if (!require(pkg, character.only = TRUE)) {
      install.packages(pkg)
      library(pkg, character.only = TRUE)
    }
  }
}
