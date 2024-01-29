set_ftp <- function(year, month) {
  ftp_server <- "ftp.ifremer.fr"
  username <- "ext-catds-cpdc"
  password <- "catds2010"

  # Determine the base_dir based on the year and month
  if (as.integer(year) < 2021 || (as.integer(year) == 2021 && as.integer(month) <= 5)) {
    base_dir <- sprintf("/Land_products/GRIDDED/L3SM/RE07/MIR_CLF3ED/%s", year)
  } else {
    base_dir <- sprintf("/Land_products/GRIDDED/L3SM/OPER/MIR_CLF3ED/%s", year)
  }


  base_url <- sprintf("ftp://%s:%s@%s%s", URLencode(username), URLencode(password), ftp_server, base_dir)

    return(list(ftp_server = ftp_server, username = username, password = password, base_url = base_url))
}
