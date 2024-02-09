
#------------------------------------Data Download ---------------------------------------#
# Set the working directory to the desired path
setwd("set/your/path")

# Ensure all required packages are installed and loaded
required_packages()

# Define the local directory where data will be stored and processed
local_directory <-"set/your/directory"

# Define the time period for data download
start_year <- 2011
start_month <- 1
end_year <- 2011
end_month <- 12

# Download data from FTP server for the specified period
download_ftp(local_directory, start_year, start_month, end_year, end_month)

# Stop any remaining parallel clusters to free up resources
stopImplicitCluster()

# Unzip downloaded files in parallel for efficiency
unzip_files_in_parallel(local_directory, start_year, start_month, end_year, end_month)


#------------------------------------SR calculation ---------------------------------------#

# Collect pixel information based on catchment boundary from shapefiles
shapefile_path <- "path/to/shapefile.shp" # Provide the correct path to your shapefile
shapefile <- st_read(shapefile_path) # Read shapefile using 'sf' package
sample_directory <- paste0(local_directory, "/", start_year, "/", sprintf("%02d", as.integer(start_month))) # to collect SMOS grid information
pixel_info <- collect_pixels_catchment(sample_directory, shapefile) # Collect pixel info for the catchment

# Process data to calculate Dielectric Constant (DC) based on pixel information
DC <- process_DC(local_directory, start_year, start_month, end_year, end_month,
                 min(pixel_info$lon_num), max(pixel_info$lon_num),
                 min(pixel_info$lat_num), max(pixel_info$lat_num))

# Calculate Reflectivity (refs) from Dielectric Constant and calculate Surrgate River discharge (SR)
refs <- calculate_refs(DC)
sr <- calculate_SR(refs)

# Optionally, calculate average sr data across all pixels for each day
sr <- sr_averages(sr)
sr <- sr[1, 1, ] # Simplify array to vector if only interested in daily averages


# Collecting pixel information from a specific point (e.g., a gauging station)
lat_out <- # Latitude of the point
lon_out <- # Longitude of the point
pixel_info <- collect_pixels_outlet(sample_directory, lat_out, lon_out)
a <- pixel_info$lon_num
b <- pixel_info$lat_num

# Recalculate DC and SR for the specific outlet pixel
DC <- process_DC(local_directory, start_year, start_month, end_year, end_month, a, a, b, b)
refs <- calculate_refs(DC)
sr <- calculate_SR(refs)


#------------------------------------SRM calibration ---------------------------------------#
# Prepare data for hydrological model calibration
p <- # Precipitation data vector
et <- # Evapotranspiration data vector
DatesR <- seq.Date(as.Date("2011-01-01"), as.Date("2018-12-31"), by = "day")

# Prepare input data for the airGR hydrological model
InputSR <- data.frame(DatesR, p, et, sr)
DatesR <- as.POSIXct(InputSR$DatesR, tz = "UTC") # Convert dates to POSIXct format

# Prepare data and set calibration period for the model
prep <- PrepGR(ObsDF = InputSR, HydroModel = "GR4J", CemaNeige = FALSE)
CalPer <- c("2011-01-01", "2018-12-31") # Define the calibration period

# Calibrate the model using Surrogate River (SR) discharge data
Indicator <- 1.16 # Initial guess for the indicator value
# Optionally, use Budyko model to guess the indicator value
qfu <- calculate_qfu(start_year, end_year, start_month, end_month, p, et, alpha = 3)
Indicator <- mean(qfu, na.rm = TRUE) # Update Indicator based on qfu

# Perform model calibration and simulation
results_SR <- SRM(InputSR, Indicator, CalPer)


# Running the simulation using SR and plotting the results
SimPer <- c("2011-01-01","2018-12-31") # You can change the simulation period here
simmodSR <- SimGR(PrepGR = prep,Param=results_SR$par[1:4],EffCrit = "NSE",SimPer = SimPer)
QSRsim<-simmodSR$OutputsModel$Qsim


# Access simulated result (QSRsim) if you have observed river discharge (q)
nse=nash(QSRsim,q)

# Extract calibration parameters and objective function value
par(mfrow = c(3, 1)) # Set plot layout to 3 rows, 1 column
# Visualize SR, Rain, and Discharge data along with model simulation
plot(DatesR, sr, type = 'l', main = "Soil Reflectivity", ylim = rev(range(sr, na.rm = TRUE)))
plot(DatesR, p, type = 'l', main = "Precipitation")
plot(DatesR, q, type = 'l', main = "Observed Discharge")
lines(DatesR, QSRsim, type = 'l', col = 'red', main = "Simulated Discharge")
legend("topright", legend = c("Observed", "Simulated"), col = c("black", "red"), lty = c(1, 1))



