# Check regulatiry of time series in folder
# 
# author: J.A. Torres-Matallana
# organization: Luxembourg Institute of Science and Technology (LIST), Belvaux, Luxembourg
# date: 01.10.2018 - 29.05.2019
# 
# ====================================================================================================
#' path          : character, path to folder which contains the imagery
#' sep           : character, separation character
#' pattern       : character, extension of the files to check
#' format        : character, time format e.g. "%y%m%d%H%M"
# ====================================================================================================
IsReg.files <- function(path, sep = "-", pattern=".tif", format = "%y%m%d%H%M"){
  # extract time series
  fileNames    <- list.files(path, pattern = pattern, full.names = FALSE)
  times        <- unlist(lapply(fileNames, function(x) strsplit(x, sep)[[1]][3]))
  times        <- lapply(times, function(x) as.POSIXct(x, format = "%y%m%d%H%M"))
  times        <- data.frame(times = do.call(c, times), data = NA)
  
  ## check if times are regular
  check <- IsReg.ts(times, format = format)
  
  ## return
  return(check)
}
