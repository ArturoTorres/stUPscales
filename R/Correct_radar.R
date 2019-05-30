# Correction of precipitation radar imagery upon calibrated data
# 
# author: J.A. Torres-Matallana
# organization: Luxembourg Institute of Science and Technology (LIST), Belvaux, Luxembourg
# date: 01.10.2018 - 29.05.2019
# 
# author: B. Graeler
# organization: 52* North Initiative for Geospatial Open Source Software GmbH, Muenster, Germany
# date: 01.10.2018 - 07.05.2019
# 
# ====================================================================================================
#' path.calibrated     : character, path to folder which contains the calibrated imagery in .tif format
#' path.non.calibrated : character, path to folder which contains the non-calibrated imagery in .tif format
#' time.ini.calib      : character, initial time for calibration e.g. "1112160055"
#' format              : character, initial time format e.g. "%y%m%d%H%M"
#' n                   : numeric, number of images to process including time.ini.calib
#' NAcutoff <- 250     : numeric, less than 10 pixels per image
# ====================================================================================================
Correct.radar <- function(path.calibrated, path.non.calibrated, time.ini.calib,
                              format = "%y%m%d%H%M", n, NAcutoff = 250){
  
  # require(rgdal)      # readGDAL
  # require(hexbin)     # hexbin
  # require(xts)        # xts
  # require(spacetime)  # STFDF, stplot
  
  filesInRepoCalibrated    <- list.files(path.calibrated, pattern = ".tif", full.names = TRUE)
  filesInRepoNonCalibrated <- list.files(path.non.calibrated, pattern = ".tif", full.names = TRUE)
  
  imgCalibrated <- readGDAL(filesInRepoCalibrated[2])
  imgCalibrated$band1[imgCalibrated$band1 > NAcutoff] <- NA
  # plot(imgCalibrated)

  imgNonCalibrated <- readGDAL(filesInRepoNonCalibrated[2])
  imgNonCalibrated$band1[imgNonCalibrated$band1 > NAcutoff] <- NA
  # plot(imgNonCalibrated)
  
  ## check if times are regular
  timesCalibrated <- IsReg.files(path = path.calibrated, sep = "-", pattern = ".tif", format = "%y%m%d%H%M")
  stopifnot(timesCalibrated[[1]] == "_TSregular")
  
  timesNonCalibrated <- IsReg.files(path = path.non.calibrated, sep = "-", pattern = ".tif", format = "%y%m%d%H%M")
  stopifnot(timesNonCalibrated[[1]] == "_TSregular")
  
  ## aggregate to 10 min: as sum/average/sum(log())            # TODO: generic aggregation
  agg      <- 2 # numeric, integer, the time steps to aggregate # TODO: generic aggregation
  imgVals  <- NULL
  img      <- NULL
  id.ini   <- which(index(timesNonCalibrated[[2]]) == as.POSIXct(time.ini.calib, format = format))/agg
  id.end   <- id.ini + n - 1
  time.end <- index(timesNonCalibrated[[2]][id.end*agg])
  par(mfrow=c(2,3))
  for(tenMinSlice in id.ini:id.end) {
    imgNC01 <- readGDAL(filesInRepoNonCalibrated[tenMinSlice*agg-1])
    imgNC01$band1[imgNC01$band1 > NAcutoff] <- NA
    hist(imgNC01$band1)

    imgNC02 <- readGDAL(filesInRepoNonCalibrated[tenMinSlice*agg])
    imgNC02$band1[imgNC02$band1 > NAcutoff] <- NA
    hist(imgNC02$band1)

    imgVals <- cbind(imgVals, (imgNC01$band1 + imgNC02$band1)) # 1/100 mm/10 min
    
    img <- rbind(img, filesInRepoNonCalibrated[tenMinSlice*agg])
  }

  # convert unit to 1/10 mm/h   # TODO: generic aggregation
  imgVals <- imgVals/10*6

  ## disaggreagte 1h calibrated to 10 calibrated proxies   # TODO: generic disaggregation
  imgSum <- apply(imgVals, 1, mean)

  dev.off()
  # plot(imgSum, imgCalibrated$band1)
  plot(hexbin(imgSum, imgCalibrated$band1))

  imgRate <- imgCalibrated$band1/imgSum
  
  imgCor <- imgVals*imgRate
  boolsZeroNotNA <- (imgSum == 0 & imgCalibrated$band1 > 0) | (is.na(imgVals) & !is.na(imgCalibrated$band1))
  boolsZeroNotNA[is.na(boolsZeroNotNA)] <- FALSE
  apply(boolsZeroNotNA,2,sum)

  imgCor[boolsZeroNotNA] <- imgCalibrated$band1[boolsZeroNotNA]
  
  imgCorSum <- apply(imgCor, 1, mean)
  # plot(imgCorSum, imgCalibrated$band1)

  imgSTFDF <- STFDF(sp = geometry(imgCalibrated), 
                    time = xts(1:n, order.by = seq.POSIXt(from = as.POSIXct(time.ini.calib, 
                                                                            format = "%y%m%d%H%M"),   # TODO: check this generic indexing
                                                   to = time.end, length.out = n)),
                    data = data.frame(as.vector(imgCor)))
  
  return(imgSTFDF)
}
