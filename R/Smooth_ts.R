# Kernel smoothing for time series
# 
# author: J.A. Torres-Matallana
# organization: Luxembourg Institute of Science and Technology (LIST), Belvaux, Luxembourg
# date: 01.10.2018 - 29.05.2019
# 
# ====================================================================================================
#' data.xts     : xts, time series to smooth
#' kernel       : tskernel, the smoothing kernel
#' sm.threshold : numeric, just apply the Kernel smoothing to values lower than sm.threshold
# ====================================================================================================
Smooth.ts <- function(data.xts, kernel = kernel("daniell", c(1, 1)), sm.threshold = 1){
  # Kernel smoothing of time series
  kd <- kernel("daniell", c(1, 1))
  plot(kd)
  
  # removing the initial and two ending values because will be lost after smoothing
  data1.xts <- data.xts[3:(nrow(data.xts)-2),] # TODO: generic kernel
  data.xts.sm  <- data1.xts
  
  for(i in 1:ncol(data.xts)){ # Kernel smoothing and NA filling
    data.xts.sm[,i] <- kernapply(as.numeric(coredata(na.locf(data.xts[,i]))), kd)  
  }
  
  # just apply the Kernel smoothing to values lower than sm.threshold
  for(i in 1:ncol(data.xts.sm)){ # Kernel smoothing and NA filling
    data.xts.sm[,i] <- ifelse(data.xts.sm[,i] < sm.threshold, data.xts.sm[,i], 
                                                 as.numeric(coredata(data1.xts[,i])))
    data.xts.sm[,i] <- ifelse(data.xts.sm[,i] < 0, 0, as.numeric(coredata(
                                                     data.xts.sm[,i])))
    data.xts.sm[,i] <- na.locf(data.xts.sm[,i])
  }
  
  return(data.xts.sm)
}
