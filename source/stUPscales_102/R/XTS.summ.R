# summary statistics of time series
# author: J.A. Torres-Matallana
# organization: Luxembourg Institute of Science and technology (LIST)
#               Wagenigen University and Research Centre (WUR)   
# date: 22.09.2016 - 22.09.2016

XTS.summ <- function(xts, fileName){
  requireNamespace("moments")
  Mean  <- apply(xts, 2, mean, na.rm=T)
  Variance   <- apply(xts, 2, var, na.rm=T)
  
  acov  <- apply(xts, 2, acf, plot=F, type="covariance", na.action=na.pass, lag.max=10)
  Autocovariance <- sapply(acov, "[[", 1)  
  
  acorr  <- apply(xts, 2, acf, plot=F, type="correlation", na.action=na.pass, lag.max=10)
  Autocorrelation <- sapply(acorr, "[[", 1)  
  
  pdry <- function(x) {1-sum((x>10^-7)&(!is.na(x)))/length(x[!is.na(x)]) }  
  ProbabilityDry  <- apply(xts, 2, pdry)
  
  Skewness  <- apply(xts, 2, skewness, na.rm=T)
  
  Sum <- apply(xts, 2, sum, na.rm=T)
  
  xts.summ  <- data.frame(Mean, Variance, ProbabilityDry, Skewness, Sum,
                         t(Autocovariance), t(Autocorrelation))
  
  xts.summ1 <- apply(xts.summ, 2, mean)
  
  xts.summ  <- rbind.data.frame(xts.summ, Mean=xts.summ1)
  xts.summ <- t(xts.summ)
  
  row.names(xts.summ)[6:(6+10)] <- c("Autocovariance 0", "Autocovariance 1", "Autocovariance 2",
                                    "Autocovariance 3", "Autocovariance 4", "Autocovariance 5",
                                    "Autocovariance 6", "Autocovariance 7", "Autocovariance 8",
                                    "Autocovariance 9", "Autocovariance 10")
  row.names(xts.summ)[17:(17+10)] <- c("Autocorrelation 0", "Autocorrelation 1", "Autocorrelation 2",
                                      "Autocorrelation 3", "Autocorrelation 4", "Autocorrelation 5",
                                      "Autocorrelation 6", "Autocorrelation 7", "Autocorrelation 8",
                                      "Autocorrelation 9", "Autocorrelation 10")
  
  #assign(fileName, xts.summ)
  save(xts.summ, file=paste(fileName, ".RData", sep=""))
  
  return(xts.summ)
}