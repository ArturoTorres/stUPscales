# Creation of STFDF object from xts and SpatialPointsDataFrame objects
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
#' data.xts   : xts, time series
#' point      : SpatialPointsDataFrame, point geospatial domain of data.xts
#' 
# ====================================================================================================
xts2STFDF <- function(data.xts, point){
  
  # require(spacetime)  # STFDF
  
  # check the ordering of the spatialPointsDataFrame and the xts series
  stopifnot(all(diff(match(point$name, colnames(as.data.frame(data.xts))))==1))
  
  # data.frame for data
  df <- data.frame(data = as.vector(t(as.matrix(as.data.frame(data.xts)))))
  
  # creating STFDF object
  stfdf <- STFDF(sp = point, time = index(data.xts), data = df)
  
  # return
  return(stfdf)
}
