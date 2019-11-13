# Create a subset from a STFDF object in space
# 
# author: J.A. Torres-Matallana
# organization: Luxembourg Institute of Science and Technology (LIST), Belvaux, Luxembourg
#               Wagenigen University and Research Centre (WUR), Wageningen, The Netherlands 
# date: 12.11.2019 - 12.11.2019

#' Create a subset from a STFDF object
#'
#' Given a STFDF object and a SpatialPolygonsDataFrame, create a subset as the intersection between
#' the STFDF and the SpatialPolygonsDataFrame in space.
#'
#' @param stfdf the STFDF object to subset.
#' @param sp.polygons the SpatialPolygonsDataFrame for subsetting the STFDF.
#'
#' @return a STFDF object which is the requested spatial subset from stfdf.
#'
#' @importFrom "rgdal" "readOGR"
#' @importFrom "raster" "extent"

#' @export Create.grid

Subset.stfdf <- function(stfdf, sp.polygons){
  if(identical(stfdf@sp@proj4string, sp.polygons@proj4string) == FALSE) 
    stop("geographical projections for stfdf and sp.polygons must be the same")
  
  idx <- 
    coordinates(stfdf@sp)[,1] > sp.polygons@bbox[1,1] &
    coordinates(stfdf@sp)[,1] < sp.polygons@bbox[1,2] &
    coordinates(stfdf@sp)[,2] > sp.polygons@bbox[2,1] & 
    coordinates(stfdf@sp)[,2] < sp.polygons@bbox[2,2]
  
  my.subset <- stfdf[idx,,1:ncol(stfdf@data)]  # TODO: check if all data columns are subsetted
  
  my.subset@sp@bbox <- sp.polygons@bbox
  
  return(my.subset)
}
