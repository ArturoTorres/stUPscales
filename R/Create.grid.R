# Create a spatial grid for kriging prediction
# 
# author: J.A. Torres-Matallana
# organization: Luxembourg Institute of Science and Technology (LIST), Belvaux, Luxembourg
#               Wagenigen University and Research Centre (WUR), Wageningen, The Netherlands 
# date: 12.11.2019 - 12.11.2019

#' Create a spatial grid for kriging prediction
#'
#' Given a bounding box coordinates and a cellsize, this function creates a spatial grid for 
#' kriging prediction
#'
#' @param bbox bunding box for the grid. A 2x2 matrix with first row equal to c(x_min, x_max) and
#' second row equal to  c(y_min, y_max).
#' @param proj spatial projection of the grid to be created in metres.
#' @param cell.size cell size for the grid to be created in units consistent with the 'proj' argument.
#' Should be in metres.
#'
#' @return 
#'
#' @importFrom "rgdal" "readOGR"
#' @importFrom "raster" "extent"

#' @export Create.grid

Create.grid <- function(bbox, proj, cell.size){
  ## The origin 
  x_ori <- bbox[1,1]
  y_ori <- bbox[2,1]
  
  ## Define how many cells for x and y axis
  x_cell500 <- ceiling((bbox[1,2] - bbox[1,1])/cell.size)
  y_cell500 <- ceiling((bbox[2,2] - bbox[2,1])/cell.size)
  
  ## Create the extent
  ext500 <- extent(x_ori, x_ori + (x_cell500 * cell.size), y_ori, y_ori + (y_cell500 * cell.size)) 
  
  ## Initialize a raster layer
  ras500 <- raster(ext500)
  
  ## Set the resolution to be
  res(ras500) <- c(cell.size, cell.size)
  ras500[] <- 0
  
  ## Project the raster
  projection(ras500) <- proj
  
  ## Visualise the raster
  # plot(ras500)
  
  ## Finally, to use the kriging functions from the package gstat, 
  ## we need to convert the raster to SpatialPixels.
  
  ## Convert to spatial pixel
  grid500          <- rasterToPoints(ras500, spatial = TRUE)
  gridded(grid500) <- TRUE
  grid500          <- as(grid500, "SpatialPixels")
  grid500.sp       <- as(grid500, "SpatialPolygons")
  
  ## Extract polygon ID's
  pid500           <- sapply(slot(grid500.sp, "polygons"), function(x) slot(x, "ID"))
  
  # Try coersion
  grid500.sp       <- SpatialPolygonsDataFrame(Sr = grid500.sp, 
                                               data = data.frame(ID=1:length(grid500.sp), row.names = pid500))
  
  return(grid500.sp)
}
