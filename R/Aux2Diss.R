# Auxiliary functions for subsetting and sppliting STFDF and grid objects
# 
# author: J.A. Torres-Matallana
# organization: Luxembourg Institute of Science and Technology (LIST), Belvaux, Luxembourg
#               Wagenigen University and Research Centre (WUR), Wageningen, The Netherlands 
# date: 12.11.2019 - 27.11.2019

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
#' @importFrom "sp" "coordinates"
#' 
#' @export Subset.stfdf

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


#' Split a STFDF object in space
#'
#' Given a STFDF object and a number integer of tiles , n, split
#' the STFDF object in the n tiles in space.
#'
#' @param stfdf the STFDF object to split.
#' @param n integer for splitting the STFDF.
#'
#' @return a list of n STFDF objects which is the spatial split from stfdf.
#' 
#' @export Split.stfdf

Split.stfdf <- function(stfdf, n){
  # coerce to SpatialPolygons
  spPoly  <- as(stfdf@sp, "SpatialPolygons")
  plot(spPoly)
  
  # split in n tiles
  nsplit <- lapply(1:n, function(i){
    # n <- 3
    ndata <- nrow(stfdf@data)
    ntime <- length(stfdf@time)
    k     <- ndata/ntime/n
    # stfdf[((k*i - k + 1):(k*i)),,1:ncol(stfdf@data)]
    
    # spliting by polygons
    # i <- 3
    Subset.stfdf(stfdf, spPoly[(k*i - k + 1):(k*i)]) 
  })
}


#' Create a spatial grid for kriging prediction
#'
#' Given a bounding box coordinates, a projection and a cellsize, this function creates a spatial grid for 
#' kriging prediction.
#'
#' @param bbox bunding box for the grid. A 2x2 matrix with first row equal to c(x_min, x_max) and
#' second row equal to  c(y_min, y_max).
#' @param proj geographical projection of the grid to be created (e.g. in metres).
#' @param cell.size cell size for the grid to be created in units consistent with the 'proj' argument.
#' Should be in metres.
#'
#' @return the grid as a SpatialPolygonsDataFrame object.
#'
#' @importFrom "raster" "raster"
#' @importFrom "raster" "extent"
#' @importFrom "raster" "rasterToPoints"
#' @importFrom "raster" "res"
#' @importFrom "sp" "gridded"

#' @export Create.grid

Create.grid <- function(bbox, proj, cell.size){
  ## The origin 
  x_ori <- bbox[1,1]
  y_ori <- bbox[2,1]
  
  ## Define how many cells for x and y axis
  x_cell500 <- ceiling((bbox[1,2] - bbox[1,1])/cell.size)
  y_cell500 <- ceiling((bbox[2,2] - bbox[2,1])/cell.size)
  
  ## Create the extent
  ext500 <- extent(x_ori, x_ori + (x_cell500 * cell.size), 
                   y_ori, y_ori + (y_cell500 * cell.size)) 
  
  ## Initialize a raster layer
  ras500 <- raster(ext500)
  
  ## Set the resolution to be
  raster::res(ras500) <- c(cell.size, cell.size)
  ras500[] <- 0
  
  ## Project the raster
  raster::projection(ras500) <- proj
  
  ## Visualise the raster
  # plot(ras500)
  
  ## Finally, to use the kriging functions from the package gstat, 
  ## we need to convert the raster to SpatialPixels.
  
  ## Convert to spatial pixel
  grid500          <- rasterToPoints(ras500, spatial = TRUE)
  sp::gridded(grid500) <- TRUE
  grid500          <- as(grid500, "SpatialPixels")
  grid500.sp       <- as(grid500, "SpatialPolygons")
  
  ## Extract polygon ID's
  pid500           <- sapply(slot(grid500.sp, "polygons"), 
                             function(x) slot(x, "ID"))
  
  # Try coercion
  grid500.sp       <- SpatialPolygonsDataFrame(Sr = grid500.sp, 
                                               data = data.frame(ID=1:length(grid500.sp), 
                                                                 row.names = pid500))
  
  return(grid500.sp)
}


#' Create a spatial grid spplitted for kriging prediction
#'
#' Given a a STFDF object splitted in a list coordinates, a projection and a cellsize, 
#' this function creates a splitted spatial grid for kriging prediction.
#'
#' @param stfdf_split a list of STFDF objects which is the spatial split from stfdf
#' as provided by the function Split.stfdf.
#' @param proj geographical projection of the grid to be created (e.g. in metres).
#' @param cell.size cell size for the grid to be created in units consistent with the 'proj' argument.
#' Should be in metres.
#'
#' @return the splitted grid as a list of SpatialPolygonsDataFrame objects.
#'
#' @export Create.grid.split
#' 
#' @importFrom "sp" "bbox"
#' @importFrom "raster" bind"

Create.grid.split <- function(stfdf_split, proj, cell.size){
  a <- lapply(stfdf_split, function(x) {
    as(x@sp, "SpatialPolygons")
  })
  
  grids <- lapply(a, function(y) {
    a_bbox <- lapply(y@polygons, bbox)
    a_grid <- lapply(a_bbox, function(x) Create.grid(bbox = x, 
                                                     proj = proj, 
                                                     cell.size = cell.size))
    a_grid <- lapply(a_grid, function(x) as(x,"SpatialPolygons"))
    
    # joining all polygons in just one SpatialPolygons via raster package
    a_grid_join <- do.call(bind, a_grid) 
  })
  
  return(grids)  
}


#' Split a grid (SpatialPolygonsDataFrame) in space
#'
#' Given a grid as  SpatialPolygonsDataFrame and a number integer of tiles , n, split
#' the grid in the n tiles in space.
#'
#' @param grid the SpatialPolygonsDataFrame object to split.
#' @param n integer for splitting grid.
#'
#' @return a list of n SpatialPolygonsDataFrame objects which is the spatial split from grid.
#' 
#' @export Split.grid

Split.grid <- function(grid, n){
  # coerce to SpatialPoints
  spPoints  <- as(grid, "SpatialPoints")

  # split in n tiles
  nsplit <- lapply(1:n, function(i){
    # n <- 68
    ndata <- nrow(grid@data)
    k     <- ndata/n
    
    # spliting by polygons
    # i <- 3
    split_grid <- spPoints[(k*i - k + 1):(k*i)]
  })
}
