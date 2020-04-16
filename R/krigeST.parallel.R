# Parallel imlementation for krigeST function
# 
# author: J.A. Torres-Matallana
# organization: Luxembourg Institute of Science and Technology (LIST), Belvaux, Luxembourg
#               Wagenigen University and Research Centre (WUR), Wageningen, The Netherlands 
# date: 22.11.2019 - 27.11.2019

#' Parallel imlementation for krigeST function
#'
#' Wrapper function for parallel computing for the krigeST function of package gstat.
#' 
#' @param cluster
#' @param cores
#' @param data_stfdf a STFDF object containing the input data (observations).
#' @param data_formula the formula to be passed to argument formula of krigeST function.
#' @param data_target a list of n STFDF objects which is the spatial split of the prediction grid as
#' provided by Split_grid function.
#' @param vgm_model the variogram model.
#' @param cores number of cores to use for parallel computation.
#'
#' @return the predicted STFDF object.
#'
#' @export Create.grid.split
#' 
#' @importFrom "parallel" "makeCluster"
#' @importFrom "parallel" "clusterExport"
#' @importFrom "parallel" "parLapply"
#' @importFrom "parallel" "stopCluster"
#' @importFrom "doParallel" "registerDoParallel"
#' @importFrom "gstat" "krigeST"
#' @importFrom "gstat" "vgmAreaST"

krigeST.parallel <- function(cluster, cores, data_stfdf, data_formula, 
                             data_target, vgm_model, p_nmax,
                             p_bufferNmax, p_computeVar, p_verbose,
                             p_modelList, p_ndiscrSpace){

  
  # clusterExport(cl = cl, varlist = c("krigeST", "vgmAreaST"), envir = .GlobalEnv)
  clusterEvalQ(cl = cluster, library(gstat))
  clusterExport(cl = cluster, varlist = c("data_stfdf", "data_formula", 
                                     "data_target", "vgm_model", "p_nmax", "p_bufferNmax", 
                                     "p_computeVar", "p_verbose", "p_modelList", 
                                     "p_ndiscrSpace"), envir = environment())
  
  # force(data_target)
  
  a2pST <- parLapply(cl = cluster, X = 1:cores, fun = function(x){
    krigeST(data = data_stfdf, 
            formula = data_formula, 
            newdata = data_target[[x]], 
            model = vgm_model, # point variogram
            nmax = p_nmax,
            bufferNmax = p_bufferNmax,
            computeVar = p_computeVar, 
            verbose = p_verbose,
            modelList = p_modelList, 
            ndiscrSpace = p_ndiscrSpace
  )})
  
  # # merging stfdf splitted
  # ids <- lapply(a2pST, function(x) lapply(x@sp@polygons, function(y) y@ID ))
  # ids <- length(do.call(rbind, ids))
  # ids_new <- matrix(1:ids, nrow = ids/length(a2pST))
  # 
  # a2pST_merged <- lapply(1:length(a2pST), function(x){
  #   for (i in 1:length(a2pST[[x]]@sp@polygons)){
  #     a2pST[[x]]@sp@polygons[[i]]@ID <- as.character(ids_new[i,x])
  #   }
  #   a2pST[[x]] 
  # })
  # 
  # a2pST <- do.call("rbind", a2pST_merged)
  
  return(a2pST)
  
  stopCluster(cl)
  closeAllConnections()
}