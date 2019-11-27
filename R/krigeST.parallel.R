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
#' @importFrom "doParallel" "registerDoParallel"
#' @importFrom "gstat" "krigeST"
#' @importFrom "gstat" "vgmAreaST"

krigeST.parallel <- function(data_stfdf, data_formula, data_target, vgm_model, cores, ...){
  cl <- makeCluster(cores)
  registerDoParallel(cl, cores=cores)
  
  clusterExport(cl = cl, varlist = c("krigeST", "vgmAreaST"), envir = .GlobalEnv)
  # clusterEvalQ(cl, library(gstat))
  clusterExport(cl = cl, varlist = c("data_stfdf", "data_formula", "data_target", "vgm_model"), envir = environment())
  
  force(data_target)
  
  a2pST <- parLapply(cl = cl, X = 1:cores, fun = function(x){
    krigeST(formula = data_formula, 
            # data = data_split[[x]], 
            data = data_stfdf, 
            newdata = data_target[[x]], 
            # nmax = 120, 
            bufferNmax = 1,
            computeVar = FALSE, 
            verbose = TRUE,
            modelList = vgmAreaST, ndiscrSpace = 9, 
            model = vgm_model # point variogram
    )
  })
  
  return(a2pST)
}