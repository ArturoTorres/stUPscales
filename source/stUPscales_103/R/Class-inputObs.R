inputObs <- setClass("inputObs", 
         slots = c(
           id = "numeric",
           plot = "numeric",
           delta = "list",
           observations = "list",
           lev2vol = "list",
           namePlot = "character",
           legendPosition = "list",
           var = "character"),
         prototype = list(
           id = 1,
           plot = 1,
           delta = NULL,
           observations = NULL,
           lev2vol = NULL,
           namePlot = "Validation plot",
           legendPosition = NULL,
           var = NULL)
         )