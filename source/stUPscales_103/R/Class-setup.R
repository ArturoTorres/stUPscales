setup <- setClass("setup", 
         slots = c(id        = "character",
                   nsim      = "numeric", 
                   seed      = "numeric",
                   mcCores   = "numeric",
                   ts.input  = "data.frame",
                   rng       = "list",
                   ar.model  = "list",
                   var.model = "list"),
                   
         prototype = list(id        = "MC_sim_1",
                          nsim      = 1,
                          seed      = 0.7010607,
                          mcCores   = 1,
                          ts.input  = NULL,
                          rng       = NULL,
                          ar.model  = list(NULL),
                          var.model = list(NULL))
         )