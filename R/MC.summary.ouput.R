# Global summary of Monte Carlo simulation per output variable
# 
# author: J.A. Torres-Matallana
# organization: Luxembourg Institute of Science and Technology (LIST), Belvaux, Luxembourg
#               Wagenigen University and Research Centre (WUR), Wageningen, The Netherlands 
# date: 04.06.2019 - 24.06.2019

library(DescTools)

#' Sample bias
Bias <- function(x, true){
  bias <- (mean(x)) - true
  
  return(bias)
  }

#' Sample coefficient of variation
CV <- function(x){sd(x)/mean(x)}

Skewness <- function(x){mean((x-mean(x))^3)/(mean((x-mean(x))^2)^(1.5))}

# # from Boos et al (2015)
# Kurtosis <- function(x){mean((x-mean(x))^4)/(mean((x-mean(x))^2)^2)}

# from Cramer (1946)
Kurtosis <- function(x){mean((x-mean(x))^4)/(mean((x-mean(x))^2)^2)} - 3

#' bias-corrected estimator of Bao, Y. (2009). 
#' "Finite-sample moments of the coefficient of variation". 
#' Econom. Theory, 25, 291–297.
# CV_BC <- function(x, n){
#   ((CV(x))^3)/n - (CV(x))/(4*n) -  ((CV(x))^2*Skewness(x))/(2*n) - (CV(x)*(Kurtosis(x) - 3))/(8*n)
# } 

CV_BC <- function(x, n){
  ((CV(x))^3)/n - (CV(x))/(4*n) -  ((CV(x))^2*Skewness(x))/(2*n) - (CV(x)*(Kurtosis(x)))/(8*n)
} 


#' For a variance estimator for either, we use estimates plugged 
#' into the asymptotic variance expression which may be found in 
#' Serfling, R.J. (1980, p. 137). Approximation Theorems of 
#' Mathematical Statistics. New York: Wiley.
AVarCV <- function(x, n){
  ((CV(x))^4)/n - ((CV(x))^3)*Skewness(x)/n + ((CV(x))^2)*(Kurtosis(x) - 1)/(4*n)  ## to check: Kurtosis(x) - 1 
}

#' Jackknife variance estimate adapted from the Appendix of 
#' Efron, B. and Tibshirani, R. (1993). 
#' An Introduction to the Bootstrap. Chapman & Hall Ltd.
Jack.var <- function(x, theta, ...){
  n <- length(x)
  u <- rep(0, n)
  for(i in 1:n) {
    u[i] <- theta(x[ -i], ...)  # leave-1-out estimators
  }
  jack.var <- ((n - 1)/n) * sum((u - mean(u))^2)
  
  return(jack.var)
}

#' Jackknife estandard error modified from the Appendix of 
#' Efron, B. and Tibshirani, R. (1993). 
#' An Introduction to the Bootstrap. Chapman & Hall Ltd.
Jack.se <- function(x, theta, ...){
  call <- match.call()
  n <- length(x)
  u <- rep(0, n)
  for(i in 1:n) {u[i] <- theta(x[ - i], ...)}
  
  jack.se <- sqrt(((n - 1)/n) * sum((u - mean(u))^2))
  
  return(jack.se)
}

#' Bootstrap
                    
Boot.var <- function(x, B, theta, ...){
  n         <- length(x)
  bootsam   <- matrix(sample(x, size = n*B, replace=TRUE), nrow = B, ncol = n)
  thetastar <- apply(bootsam, 1, theta, ...)
  boot.var  <- var(thetastar)
  
  return(boot.var)
}

#' Compute estimator and standard errors by bootstrap re-sampling
MC.se.boot <- function(X, theta, B, seed, ...){
  est <- theta(x = X, ...)
  
  #' Bootstrap
  var.J1 <- Boot.var(x = X, B = B, theta = theta, ...)
  
  #' standard errors
  se <- var.J1^.5
  
  #' output
  col <- data.frame(est, se)
  
  return(estimate = col)
}

#' Compute estimator and standard errors by bootstrap re-sampling
MC.statistic <- function(mc, theta, trim, zero = 0, centered = FALSE, transform.n = 0, ...){
  # estimate
  x <- mc[1,]
  length(x)
  est <- apply(mc, 1, function(x) {
    # filter zero values
    x.nonzero <- x[x > zero]
    
    # trim
    x.trimmed <- Trim(x = sort(x.nonzero), trim = trim)
    n         <- length(x.trimmed)
    
    # # centered (substract mean)
    if(centered) x.trimmed <- x.trimmed - mean(x.trimmed)
    # if(transform.n != 0) x.trimmed <- (log(x.trimmed))^transform.n
    if(transform.n != 0) x.trimmed <- x.trimmed^transform.n
    
    # estimate
    if(identical(attributes(theta), attributes(CV_BC))){
      est <- data.frame(est = theta(x.trimmed, n = n, ...), n = n)
    }else est <- data.frame(est = theta(x.trimmed, ...), n = n)
    
    return(est)
    }) 
  
  do.call(rbind, est)
}



# ====================================================================================================
#' summ     : list, contains the summary statistics of the Monte Carlo simulation per 
#'            output variable, as the output of the MC.analysis or MC.analysis_generic
#'            functions.
#' variance : data.frame, contains the summary statistics of the variance of the Monte
#'            Carlo simulation per output variable, as the output of the MC.analysis or 
#'            MC.analysis_generic functions.
# ====================================================================================================


# MC.summary.global <- function(summ, variance){
#   
#   # require(rgdal)      # readGDAL
#   
#   summary(summ[[1]])
#   
#   
#   
#   
#   return(imgSTFDF)
# }
