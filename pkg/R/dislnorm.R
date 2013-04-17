# Discretized probability mass function
# Input: Data vector, meanlog, sdlog, log parameter
# Output: Vector of (log) probabilities
# Taken from R code from Cosma Shalizi
dlnorm.disc <- function(x, meanlog, sdlog, log=FALSE) {
  # When x is very large, plnorm(x, lower.tail=TRUE) gets returned as 1,
  # but plnorm(x,lower.tail=FALSE) gets returned as a small but non-zero
  # number, so we should get fewer zeroes this way
  p = plnorm(x-0.5,meanlog,sdlog,lower.tail=FALSE) - 
    plnorm(x+0.5,meanlog,sdlog,lower.tail=FALSE)
  if (log) {
    return(log(p))
  } else {
    return(p)
  }
}


lnorm.tail.disc.loglike <- function(x, meanlog, sdlog, xmin) {
  n = length(x)
  JointProb <- sum(dlnorm.disc(x, meanlog, sdlog,log=TRUE))
  ProbOverThreshold <- plnorm(xmin-0.5,meanlog, sdlog, lower.tail=FALSE,
                              log.p=TRUE)
  return(JointProb - n*ProbOverThreshold)
}

