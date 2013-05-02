

lnorm.tail.disc.loglike = function(x, meanlog, sdlog, xmin) {
  n = length(x)
  p = plnorm(x-0.5,meanlog,sdlog,lower.tail=FALSE) - 
    plnorm(x+0.5,meanlog,sdlog,lower.tail=FALSE)
  joint_prob = sum(log(p))
  
  ProbOverThreshold = plnorm(xmin-0.5,meanlog, sdlog, lower.tail=FALSE,
                              log.p=TRUE)
  return(joint_prob - n*ProbOverThreshold)
}

pois.tail.loglike = function(x, rate, xmin) {
  n = length(x)
  JointProb = sum(dpois(x, rate,log=TRUE))
  ProbOverThreshold = ppois(xmin-1, rate, lower.tail=FALSE, log.p=TRUE)
  return(JointProb - n*ProbOverThreshold)
}





