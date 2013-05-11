disc_lnorm_tail_ll = function(x, pars, xmin) {
  if(is.vector(pars)) pars = t(as.matrix(pars))
  n = length(x)
  p = function(par){
    m_log = par[1]; sd_log = par[2]
    plnorm(x-0.5, m_log, sd_log, lower.tail=FALSE) - 
      plnorm(x+0.5, m_log, sd_log, lower.tail=FALSE)
  }
  
  
  joint_prob = colSums(log(apply(pars, 1, p)))
  prob_over = apply(pars, 1, function(i) 
    plnorm(xmin-0.5, i[1], i[2], 
           lower.tail=FALSE, log.p=TRUE))
  
  return(joint_prob - n*prob_over)
}



pois_tail_ll = function(x, rate, xmin) {
  n = length(x)
  joint_prob = colSums(sapply(rate, function(i) dpois(x, i, log=TRUE)))
  prob_over = sapply(rate, function(i) ppois(xmin-1, i, 
                                 lower.tail=FALSE, log.p=TRUE))
  return(joint_prob - n*prob_over)
}
