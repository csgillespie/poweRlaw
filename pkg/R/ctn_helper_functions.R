con_pl_ll = function(x, pars, xmin) {
  n = length(x)
  joint_prob = colSums(sapply(pars, 
                              function(i) dplcon(x, xmin, i, log=TRUE)))
  #   ##Normalise due to xmax
  prob_over = 0
  #   if(!is.null(xmax))
  #       prob_over = sapply(pars, function(i) 
  #         log(ppldis(xmax, i, lower.tail=TRUE)))
  #   
  
  return(joint_prob - n*prob_over)
}









conlnorm_tail_ll = function(x, pars, xmin) {
  if(is.vector(pars)) pars = t(as.matrix(pars))
  n = length(x)
  joint_prob = colSums(apply(pars, 1, 
                             function(i) dlnorm(x, i[1], i[2], log=TRUE)))
  
  #ll = sum(dlnorm(x, m_log, sd_log, log=TRUE))
  prob_over = apply(pars, 1, function(i) 
    plnorm(xmin, i[1], i[2], log.p=TRUE, lower.tail=FALSE))
  joint_prob - n*prob_over
  
}


conexp_tail_ll = function(x, rate, xmin) {
  n = length(x)
  joint_prob = colSums(sapply(rate, function(i) dexp(x, i, log=TRUE)))
  prob_over = sapply(rate, function(i) pexp(xmin, i, 
                                             lower.tail=FALSE, log.p=TRUE))
  return(joint_prob - n*prob_over)
}








# conlnorm_tail_ll(x, c(1,1), 1)
# conexp_tail_ll(x, 1, 1)
# 
# pexp(1, 1, lower.tail=F, log.p=T)
# log(1 - pexp(1, 1))
# 
# log(dexp(x, 1)/(1 - pexp(1, 1)))