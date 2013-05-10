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

