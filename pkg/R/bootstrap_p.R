bootstrap_p_helper = function (i, m, x_lower, xmins, pars, xmax) {
  ## Total sample size
  N = get_n(m)
  ## sum(x >= xmin)/N
  ntail_prop = get_ntail(m, prop=TRUE)
  
  ## Proportion to sample
  n1 = sum(runif(N) > ntail_prop) # less than xmin

  # q should be of length N
  q = c(sample(x_lower, n1, replace=TRUE), #less than xmin
        dist_rand(m, N - n1))
  
  m_cpy = m$getRefClass()$new(q)
  unlist(estimate_xmin(m_cpy, xmins=xmins, pars=pars, xmax=xmax))
}

#' @rdname estimate_xmin
#' @export
bootstrap_p = function (m, xmins=NULL, pars=NULL, xmax=1e5,
                        no_of_sims=100, threads=1, 
                        seed=NULL) {
  m_cpy = m$copy()
  gof_v = estimate_xmin(m_cpy, xmins=xmins, pars=pars, xmax=xmax)
  m_cpy$setXmin(gof_v)
  
  x = m_cpy$dat
  x_lower = x[x < m_cpy$xmin]
  
  ## Start clock and parallel boostrap
  start_time = Sys.time()
  cl = makeCluster(threads)  
  
  ## Set cluster seed
  clusterSetRNGStream(cl, seed)
  
  clusterExport(cl, c("dist_rand", "estimate_xmin", "get_ntail"))
  nof = parSapply(cl, 1:no_of_sims,
                  bootstrap_p_helper,  m_cpy, 
                  x_lower, xmins, pars, xmax)
  ## Stop clock and cluster
  end_time = Sys.time()
  stopCluster(cl)
  
  total_time = difftime(end_time, start_time, units="secs")*threads
  l = list(p=sum(nof[1,] >= gof_v[["KS"]])/no_of_sims, 
           gof = gof_v[["KS"]], 
           bootstraps = as.data.frame(t(nof)), 
           sim_time = total_time[[1]]/no_of_sims, 
           seed=seed, 
           package_version = packageVersion("poweRlaw"))
  class(l) = "bs_p_xmin"
  l
}
