bootstrap_p_helper = function (i, m, x_lower, xmins, pars, xmax, distance) {
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
  
  est = estimate_xmin(m_cpy, xmins=xmins, pars=pars, xmax=xmax, distance=distance)
  ## Remove the character now, since we will change to data frame.
  est["distance"] = NULL
  unlist(est)
}

#' @rdname estimate_xmin
#' @export
bootstrap_p = function (m, xmins=NULL, pars=NULL, xmax=1e5,
                        no_of_sims=100, threads=1, 
                        seed=NULL, distance="ks") {
  m_cpy = m$copy()
  time = timer()
  time$start()
  gof_v = estimate_xmin(m_cpy, xmins=xmins, pars=pars, xmax=xmax, distance=distance)
  time$stop()
  if(is.na(gof_v$gof)) {
    stop("Unable to estimate initial xmin using estimate_xmin, so we can't bootstrap.")
  }

  if(min(m_cpy$dat) > xmax) {
    stop("The smallest value in your data set is larger than xmax. The xmax
         parameter is the upper limit of the xmin search space.")
  }
  
  if(max(m_cpy$dat) > xmax) {
    message("Some of your data is larger than xmax. The xmax parameter is
            the upper bound of the xmin search space. You could try increasing
            it. If the estimated values are below xmax, it's probably OK not to 
            worry about this.")
  }
  
  message("Expected total run time for ", no_of_sims, 
          " sims, using ", threads, " threads is ", 
          signif(time$get()*no_of_sims/threads, 3), " seconds." )
  m_cpy$setXmin(gof_v)
  
  x = m_cpy$dat
  x_lower = x[x < m_cpy$xmin]
  
  ## Start clock and parallel boostrap
  time$start()
  cl = makeCluster(threads)  
  on.exit(stopCluster(cl))
  
  ## Set cluster seed
  clusterSetRNGStream(cl, seed)
  
  clusterExport(cl, c("dist_rand", "estimate_xmin"))
  nof = parSapply(cl, 1:no_of_sims,
                  bootstrap_p_helper,  m_cpy, 
                  x_lower, xmins, pars, xmax, distance)
  ## Stop clock and cluster
  total_time = time$get(stop=TRUE)*threads
  
  if(sum(is.na(nof[1,])) > 1) {
    message(sum(is.na(nof[1,])), " bootstraps generated NA values. 
            These have been removed when calculating the associated p-value.")
  }

  bootstraps = as.data.frame(t(nof))
  l = list(p=sum(nof[1,] >= gof_v[["gof"]], na.rm=TRUE)/no_of_sims, 
           gof = gof_v[["gof"]], 
           bootstraps = bootstraps, 
           sim_time = total_time[[1]]/no_of_sims, 
           seed=seed, 
           package_version = packageVersion("poweRlaw"), 
           distance=distance)
  class(l) = "bs_p_xmin"
  l
}
