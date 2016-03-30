bootstrap_helper = function (i, m, xmins, pars, xmax, distance) {
  x = sample(m$dat, length(m$dat), replace=TRUE)
  
  m_cpy = m$getRefClass()$new(x)
  unlist(estimate_xmin(m_cpy, xmins=xmins, pars=pars, xmax=xmax, distance=distance))
}

#' @rdname estimate_xmin
#' @param seed default \code{NULL}. An integer to be supplied to \code{set.seed}, or \code{NULL}
#' not to set reproducible seeds. This argument is passed \code{clusterSetRNGStream}.
#' @importFrom parallel clusterSetRNGStream
#' @importFrom utils packageVersion
#' @export
bootstrap = function (m, xmins=NULL, pars=NULL, xmax=1e5,
                      no_of_sims=100, threads=1, 
                      seed=NULL, distance="ks") {
  m_cpy = m$copy()
  gof_v = estimate_xmin(m_cpy, xmins=xmins, pars=pars, xmax=xmax, distance=distance)
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
  
  
  
  m_cpy$setXmin(gof_v)
  # x = m_cpy$dat
  # N = length(x)
  
  ## Start clock and parallel boostrap
  cl = parallel::makeCluster(threads)
  start_time = Sys.time()
  
  ## Set cluster seed
  parallel::clusterSetRNGStream(cl, seed)
  
  parallel::clusterExport(cl, c("estimate_xmin"))
  nof = parallel::parSapply(cl, 1:no_of_sims,
                  bootstrap_helper,  m_cpy, 
                  xmins, pars, xmax, distance)
  
  ## Stop clock and cluster
  end_time = Sys.time()
  parallel::stopCluster(cl)
  
  total_time = difftime(end_time, start_time, units="secs")*threads
  bootstraps = as.data.frame(t(nof))
  bootstraps = bootstraps[, colnames(bootstraps) != "distance"]
  l = list(gof = gof_v[["gof"]], 
           bootstraps=bootstraps, 
           sim_time = total_time[[1]]/no_of_sims, 
           seed=seed,
           package_version = packageVersion("poweRlaw"), 
           distance=distance)
  class(l) = "bs_xmin"
  l
}


