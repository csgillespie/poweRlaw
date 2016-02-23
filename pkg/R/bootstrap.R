bootstrap_helper = function (i, m, xmins, pars, xmax) {
  x = sample(m$dat, length(m$dat), replace=TRUE)
  
  m_cpy = m$getRefClass()$new(x)
  unlist(estimate_xmin(m_cpy, xmins=xmins, pars=pars, xmax=xmax))
}

#' @rdname estimate_xmin
#' @param seed default \code{NULL}. An integer to be supplied to \code{set.seed}, or \code{NULL}
#' not to set reproducible seeds. This argument is passed \code{clusterSetRNGStream}.
#' @importFrom parallel clusterSetRNGStream
#' @importFrom utils packageVersion
#' @export
bootstrap = function (m, xmins=NULL, pars=NULL, xmax=1e5,
                      no_of_sims=100, threads=1, 
                      seed=NULL) {
  m_cpy = m$copy()
  gof_v = estimate_xmin(m_cpy, xmins=xmins, pars=pars, xmax=xmax)
  
  m_cpy$setXmin(gof_v)
  x = m_cpy$dat
  N = length(x)
  
  ## Start clock and parallel boostrap
  start_time = Sys.time()
  cl = makeCluster(threads)
  
  ## Set cluster seed
  clusterSetRNGStream(cl, seed)
  
  clusterExport(cl, c("estimate_xmin"))
  nof = parSapply(cl, 1:no_of_sims,
                  bootstrap_helper,  m_cpy, 
                  xmins, pars, xmax)
  
  ## Stop clock and cluster
  end_time = Sys.time()
  stopCluster(cl)
  
  total_time = difftime(end_time, start_time, units="secs")*threads
  l = list(gof = gof_v[["KS"]], 
           bootstraps = as.data.frame(t(nof)), 
           sim_time = total_time[[1]]/no_of_sims, 
           seed=seed,
           package_version = packageVersion("poweRlaw"))
  class(l) = "bs_xmin"
  l
}


