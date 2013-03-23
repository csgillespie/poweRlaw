bootstrap_helper = function (i, m, xmins, pars) {
  x = sample(m$dat, length(m$dat), replace=TRUE)
  
  m_cpy = m$getRefClass()$new(x)
  unlist(estimate_xmin(m_cpy, xmins=xmins, pars=pars))
}

#' @description \code{bootstrap} estimates the unncertainity 
#' in the xmin and parameter values using bootstraping. 
#' This function runs in parallel with the
#' number of threads specficied by the \code{threads} argument. 
#' 
#' \code{bootstrap_p} estimates the unncertainity 
#' in the xmin and parameter values using bootstraping. 

#' @importFrom parallel makeCluster parSapply 
#' @importFrom parallel clusterExport stopCluster
#' @rdname estimate_xmin
#' @param threads number of concurrent threads used during the bootstrap.
#' @param no_of_sims number of bootstrap simulations. This can 
#' take a while to run.
#' @return Returns a list containing the results of the bootstrap 
#' procedure. The list gives, amongst other things, an estimate of
#' how long a single bootstrap takes.
#' @export
bootstrap = function (m, xmins=NULL, pars=NULL, 
                        no_of_sims=100, threads=1) {
  m_cpy = m$copy()
  gof_v = estimate_xmin(m_cpy, xmins=xmins, pars=pars)
  m_cpy$setXmin(gof_v)
  x = m_cpy$dat
  N = length(x)
  
  start_time = Sys.time()
  ##Parallel bootstrap
  cl = makeCluster(threads)
  clusterExport(cl, c("estimate_xmin"))
  nof = parSapply(cl, 1:no_of_sims,
                  bootstrap_helper,  m_cpy, 
                  xmins, pars)
  stopCluster(cl)
  end_time = Sys.time()
  
  
  total_time = difftime(end_time, start_time, units="secs")
  l = list(gof = gof_v[["KS"]], 
       bootstraps = as.data.frame(t(nof)), 
       sim_time = total_time[[1]]/no_of_sims)
  class(l) = "bs_xmin"
  l
}


