bootstrap_helper = function (i, m, N, y, xmins, pars) {
  ##Make thread safe
  m_tmp = m$copy()
  ny = length(y);  nz = N - ny; pz = nz/N
  
  n1 = sum(runif(N) > pz)
  q1 = y[sample(N-nz, n1, replace=TRUE)]
  q2 = dist_rand(m_tmp, N-n1)
  q = c(q1, q2)
  m_tmp$pl_data = pl_data$new(q)
  estimate_xmin(m_tmp, xmins=xmins, pars=pars)
}

#' @description \code{bootstrap_xmin} estimates unncertainity in the xmin and
#' parameter values using bootstraping. This function runs in parallel with the
#' number of threads specficied by the \code{threads} argument. 
#' @importFrom parallel makeCluster parSapply 
#' @importFrom parallel clusterExport stopCluster
#' @rdname estimate_xmin
#' @param threads number of concurrent threads used during the bootstrap
#' @param no_of_sims number of bootstrap simulations. This can take a while to
#' run
#' @export
bootstrap_xmin = function (m, xmins=NULL, pars=NULL, no_of_sims=100, threads=1) {
  m_cpy = m$copy()
  gof_v = estimate_xmin(m_cpy)
  m_cpy$xmin = gof_v["xmin"]
  m_cpy$pars = gof_v["pars"]
  
  x = m_cpy$pl_data$x
  N = length(x)
  z = x[x >= m_cpy$xmin]#; nz = length(z)
  y = x[x < m_cpy$xmin]#; ny = length(y)
  
  ##Parallel bootstrap
  cl = makeCluster(threads)
  clusterExport(cl, c("dist_rand", "estimate_xmin", "pl_data"))
  #, environment(dist_rand))
  nof = parSapply(cl, 1:no_of_sims, 
                  bootstrap_helper,  m_cpy, N, y, xmins, pars)
  stopCluster(cl)
  
  list(p=sum(nof >= gof_v["KS"])/length(nof), 
       gof = gof_v["KS"], 
       bootstraps = as.data.frame(t(nof)))
}


