bootstrap_p_helper = function (i, m, N, y, xmins, pars, data_max) {
  ny = length(y);  nz = N - ny; pz = nz/N
  n1 = sum(runif(N) > pz)
  q = dist_rand(m, N-n1)
  
  q = q[q < data_max]
  if(m$datatype == "discrete")
    q = c(y[sample(N-nz, n1, replace=TRUE)], q)
  else 
    q = c(runif(n1, 0, N-nz), q)
  
  m_cpy = m$getRefClass()$new(q)
  unlist(estimate_xmin(m_cpy, xmins=xmins, pars=pars))
}

#' @rdname estimate_xmin
#' @export
bootstrap_p = function (m, xmins=NULL, pars=NULL, 
                           no_of_sims=100, threads=1, 
                           data_max=1e6) {
  m_cpy = m$copy()
  gof_v = estimate_xmin(m_cpy, xmins=xmins, pars=pars)
  m_cpy$setXmin(gof_v)
  
  x = m_cpy$dat
  N = length(x)
  z = x[x >= m_cpy$xmin]
  y = x[x < m_cpy$xmin]
  
  start_time = Sys.time()
  ##Parallel bootstrap
  cl = makeCluster(threads)
  clusterExport(cl, c("dist_rand", "estimate_xmin"))
  nof = parSapply(cl, 1:no_of_sims,
                 bootstrap_p_helper,  m_cpy, 
                 N, y, xmins, pars, data_max)
  stopCluster(cl)
  end_time = Sys.time()
  total_time = difftime(end_time, start_time, units="secs")
  l = list(p=sum(nof[1,] >= gof_v[["KS"]])/no_of_sims, 
       gof = gof_v[["KS"]], 
       bootstraps = as.data.frame(t(nof)), 
       sim_time = total_time[[1]]/no_of_sims)
  class(l) = "bs_xmin"
  l
}


