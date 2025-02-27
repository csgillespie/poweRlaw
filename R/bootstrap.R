sample_helper = function(i, dat) {
  sample(dat, length(dat), replace = TRUE)
}

bootstrap_helper = function(i, m, xmins, pars, xmax, distance) {
  x = sample_helper(i, m$dat)
  m_cpy = m$getRefClass()$new(x)
  est = estimate_xmin(m_cpy, xmins = xmins, pars = pars, xmax = xmax, distance = distance)
  est["distance"] = NULL
  unlist(est)
}

#' @rdname estimate_xmin
#' @export
get_bootstrap_sims = function(m, no_of_sims, seed, threads = 1) {
  cl = parallel::makeCluster(threads)
  on.exit(parallel::stopCluster(cl))

  ## Set cluster seed
  parallel::clusterSetRNGStream(cl, seed)
  parallel::parSapply(cl, 1:no_of_sims, sample_helper, m$dat)
}

#' @rdname estimate_xmin
#' @param seed default `NULL`. An integer to be supplied to `set.seed`, or `NULL`
#' not to set reproducible seeds. This argument is passed `clusterSetRNGStream`.
#' @export
bootstrap = function(m, xmins = NULL, pars = NULL, xmax = 1e5,
                      no_of_sims = 100, threads = 1,
                      seed = NULL, distance = "ks") {
  time = timer()
  m_cpy = m$copy()
  time$start()
  gof_v = estimate_xmin(m_cpy, xmins = xmins, pars = pars, xmax = xmax, distance = distance)
  time$stop()
  if (is.na(gof_v$gof) || is.infinite(gof_v$gof)) {
    stop("Unable to estimate initial parameters using estimate_xmin, so we can't bootstrap.")
  }

  if (min(m_cpy$dat) > xmax) {
    stop("The smallest value in your data set is larger than xmax. The xmax
         parameter is the upper limit of the xmin search space.")
  }

  if (max(m_cpy$dat) > xmax) {
    message("Some of your data is larger than xmax. The xmax parameter is
            the upper bound of the xmin search space. You could try increasing
            it. If the estimated values are below xmax, it's probably OK not to
            worry about this.")
  }

  message("Expected total run time for ", no_of_sims,
            " sims, using ", threads, " threads is ",
          signif(time$get() * no_of_sims / threads, 3), " seconds.")

  m_cpy$setXmin(gof_v)

  ## Start clock and parallel boostrap
  cl = parallel::makeCluster(threads)
  on.exit(parallel::stopCluster(cl))
  time$start()

  ## Set cluster seed
  if (!is.null(seed)) parallel::clusterSetRNGStream(cl, seed)

  parallel::clusterExport(cl, c("estimate_xmin"))
  nof = parallel::parSapply(cl, 1:no_of_sims,
                  bootstrap_helper,  m_cpy,
                  xmins, pars, xmax, distance)

  ## Stop clock and cluster
  total_time = time$get(stop = TRUE) * threads

  bootstraps = as.data.frame(t(nof), stringsAsFactors = FALSE)
  l = list(gof = gof_v[["gof"]],
           bootstraps = bootstraps,
           sim_time = total_time[[1]] / no_of_sims,
           seed = seed,
           package_version = utils::packageVersion("poweRlaw"),
           distance = distance)
  class(l) = "bs_xmin"
  l
}
