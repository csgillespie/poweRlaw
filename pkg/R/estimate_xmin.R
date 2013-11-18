#' @rdname estimate_xmin
#' @export
get_KS_statistic = function(m) {
  if(is(m, "discrete_distribution")) {
    data_cdf = dist_data_cdf(m, all_values=TRUE)
    fit_cdf = dist_cdf(m, all_values=TRUE)
  } else {
    data_cdf = dist_data_cdf(m)
    fit_cdf = dist_cdf(m)
  }
  gof = max(abs(data_cdf - fit_cdf))
  return(gof)
}


#' Estimates the lower bound (xmin)
#' 
#' When fitting heavy tailed distributions, sometimes it is necessary to estimate the lower threshold, xmin. The
#' lower bound is estimated by calculating the minimising the Kolmogorov-Smirnoff statistic 
#' (as described in Clauset, Shalizi, Newman (2009)).
#' \describe{
#' \item{\code{get_KS_statistic}}{Calculates the KS statistic for a particular value of xmin}
#' \item{\code{estimate_xmin}}{Estimates the optimal lower cutoff using a 
#' goodness-of-fit based approach. This function may issue \code{warnings} 
#' when fitting lognormal, Poisson or Exponential distributions. The 
#' warnings occur for large values of xmin. Essentially, we are discarding 
#' the bult of the distribution and cannot calculate the tails to enough
#' accuracy.}
#' \item{\code{bootstrap}}{Estimates the unncertainity in the xmin and parameter values via bootstraping.}
#' \item{\code{bootstrap_p}}{Performs a bootstrapping hypothesis test to determine whether a power law
#' distribution is plausible. This function only available for power law distribution objects.}}
#' @param m A reference class object that contains the data.
#' @param pars default NULL. A vector of parameters used to optimise over. 
#' Otherwise, for each value of xmin, the mle will be used, i.e. \code{estimate_pars(m)}.
#' For small samples, the mle may be biased. 
#' @param xmins default NULL. A vector of possible values of xmin to explore. 
#' The default, \code{xmins=NULL}, results in exploring all possible xmin values.
#' @param data_max default 1e5. When estimating xmin for discrete distributions, 
#' the search space when comparing the data_cdf and distribution_cdf runs from 1:data_max
#' @param threads number of concurrent threads used during the bootstrap.
#' @param no_of_sims number of bootstrap simulations. When \code{no_of_sims} is large, this can 
#' take a while to run.
#' @importFrom parallel makeCluster parSapply 
#' @importFrom parallel clusterExport stopCluster
#' @note Adapted from Laurent Dubroca's code found at
#' \url{http://tuvalu.santafe.edu/~aaronc/powerlaws/plfit.r}
#' @export
#' @examples
#' ###################################################
#' # Load the data set and create distribution object#
#' ###################################################
#' x = 1:10
#' m = displ$new(x)
#' 
#' ###################################################
#' # Estimate xmin and pars                          #
#' ###################################################
#' est = estimate_xmin(m)
#' m$setXmin(est)
#' 
#' ###################################################
#' # Bootstrap examples                              #    
#' ###################################################
#' bootstrap(m, no_of_sims=1, threads=1)
#' bootstrap_p(m, no_of_sims=1, threads=1)
estimate_xmin = function (m, 
                          xmins = NULL, 
                          pars=NULL,
                          data_max = 1e5) {
  ##Make thread safe
  m_cpy = m$getRefClass()$new(m$dat)
  if(is.null(xmins)) xmins = unique(m$dat)
  
  ## xmin scan
  nr = length(xmins) - m_cpy$no_pars - 1
  nr = min(data_max, nr)
  dat = matrix(0, nrow=nr, ncol=(1 + m_cpy$no_pars))
  
  xm = 1
  ##Initialise
  est = estimate_pars(m_cpy)$pars
  m_cpy$pars = pars
  
  for(xm in 1:nr){
   
    m_cpy$xmin = xmins[xm]
    if(is.null(pars)) m_cpy$mle(initialise=est)
    else m_cpy$pars = pars
    
    ##Doesn't work for lognormal - need par matrix    
    if(!is.null(pars)) {
      L = dist_ll(m_cpy)
      I = which.max(L)
      m_cpy$pars = m_cpy$pars[I]
    }
    gof = get_KS_statistic(m_cpy)
    dat[xm,] = c(gof, m_cpy$pars)
  }
  
  I = which.min(dat[,1])
  xmin = xmins[I]
  n = sum(m_cpy$dat >= xmin)
  pars = dat[I,2:ncol(dat)]
  
  l = list(KS=dat[I,1], xmin=xmin, pars=pars)
  class(l) = "estimate_xmin"
  return(l)
}

