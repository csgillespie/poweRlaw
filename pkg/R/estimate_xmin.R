get_xmin_est = function(dat, xmins){
  row = which.min(dat[,1])
  ## Check for numerical instabilities
  ## Can happen in the tails of the LN
  if(!length(row)) {
    row = 1L
    dat[row,] = NA_real_
    xmins[row] = NA_real_
    warning("Unable to estimate xmin. This may be due to numerical instabilities. 
            For example the parameter estimates are in the distribution tails.")
  }
  
  xmin = xmins[row]
  pars = dat[row, 2:(ncol(dat)-1L)]
  ntail = dat[row, ncol(dat)]
  
  l = list(gof=dat[row, 1], xmin=xmin, pars=pars, ntail=ntail)
  class(l) = "estimate_xmin"
  l
}

get_gof = function(fit_cdf, data_cdf, distance) {
  if(!(distance %in% c("ks", "reweight")) || length(distance) > 1)
    stop("Unknown distance measure. The distance parameter should be either ks or reweight")
  
  
  if(distance == "ks")
    gof = max(abs(data_cdf - fit_cdf))
  if(distance == "reweight")
    gof = max(abs(data_cdf - fit_cdf)/sqrt(fit_cdf*(1-fit_cdf)))
  gof 
}

#' @rdname get_KS_statistic-deprecated
#' @title Deprecated function
#' @description This function is now deprecated and may be removed in future 
#' versions.
#' @inheritParams get_distance_statistic
#' @seealso get_distance_statistic
#' @export
get_KS_statistic = function(m, xmax=1e5, distance="ks") {
  .Deprecated("get_distance_statistic")
  get_distance_statistic(m, xmax, distance)
}

#' @rdname estimate_xmin
#' @export
get_distance_statistic = function(m, xmax=1e5, distance="ks") {
  if(is(m, "discrete_distribution")) {
    data_cdf = dist_data_all_cdf(m, xmax=xmax)
    fit_cdf = dist_all_cdf(m, xmax=xmax)
  } else {
    q = m$dat
    n = m$internal[["n"]]
    N = length(q)
    q = q[(N-n+1):N]
    q = q[q <= xmax]
    fit_cdf = dist_cdf(m, q)
    data_cdf = ((0:(n-1))/n)[1:length(fit_cdf)]
  }
  get_gof(fit_cdf, data_cdf, distance)
}

#' Estimating the lower bound (xmin)
#' 
#' When fitting heavy tailed distributions, sometimes it 
#' is necessary to estimate the lower threshold, xmin. The
#' lower bound is estimated by calculating the minimising the
#' Kolmogorov-Smirnoff statistic 
#' (as described in Clauset, Shalizi, Newman (2009)).
#' \describe{
#' \item{\code{get_KS_statistic}}{Calculates the KS statistic for a particular value of xmin.}
#' \item{\code{estimate_xmin}}{Estimates the optimal lower cutoff using a 
#' goodness-of-fit based approach. This function may issue \code{warnings} 
#' when fitting lognormal, Poisson or Exponential distributions. The 
#' warnings occur for large values of \code{xmin}. Essentially, we are discarding 
#' the bulk of the distribution and cannot calculate the tails to enough
#' accuracy.}
#' \item{\code{bootstrap}}{Estimates the unncertainity in the xmin and parameter values via bootstraping.}
#' \item{\code{bootstrap_p}}{Performs a bootstrapping hypothesis test to determine whether a suggested
#' (typically power law) distribution is plausible. This is only available for distributions that 
#' have \code{dist_rand} methods available.}}
#' @param m A reference class object that contains the data.
#' @param distance A string containing the distance measure (or measures) to calculate. 
#' Possible values are \code{ks} or \code{reweight}. See details for further information. 
#' @param pars default \code{NULL}. A vector of parameters used to 
#' optimise over. 
#' Otherwise, for each value of \code{xmin}, the mle will be used, i.e. \code{estimate_pars(m)}.
#' For small samples, the mle may be biased. 
#' @param xmins default \code{1e5}. A vector of possible values 
#' of xmin to explore. When a single value is passed, this represents
#' the maximum value to search, i.e. by default we search from
#' (1, 1e5). See details for further information.
#' @param xmax default \code{1e5}. The maximum x value calculated when working out the CDF. See details for further 
#' information.
#' @param threads number of concurrent threads used during the bootstrap.
#' @param no_of_sims number of bootstrap simulations. When \code{no_of_sims} is large, this can 
#' take a while to run.
#' @details When estimating \code{xmin} for discrete distributions, the search space when 
#' comparing the data-cdf (empirical cdf)
#' and the distribution_cdf runs from xmin to \code{max(x)}
#' where \code{x} is the data set. This \strong{can} often be 
#' computationally brutal. In particular, when bootstrapping
#' we generate random numbers from the power law distribution, 
#' which has a long tail. 
#' 
#' To speed up computations for discrete distributions it is sensible to put an 
#' upper bound, i.e. \code{xmax} and/or explicitly give values of where to search, i.e. \code{xmin}.
#' 
#' Occassionally bootstrapping can generate strange situations. For example, 
#' all values in the simulated data set are less then \code{xmin}. In this case, 
#' the estimated distance measure will be \code{Inf} and the parameter values, \code{NA}.
#' 
#' There are other possible distance measures that can be calculated. The default is the
#' Kolomogorov Smirnoff statistic (\code{KS}). This is equation 3.9 in the CSN paper. The
#' other measure currently available is \code{reweight}, which is equation 3.11.
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
#' \dontrun{
#' bootstrap(m, no_of_sims=1, threads=1)
#' bootstrap_p(m, no_of_sims=1, threads=1)
#' }
estimate_xmin = function (m, xmins=NULL, pars=NULL, 
                          xmax=1e5, distance="ks") {
  ## Flag. Go through a bunch of checks to test whether we 
  ## can estimate xmin 
  estimate = length(unique(m$dat)) > 1
  
  ## Make thread safe
  if(estimate) {
    m_cpy = m$getRefClass()$new(m$dat)
    m_cpy$pars = pars
    if(is.null(xmins))  xmins = unique(m$dat)
    xmins = xmins[xmins <= xmax]
  }
  
  ## Need to have at least no_pars + 1 data points
  ## to estimate parameters. 
  ## Find (largest - no_pars) data point and subset xmins
  if(estimate) {
    unique_dat = unique(m_cpy$dat)
    q_len = length(unique_dat)
    max_data_pt_needed = sort(unique_dat, 
                              partial=q_len-m_cpy$no_pars-1)[q_len-m_cpy$no_pars-1]
    xmins = xmins[xmins <= max_data_pt_needed]
    
    ## Initialise xmin scan
    nr = length(xmins)
  }
  
  ## Bootstrapping may generate strange data
  ## Columns: gof, Pars, xmin, ntail
  if(!estimate || nr < 1) {
    ## Insufficient data to estimate parameters
    dat = matrix(0, nrow=1, ncol=(2 + m$no_pars))
    dat[1, ] = c(rep(Inf, length(distance)), 
                 rep(NA, m$no_pars + 1))
    estimate = FALSE
  } else {
    dat = matrix(0, nrow=nr, ncol=(2 + m_cpy$no_pars))   
    est = estimate_pars(m_cpy)$pars
  }
  
  xm = 0L
  while(estimate && xm < nr)   {
    m_cpy$xmin = xmins[xm + 1L]
    if(is.null(pars)) m_cpy$mle(initialise=est)
    else m_cpy$pars = pars
    
    ## Doesn't work for lognormal - need par matrix    
    if(!is.null(pars)) {
      L = dist_ll(m_cpy)
      I = which.max(L)
      m_cpy$pars = m_cpy$pars[I]
    }
    gof = get_distance_statistic(m_cpy, xmax, distance)
    dat[xm <- xm + 1L,] = c(gof, m_cpy$pars, get_ntail(m_cpy))
  }
  
  l = get_xmin_est(dat, xmins)
  l$distance = distance
  l
}
