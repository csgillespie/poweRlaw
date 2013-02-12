#' @description \code{get_KS_statistic} calculates the Kolmogorov-Smirnov or 
#' KS statistic. 
#' This is the maximum distance between the data CDF and fitted model CDF.
#' This corresponds to expression (3.9) in the Newman, et al, 2009 paper. 
#' The Kolmogorov-Smirnov statistic is used when estimating the cut-off, xmin.
#' @rdname estimate_xmin
#' @export
get_KS_statistic = function(m) {
  x_values = m$pl_data$x
  x_values = x_values[x_values >= m$xmin]
  data_cdf = get_data_cdf(x_values, pad=TRUE)[m$xmin:max(x_values)]
  fit_cdf = dist_cdf(m, cumulative=TRUE)
  
  gof = max(abs(data_cdf - fit_cdf))
  return(gof)
}


#' @title Estimates the lower bound (xmin)
#' 
#' @description \code{estimate_xmin} estimates the lower cutoff using a 
#' goodness-of-fit based approach. This method is described in
#' Clauset, Shalizi, Newman (2009)
#' @param m A reference class object that contains the data.
#' @param pars default NULL. A vector of parameters used to optimise over. 
#' Otherwise, for each value of xmin, the mle will be used, i.e. \code{estimate_pars(m)}.
#' For small samples, the mle may be biased. 
#' @param xmins default NULL. A vector of possible values of xmin to explore. 
#' The default, \code{xmins=NULL}, results in exploring all possible xmin values.
#' @return \code{estimate_xmin} returns a vector containing the optimial 
#' parameter value, xmin and the associated KS statistic.
#' @note Adapted from Laurent Dubroca's code found at
#' http://tuvalu.santafe.edu/~aaronc/powerlaws/plfit.r
#' @export
#' @examples
#' data(moby_sample)
#' pl_d = pl_data$new(moby_sample)
#' m = displ$new(pl_d)
#' estimate_xmin(m)
#' estimate_xmin(m, xmins=c(10, 11, 12))
#' ############################
#' ##Bootstrap examples
#' bootstrap_xmin(m, no_of_sims=2, threads=1)
estimate_xmin = function (m, xmins = NULL, pars=NULL) {
  ##Make thread safe
  m_cpy = m$copy(TRUE)
  
  if(is.null(xmins)) {
    xmins = unique(m_cpy$pl_data$x)
    xmins = xmins[-length(xmins)]
  }
  dat = matrix(0, nrow=length(xmins), ncol=2)
  
  #ll = pldis_ll$new(x)
  xm = 1
  for(xm in 1:length(xmins)){
    m_cpy$xmin = xmins[xm]
    if(is.null(pars)) m_cpy$mle()
    else m_cpy$pars = pars
    
    L = dist_ll(m_cpy)
    I = which.max(L)
    m_cpy$pars = m_cpy$pars[I]
    gof = get_KS_statistic(m_cpy)
    
    dat[xm,] = c(gof, m_cpy$pars)
  }
  
  I = which.min(dat[,1])
  xmin = xmins[I]
  n = sum(m_cpy$pl_data$x >= xmin)
  alpha = dat[I,2]
  
  r = c(dat[I,1], xmin, alpha)
  names(r) = c("KS", "xmin", "pars")
  return(r)
}

