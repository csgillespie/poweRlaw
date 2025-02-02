#' @title Estimates the distributions using mle.
#'
#' @description `estimate_pars` estimates the distribution's
#' parameters using their maximum likelihood estimator. This estimate
#' is conditional on the current xmin value.
#' @inheritParams estimate_xmin
#' @return returns list.
#' @export
#' @examples
#' data(moby_sample)
#' m = displ$new(moby_sample)
#' estimate_xmin(m)
#' m$setXmin(7)
#' estimate_pars(m)
estimate_pars = function(m, pars = NULL) {
  if (is.null(m$getDat())) {
    l = list(pars = NA, ll = NA)
    class(l) = "estimate_pars"
  } else if (is.null(pars)) {
    l = m$mle(set = FALSE)
  } else {
    m$pars = pars
    ll = dist_ll(m)
    i = which.max(ll)

    if (is.vector(pars) && m$no_pars == 1L) par = pars[i]
    else if (is.vector(pars)) par = pars
    else par = unlist(pars[i, ], use.names = FALSE)
    l = list(pars = par, ll = ll[i])
    class(l) = "estimate_pars"
  }
  return(l)
}
