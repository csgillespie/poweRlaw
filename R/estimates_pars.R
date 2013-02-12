#' @title Estimates the distributions using mle. 
#' 
#' @description \code{estimate_pars} estimates the distribution's 
#' parameters using their maximum likelihood estimator. This estimate
#' is conditional on the current xmin value.
#' @param m A reference class object that contains the data.
#' @return returns a vector of the mles.
#' @export
#' @examples
#' data(moby_sample)
#' pl_d = pl_data$new(moby_sample)
#' m = displ$new(pl_d)
#' estimate_xmin(m)
#' m$setXmin(7)
#' estimate_pars(m)
estimate_pars = function (m) {
    m$mle(set=FALSE)
}