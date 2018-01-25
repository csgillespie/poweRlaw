#' Values greater than or equal to xmin
#'
#' Returns the number of data points greater than or equal to 
#' current value of xmin. In the Clauset et al, paper this is 
#' called `ntail`.
#' @inheritParams dist_cdf
#' @param prop default \code{FALSE}. Return the value as a proportion of the total sample size
#' @param lower default \code{FALSE}. If \code{TRUE} returns sample size  - ntail
#' @examples
#' ################################################
#' # Load data and create example object
#' ################################################
#' data(moby_sample)
#' m = displ$new(moby_sample)
#' m$setXmin(7)
#' ################################################
#' # Get ntail
#' ################################################
#' get_ntail(m)
#' sum(moby_sample >= 7)
#' @export
get_ntail = function(m, prop=FALSE, lower=FALSE) {
  ntail = m$internal[["n"]]
  n = get_n(m)
  if(lower)
    ntail = n - ntail
  
  if(prop)
    return(ntail/n)
  else
    return(ntail)
}

