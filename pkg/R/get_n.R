#' Sample size
#'
#' Returns the sample size of the data set contained within the 
#' distribution object.
#' @inheritParams dist_cdf
#' @export
#' @examples
#' ################################################
#' # Load data and create example object
#' ################################################
#' data(moby_sample)
#' m = displ$new(moby_sample)
#' ################################################
#' # get_n and length should return the same value
#' ################################################
#' get_n(m)
#' length(moby_sample)
get_n = function(m) {
  n = m$internal$cum_n[1]
  if(is.null(n)) n = 0 
  n
}

