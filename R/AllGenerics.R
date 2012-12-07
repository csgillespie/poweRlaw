##Plotting generics
#' @exportMethod lines
setGeneric("lines")


#' @exportMethod points
setGeneric("points")

#' The cumulative distribution function
#'
#' This is generic function for distribution reference objects.
#' This function calculates the probability density for the current 
#' parameters and xmin value.
#' 
#' @param m a reference class distribution object. 
#' This object should contains data
#' @param a q a vector values where the function will be evaluated. 
#' If \code{q} is \code{NULL} (default), then the data value will be used.
#' @param lower.tail logical; 
#' if \code{TRUE} (default), probabilities are \eqn{P[X \le x]}, 
#' otherwise, \eqn{P[X > x]}.
#' @note These methods do *not* alter the internal state of
#' the distribubtion objects.
#' @docType methods
#' @exportMethod dist_cdf
#' @note This method does *not* alter the internal state of
#' the distribubtion objects.
#' @rdname dist_cdf-methods
#' @export
#' @examples
#' data(moby_sample)
#' pl_d = pl_data$new(moby_sample)
#' m = displ$new(pl_d)
#' m$setXmin(7);m$setPars(2)
#' #CDF at a particular value
#' dist_cdf(m, 10:15)
#' dist_cdf(m) #at a the data values
setGeneric("dist_cdf", 
           function(m, q=NULL, lower.tail=FALSE) standardGeneric("dist_cdf"))

#' PDF for the distribution objects.
#'
#' This is generic function for distribution reference objects.
#' This function calculates the probability density for the current 
#' parameters and xmin value.
#'
#' @param m The distribution reference object.
#' @param q a vector values where the function will be evaluated. 
#' If \code{q} is \code{NULL} (default), then the data value will be used.
#' @return The probability density (or mass) function
#' 
#' @seealso \code{\link{dist_cdf}}, \code{\link{dist_ll}} 
#' and \code{\link{dist_rand}}
#' @note This method does *not* alter the internal state of
#' the distribubtion objects.
#' @export
#' @docType methods
#' @rdname dist_pdf-methods
#' @examples
#' data(moby_sample)
#' pl_d = pl_data$new(moby_sample)
#' m = displ$new(pl_d)
#' m$setXmin(7);m$setPars(2)
#' dist_pdf(m)
setGeneric("dist_pdf", function(m, q=NULL) standardGeneric("dist_pdf"))


#' log-likelihood for the distribution objects.
#'
#' This is generic function for distribution reference objects.
#' This function calculates the log-likelihood for the current 
#' parameters and xmin value.
#'
#' @param m The distribution reference object.
#' @return The log-likelihood
#' 
#' @seealso \code{\link{dist_cdf}},  \code{\link{dist_pdf}} 
#' and \code{\link{dist_rand}}
#' @note This method does *not* alter the internal state of
#' the distribubtion objects.
#' @export
#' @docType methods
#' @rdname dist_ll-methods
#' @examples
#' data(moby_sample)
#' pl_d = pl_data$new(moby_sample)
#' m = displ$new(pl_d)
#' m$setXmin(7);m$setPars(2)
#' dist_ll(m)
setGeneric("dist_ll", function(m) standardGeneric("dist_ll"))

#' Random number generation for the distribution objects
#'
#' This is generic function for generating random numbers from the underlying 
#' distribution of the distribution reference objects.
#' This function generates \code{n} random numbers using the parameters 
#' and xmin values found in the associated reference object.
#'
#' @param m a distribution reference object.
#' @param n number of observations to be generated.
#'
#' @return n random numbers
#' 
#' @seealso \code{\link{dist_cdf}},  \code{\link{dist_pdf}} 
#' and \code{\link{dist_ll}}
#' @note This method does *not* alter the internal state of
#' the distribubtion object.
#' @export
#' @docType methods
#' @rdname dist_rand-methods
#' @examples
#' data(moby_sample)
#' pl_d = pl_data$new(moby_sample)
#' m = displ$new(pl_d)
#' m$setXmin(7);m$setPars(2)
#' dist_rand(m, 5)
setGeneric("dist_rand", function(m, n) standardGeneric("dist_rand"))


