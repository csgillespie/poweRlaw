##Plotting generics
#' @exportMethod lines
setGeneric("lines")

#' @exportMethod points
setGeneric("points")


#' The cumulative distribution function
#'
#' This is generic function for calculating
#' the cumulative distribution function (cdf) of 
#' reference objects. This is similar to base R's \code{pnorm} for the 
#' normal distribution. The \code{dist_cdf} function calculates the 
#' cumulative probability distribution for the 
#' current parameters and xmin value.
#' 
#' @param m a distribution object.
#' @param all_values logical; if \code{FALSE} (default). If \code{TRUE},
#' then the cdf is evaluated at points xmin, xmin+1, ..., xmax.
#' @param q a vector values where the function will be evaluated. 
#' If \code{q} is \code{NULL} (default), then the data values 
#' will be used.
#' @param lower_tail logical; if \code{TRUE} (default), 
#' probabilities are \eqn{P[X \le x]}, 
#' otherwise, \eqn{P[X > x]}.
#' @docType methods
#' @exportMethod dist_cdf
#' @note This method does *not* alter the internal state of
#' the distribubtion objects.
#' @rdname dist_cdf-methods
#' @export
#' @examples
#' data(moby_sample)
#' m = displ$new(moby_sample)
#' m$setXmin(7); m$setPars(2)
#' #CDF at a particular value
#' dist_cdf(m, 10:15)
#' dist_cdf(m) #at the data values
setGeneric("dist_cdf", 
           function(m, q=NULL, lower_tail=FALSE, ...) 
             standardGeneric("dist_cdf"))


#' The data cumulative distribution function
#'
#' This is generic function for distribution reference objects.
#' This function calculates the cumulative probability density 
#' for the current parameters and xmin value.
#' 
#' @param m a reference class distribution object.
#' @param lower_tail logical; 
#' if \code{TRUE} (default), probabilities are \eqn{P[X \le x]}, 
#' otherwise, \eqn{P[X > x]}.
#' @param all_values logical; if \code{FALSE} (default). If \code{TRUE},
#' then the cdf is evaluated at points xmin, xmin+1, ..., xmax.
#' @docType methods
#' @exportMethod dist_data_cdf
#' @note This method does *not* alter the internal state of
#' the distribubtion objects.
#' @rdname dist_data_cdf-methods
#' @export
#' @examples
#' data(moby_sample)
#' m = displ$new(moby_sample)
#' m$setXmin(7);m$setPars(2)
#' dist_data_cdf(m)
setGeneric("dist_data_cdf", 
           function(m, lower_tail=TRUE, all_values=FALSE) 
             standardGeneric("dist_data_cdf"))






#' The probability density function
#'
#' This is generic function for distribution reference objects.
#' This function calculates the probability density function (pdf) 
#' for the current parameters and xmin value.
#'
#' @param m The distribution reference object.
#' @param q a vector values where the function will be evaluated. 
#' If \code{q} is \code{NULL} (default), then the data value will be used.
#' @param log default \code{FALSE}. If \code{TRUE}, probabilities are given as log(p).
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
#' m = displ$new(moby_sample)
#' m$setXmin(7);m$setPars(2)
#' dist_pdf(m)
setGeneric("dist_pdf", 
           function(m, q=NULL, log=FALSE) 
             standardGeneric("dist_pdf"))


#' The log-likelihood function
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
#' m = displ$new(moby_sample)
#' m$setXmin(7);m$setPars(2)
#' dist_ll(m)
setGeneric("dist_ll", 
           function(m) 
             standardGeneric("dist_ll"))

#' Random number generation for the distribution objects
#'
#' This is generic function for generating random numbers 
#' from the underlying 
#' distribution of the distribution reference objects.
#' This function generates \code{n} random numbers using the parameters 
#' and xmin values found in the associated reference object.
#'
#' @param m a distribution reference object.
#' @param n number of observations to be generated.
#' @return n random numbers
#' 
#' @seealso \code{\link{dist_cdf}}, \code{\link{dist_pdf}} 
#' and \code{\link{dist_ll}}
#' @note This method does *not* alter the internal state of
#' the distribubtion object.
#' @export
#' @docType methods
#' @rdname dist_rand-methods
#' @examples
#' data(moby_sample)
#' m = displ$new(moby_sample)
#' m$setXmin(7);m$setPars(2)
#' dist_rand(m, 5)
setGeneric("dist_rand", 
           function(m, n) 
             standardGeneric("dist_rand"))

