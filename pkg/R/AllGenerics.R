#' @include aaa_all_classes.R
##Plotting generics
#' @importFrom methods setGeneric
#' @exportMethod lines
setGeneric("lines")

#' @exportMethod points
setGeneric("points")

#' The cumulative distribution function (cdf)
#'
#' This is a generic function for calculating
#' the cumulative distribution function (cdf) of 
#' distribution objects. This is similar to base R's \code{pnorm} 
#' for the normal distribution. 
#' The \code{dist_cdf} function calculates the 
#' cumulative probability distribution for the 
#' current parameters and xmin value.
#' 
#' @param m a distribution object.
#' @param q a vector values where the function will be evaluated. 
#' If \code{q} is \code{NULL} (default), then the data values 
#' will be used.
#' @param lower_tail logical; if \code{TRUE} (default), 
#' probabilities are \eqn{P[X \le x]}, otherwise, \eqn{P[X > x]}.
#' @param xmax default \code{1e5}. The maximum x value calculated when working out the CDF.
#' @docType methods
#' @exportMethod dist_cdf
#' @note This method does *not* alter the internal state of
#' the distribubtion objects.
#' @rdname dist_cdf-methods
#' @export
#' @examples
#' ##########################################
#' #Load data and create distribution object#
#' ##########################################
#' data(moby_sample)
#' m = displ$new(moby_sample)
#' m$setXmin(7); m$setPars(2)
#' 
#' ##########################################
#' #Calculate the CDF at a particular values#
#' ##########################################
#' dist_cdf(m, 10:15)
#' 
#' ########################################## 
#' #Calculate the CDF at the data values    #
#' ##########################################
#' dist_cdf(m)
setGeneric("dist_cdf", 
           function(m, q=NULL, lower_tail=FALSE) 
             standardGeneric("dist_cdf"))

#' @export
#' @rdname dist_data_cdf-methods
setGeneric("dist_all_cdf", 
           function(m, lower_tail=TRUE, xmax=1e5) 
             standardGeneric("dist_all_cdf"))

#' The data cumulative distribution function
#'
#' This is generic function for distribution objects.
#' This function calculates the data or empirical cdf.
#' 
#' @inheritParams dist_cdf
#' @docType methods
#' @exportMethod dist_data_cdf
#' @note This method does *not* alter the internal state of
#' the distribubtion objects.
#' @rdname dist_data_cdf-methods
#' @export
#' @examples
#' ##########################################
#' #Load data and create distribution object#
#' ##########################################
#' data(moby_sample)
#' m = displ$new(moby_sample)
#' m$setXmin(7);m$setPars(2)
#' 
#' ##########################################
#' # The data cdf                           #
#' ##########################################
#' dist_data_cdf(m)
setGeneric("dist_data_cdf", 
           function(m, lower_tail=TRUE, xmax=1e5) 
             standardGeneric("dist_data_cdf"))

#' @export
#' @rdname dist_data_cdf-methods
#' @description The functions \code{dist_data_all_cdf} and \code{dist_all_cdf} are only available for discrete distributions.
#' Theremain purpose is to optimise the bootstrap procedure, where generating a vector \code{xmin:xmax} is
#' very quick. Also, when bootstrapping very large values can be generated. 
setGeneric("dist_data_all_cdf", 
           function(m, lower_tail=TRUE, xmax=1e5) 
             standardGeneric("dist_data_all_cdf"))

#' The probability density function (pdf)
#'
#' This is generic function for distribution objects.
#' This function calculates the probability density function (pdf) 
#' for the current parameters and xmin value.
#'
#' @inheritParams dist_cdf
#' @param log default \code{FALSE}. If \code{TRUE}, probabilities are given as log(p).
#' @return The probability density (or mass) function
#' 
#' @seealso \code{\link{dist_cdf}}, \code{\link{dist_ll}} 
#' and \code{\link{dist_rand}}
#' @note This method does *not* alter the internal state of
#' the distribubtion objects.
#' @exportMethod dist_pdf
#' @export
#' @docType methods
#' @rdname dist_pdf-methods
#' @examples
#' ##########################################
#' #Create distribution object              #
#' ##########################################
#' m = displ$new()
#' m$setXmin(7); m$setPars(2)
#' 
#' ##########################################
#' #Calculate the pdf at particular values  #
#' ##########################################
#' dist_pdf(m, 7:10)
setGeneric("dist_pdf", 
           function(m, q=NULL, log=FALSE) 
             standardGeneric("dist_pdf"))

#' The log-likelihood function
#'
#' This is generic function for distribution objects.
#' This function calculates the log-likelihood for the current 
#' parameters and xmin value.
#'
#' @inheritParams dist_cdf
#' @return The log-likelihood
#' 
#' @seealso \code{\link{dist_cdf}},  \code{\link{dist_pdf}} 
#' and \code{\link{dist_rand}}
#' @note This method does *not* alter the internal state of
#' the distribution objects.
#' @exportMethod dist_ll
#' @export
#' @docType methods
#' @rdname dist_ll-methods
#' @examples
#' ##########################################
#' #Load data and create distribution object#
#' ##########################################
#' data(moby_sample)
#' m = displ$new(moby_sample)
#' m$setXmin(7); m$setPars(2)
#' 
#' ##########################################
#' #Calculate the log-likelihood            #
#' ##########################################
#' dist_ll(m)
setGeneric("dist_ll", 
           function(m) 
             standardGeneric("dist_ll"))

#' Random number generation for the distribution objects
#'
#' This is generic function for generating random numbers 
#' from the underlying distribution of the distribution reference objects.
#' This function generates \code{n} random numbers using the parameters 
#' and xmin values found in the associated reference object.
#'
#' @inheritParams dist_cdf
#' @param n number of observations to be generated.
#' @return n random numbers
#' 
#' @seealso \code{\link{dist_cdf}}, \code{\link{dist_pdf}} 
#' and \code{\link{dist_ll}}
#' @note This method does *not* alter the internal state of
#' the distribubtion object. 
#' @exportMethod dist_rand
#' @export
#' @docType methods
#' @rdname dist_rand-methods
#' @examples
#' ##########################################
#' #Create distribution object              #
#' ##########################################
#' m = displ$new()
#' m$setXmin(7);m$setPars(2)
#' 
#' ##########################################
#' #Generate five random numbers            #
#' ##########################################
#' dist_rand(m, 5)
setGeneric("dist_rand", 
           function(m, n) 
             standardGeneric("dist_rand"))

