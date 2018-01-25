#############################################################
#Reference Class definition
#############################################################
#' Heavy-tailed distributions
#' 
#' The \pkg{poweRlaw} package supports a number of distributions:
#' \describe{
#' \item{displ}{Discrete power-law}
#' \item{dislnorm}{Discrete log-normal}
#' \item{dispois}{Discrete Poisson}
#' \item{disexp}{Discrete Exponential}
#' \item{conpl}{Continuous power-law}
#' \item{conlnorm}{Continuous log-normal}
#' \item{conexp}{Continuous exponential}}
#' Each object inherits the \code{discrete_distribution} or the \code{ctn_distribution} class. 
#'
#' @section Fields:
#' 
#' Each distribution object has four fields. However, the object 
#' is typically created by passing 
#' data, to the \code{dat} field. Each field has standard 
#' setters and getters. See examples below
#' \describe{
#' \item{dat}{The data set.}
#' \item{xmin}{The lower threshold, xmin. Typically set after initialisation. 
#' For the continuous  power-law, xmin >= 0 for the discrete 
#' distributions, xmin >0}
#' \item{pars}{A parameter vector. Typically set after initialisation. Note the lognormal distribution has two parameters.}
#' \item{internal}{A list. This list differs between objects and shouldn't be altered.}}
#' @param ... The object is typically created by passing 
#' data using the \code{dat} field. 
#' Each field has standard setters and getters.
#' 
#' @section Copying objects:
#' Distribution objects are reference classes. This means that when we copy
#' objects, we need to use the \code{copy} method, i.e. \code{obj$copy()}. 
#' See the examples below for further details.
#'
#' @return a reference object
#' @rdname displ
#' @aliases displ-class displ
#' @docType class
#' @aliases conpl
#' @importFrom VGAM zeta
#' @exportClass displ 
#' @export displ
#' @examples
#' ##############################################################
#' #Load data and create distribution object                    #
#' ##############################################################
#' data(moby)
#' m = displ$new(moby)
#' 
#' ##############################################################
#' #Xmin is initially the smallest x value                      #
#' ##############################################################
#' m$getXmin()
#' m$getPars()
#' 
#' ##############################################################
#' #Set Xmin and parameter                                      #
#' ##############################################################
#' m$setXmin(2)
#' m$setPars(2)
#' 
#' 
#' ##############################################################
#' #Plot the data and fitted distribution                       #
#' ##############################################################
#' plot(m)
#' lines(m)
#' ##############################################################
#' #Copying                                                     #
#' ##############################################################
#' ## Shallow copy
#' m_cpy = m
#' m_cpy$setXmin(5)
#' m$getXmin()
#' ## Instead
#' m_cpy = m$copy()
displ = 
  setRefClass("displ", 
              contains="discrete_distribution",
              fields = list(
                dat = function(x) {
                  if(!missing(x) && !is.null(x)) {
                    check_discrete_data(x)
                    x = sort(x)
                    tab = table(x)
                    values = as.numeric(names(tab))
                    freq = as.vector(tab)
                    internal[["freq"]] <<- freq
                    internal[["values"]] <<- values
                    internal[["cum_slx"]] <<-
                      rev(cumsum(log(rev(values))*rev(freq)))
                    internal[["cum_n"]] <<- rev(cumsum(rev(freq)))
                    internal[["dat"]] <<- x
                    xmin <<- min(values)
                  } else internal[["dat"]]
                },
                xmin = function(x) {
                  if(!missing(x) && !is.null(x)) {
                    if("estimate_xmin" %in% class(x)) {
                      pars <<- x$pars
                      x = x$xmin
                    }
                    internal[["xmin"]] <<- x
                    internal[["v"]] <<- 1:(x-1)
                    ##Check for empty data
                    if(length(internal[["values"]])) {
                      selection = min(which(internal[["values"]] >= x))
                      internal[["slx"]] <<- internal[["cum_slx"]][selection]
                      internal[["n"]] <<- internal[["cum_n"]][selection]    
                    }
                  } else  internal[["xmin"]]
                }, 
                pars = function(x) {
                  if (!missing(x) && !is.null(x)) {
                    if("estimate_pars" %in% class(x)) x = x$pars            
                    internal[["pars"]] <<- x
                    internal[["constant"]] <<- zeta(x)
                  } else internal[["pars"]]
                }
              ))

#############################################################
#Initialisation
#############################################################
displ$methods( 
  list(
    initialize = function(dat) {
      #datatype <<- "discrete"
      no_pars <<- 1
      ##Use the internal attribute for copying
      if(!missing(dat)) {
        check_discrete_data(dat)
        x = sort(dat)
        #x= round(sort(x))
        tab = table(x)
        values = as.numeric(names(tab))
        freq = as.vector(tab)
        internal[["freq"]] <<- freq
        internal[["values"]] <<- values
        internal[["cum_slx"]] <<-
          rev(cumsum(log(rev(values))*rev(freq)))
        internal[["cum_n"]] <<- rev(cumsum(rev(freq)))
        internal[["dat"]] <<- x
        xmin <<- min(values)
      }
    }
  )
)

#############################################################
#PDF method
#############################################################
#' @rdname dist_pdf-methods
#' @aliases dist_pdf,displ-method
setMethod("dist_pdf",
          signature = signature(m="displ"),
          definition = function(m, q=NULL, log=FALSE) {
            xmin = m$getXmin(); pars = m$getPars()
            if(is.null(q)) q = m$dat
            q = q[q >= m$xmin]
            pdf = dpldis(q[q >= m$xmin], m$xmin, m$pars, TRUE)
            if(!log) pdf = exp(pdf)
            pdf
          }
)
#############################################################
#CDF method
#############################################################
#' @rdname dist_cdf-methods
#' @aliases dist_cdf,displ-method
setMethod("dist_cdf",
          signature = signature(m="displ"),
          definition = function(m, q=NULL, lower_tail=TRUE) {
            
            xmin = m$getXmin(); pars = m$getPars()
            if(is.null(pars)) stop("Model parameters not set.")  
            
            if(is.null(q)) q = m$dat
            ppldis(q, xmin, pars, lower_tail)
          }
)

#' @rdname dist_cdf-methods
#' @aliases dist_all_cdf,displ-method
setMethod("dist_all_cdf",
          signature = signature(m="displ"),
          definition = function(m, lower_tail=TRUE, xmax=1e5) {
            
            xmin = m$getXmin(); pars = m$getPars()
            if(is.null(pars)) stop("Model parameters not set.")  
            
            inter = m$internal
            xmax = max(m$dat[m$dat <= xmax])
            v = ifelse(xmin==1, 0, sum((1:(xmin-1))^-pars))
            cumsum((((xmin:xmax)^-pars))/(inter[["constant"]] - v))                
          }
)

#############################################################
#ll method
#############################################################
#' @rdname dist_ll-methods
#' @aliases dist_ll,displ-method
setMethod("dist_ll",
          signature = signature(m="displ"),
          definition = function(m) {
            inter = m$internal
            con = inter[["constant"]]
            if(m$xmin > 2) 
              con = con - 
              colSums(vapply(m$pars, 
                             function(i) inter[["v"]]^(-i), double(m$xmin-1)))
            else if(m$xmin > 1)
              con = con - 1
            
            log_con = log(con)
            ll = -inter[["n"]]*log_con - inter[["slx"]]*m$pars
            ll[is.nan(log_con)] = -Inf
            ll
          }
)
########################################################
#Log-likelihood 
########################################################
dis_pl_ll = function(x, pars, xmin) {
  n = length(x)
  joint_prob = colSums(sapply(pars, 
                              function(i) dpldis(x, xmin, i, log=TRUE)))
  #   ##Normalise due to xmax
  prob_over = 0
  #   if(!is.null(xmax))
  #       prob_over = sapply(pars, function(i) 
  #         log(ppldis(xmax, i, lower.tail=TRUE)))
  #   
  
  return(joint_prob - n*prob_over)
}

########################################################
#Rand number generator
########################################################
#' @rdname dist_rand-methods
#' @aliases dist_rand,displ-method
setMethod("dist_rand",
          signature = signature(m="displ"),
          definition = function(m, n="numeric") {
            rpldis(n, m$xmin, m$pars)
          }
)

#############################################################
#MLE method
#############################################################
displ$methods(
  mle = function(set = TRUE, initialise=NULL) {
    n = internal[["n"]]
    
    if(is.null(initialise)) {
      slx = internal[["slx"]]
      theta_0 = 1 + n*sum(slx - log(xmin-1/2)*n)^(-1)
    } else {
      theta_0 = initialise
    }
    
    x = dat[dat > (xmin-0.5)]
    negloglike = function(par) {
      r = -dis_pl_ll(x, par, xmin)
      if(!is.finite(r)) r = 1e12
      r
    }
    
    mle = suppressWarnings(optim(par=theta_0, fn=negloglike, 
                                 method="L-BFGS-B", lower=1))
    
    if(set)  pars <<- mle$par
    class(mle) = "estimate_pars"
    names(mle)[1L] = "pars"
    mle
  }
)










