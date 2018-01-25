#############################################################
#Reference Class definition
#############################################################
#' @rdname displ
#' @aliases conexp-class conexp
#' @exportClass conexp
#' @importFrom methods new
#' @importFrom stats dexp pexp
#' @export conexp
conexp = 
  setRefClass("conexp", 
              contains="ctn_distribution",
              fields = list(
                dat = function(x) {
                  if(!missing(x) && !is.null(x)) {
                    check_ctn_data(x)
                    d = sort(x)
                    internal[["cum_n"]] <<- length(d):1
                    internal[["dat"]] <<- sort(d)
                    xmin <<- d[1]
                  } else internal[["dat"]]
                },
                xmin = function(x) {
                  if(!missing(x) && !is.null(x)) {
                    if("estimate_xmin" %in% class(x)) {
                      pars <<- x$pars
                      x = x$xmin
                    }
                    internal[["xmin"]] <<- x
                    if(length(internal[["dat"]])) {
                      selection = min(which(internal[["dat"]] >= (x- .Machine$double.eps ^ 0.5)))
                      internal[["n"]] <<- internal[["cum_n"]][selection]      
                    }
                    
                  } else  internal[["xmin"]]
                }, 
                pars = function(x) {
                  if (!missing(x) && !is.null(x)) {
                    if("estimate_pars" %in% class(x)) x = x$pars
                    internal[["pars"]] <<- x
                  } else internal[["pars"]]
                }
              )
  )
#############################################################
#Initialisation
#############################################################
conexp$methods( 
  list(
    initialize = function(dat) {
      no_pars <<- 1
      ##Use the internal attribute for copying
      if(!missing(dat)) {
        check_ctn_data(dat)
        d = sort(dat)
        internal[["cum_n"]] <<- length(d):1
        internal[["dat"]] <<- sort(d)
        xmin <<- d[1]
      }
    }
  )
)

#############################################################
#PDF method
#############################################################
#' @rdname dist_pdf-methods
#' @aliases dist_pdf,conexp-method
setMethod("dist_pdf",
          signature = signature(m="conexp"),
          definition = function(m, q=NULL, log=FALSE) {
            xmin = m$getXmin(); pars = m$getPars()
            
            if(is.null(q)) q = m$dat
            pdf = dexp(q, pars, log=TRUE) - pexp(xmin, pars, lower.tail=FALSE, log.p=TRUE)
            if(!log) {
              pdf = exp(pdf)
              pdf[q < xmin] = 0
            } else {
              pdf[q < xmin] = -Inf
            }
            pdf
          }
)

#############################################################
#CDF method
#############################################################
#' @rdname dist_cdf-methods
#' @aliases dist_cdf,conexp-method
setMethod("dist_cdf",
          signature = signature(m="conexp"),
          definition = function(m, q=NULL, lower_tail=TRUE) {
            pars = m$pars; xmin = m$xmin
            if(is.null(pars)) stop("Model parameters not set.")  
            if(is.null(q)) q = m$dat

            if(lower_tail) {
              p = pexp(q, pars, lower.tail=lower_tail) 
              C = pexp(xmin, pars, lower.tail=FALSE) 
              cdf = (p/C-1/C+1)
            } else {
              log_p = pexp(q, pars, lower.tail=FALSE, log.p=TRUE) 
              log_C = pexp(xmin, pars, lower.tail=FALSE, log.p=TRUE)
              cdf = exp(log_p - log_C)
            }
            cdf[q < xmin] = 0
            cdf
          }
)

#' @rdname dist_cdf-methods
#' @aliases dist_all_cdf,conexo-method
setMethod("dist_all_cdf",
          signature = signature(m="conexp"),
          definition = function(m, lower_tail=TRUE, xmax=1e5) {
            xmin = m$getXmin()
            xmax = min(max(m$dat), xmax)
            dist_cdf(m, q=xmin:xmax, lower_tail=lower_tail)
          }
)

#############################################################
#ll method
#############################################################
#' @rdname dist_ll-methods
#' @aliases dist_ll,conexp-method
setMethod("dist_ll",
          signature = signature(m="conexp"),
          definition = function(m) {
            n = m$internal[["n"]]
            q = m$dat
            N = length(q)
            q = q[(N-n+1):N]
            conexp_tail_ll(q, m$getPars(), m$getXmin())
          }
)

########################################################
#Log-likelihood 
########################################################
conexp_tail_ll = function(x, rate, xmin) {
  n = length(x)
  joint_prob = colSums(
                  matrix(## Needed for edge cases
                    sapply(rate, function(i) dexp(x, i, log=TRUE)), nrow=length(x)))
  prob_over = sapply(rate, 
                     function(i) 
                       pexp(xmin, i, 
                            lower.tail=FALSE, log.p=TRUE))
  return(joint_prob - n*prob_over)
}


########################################################
#Rand number generator
########################################################
#' @rdname dist_rand-methods
#' @aliases dist_rand,conexp-method
setMethod("dist_rand",
          signature = signature(m="conexp"),
          definition = function(m, n="numeric") {
            ## Rearrange the usual formula for generating
            ## exp random numbers: -log(u)/lambda > xmin
            u = runif(n, 0, exp(-m$pars*m$xmin))
            -log(u)/m$pars
          }
)  

#############################################################
#MLE method
#############################################################
conexp$methods(
  mle = function(set = TRUE, initialise=NULL) {
    x = dat
    x = x[x > xmin]
    if(is.null(initialise))
      theta_0 = mean(x)
    else 
      theta_0 = initialise
    # Chop off values below 
    negloglike = function(par) {
      r = -conexp_tail_ll(x, par, xmin)
      if(!is.finite(r)) r = 1e12
      r
    }
    mle = suppressWarnings(optim(par=theta_0, fn=negloglike, method="L-BFGS-B", lower=0))
    if(set)
      pars <<- mle$par
    class(mle) = "estimate_pars"
    names(mle)[1L] = "pars"
    mle
    
  }
)
