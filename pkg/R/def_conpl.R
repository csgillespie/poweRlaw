#############################################################
#Reference Class definition
#############################################################
#' @include aaa_all_classes.R 
#' @rdname displ
#' @aliases conpl-class conpl
#' @exportClass conpl
#' @export conpl
conpl = 
  setRefClass("conpl", 
              contains="ctn_distribution",
              fields = list(
                dat = function(x) {
                  if(!missing(x) && !is.null(x)) {
                    check_ctn_data(x)
                    d = sort(x)
                    internal[["cum_slx"]] <<-
                      rev(cumsum(log(rev(d))))
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
                      internal[["slx"]] <<- internal[["cum_slx"]][selection]
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
conpl$methods( 
  list(
    initialize = function(dat) {
      no_pars <<- 1
      ##Use the internal attribute for copying
      if(!missing(dat)) {
        check_ctn_data(dat)
        d = sort(dat)
        internal[["cum_slx"]] <<-
          rev(cumsum(log(rev(d))))
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
#' @aliases dist_pdf,conpl-method
setMethod("dist_pdf",
          signature = signature(m="conpl"),
          definition = function(m, q=NULL, log=FALSE) {
            xmin = m$getXmin(); pars = m$getPars()
            if(is.null(q)) {
              q = m$dat
              n = m$internal[["n"]]; N = length(q)
              q = q[(N-n+1):N]
            } else {
              q[q >= xmin]
            }
            pdf = dplcon(q, xmin, pars, TRUE)
            if(!log) pdf = exp(pdf)
            pdf
          }
)

#############################################################
#CDF method
#############################################################
#' @rdname dist_cdf-methods
#' @aliases dist_cdf,conpl-method
setMethod("dist_cdf",
          signature = signature(m="conpl"),
          definition = function(m, q=NULL, lower_tail=TRUE) {
            xmin = m$xmin;  pars = m$pars
            if(is.null(pars)) stop("Model parameters not set.")
            if(is.null(q))  q = m$dat
             # n = m$internal[["n"]]; N = length(q)
              #              q = q[(N-n+1):N]
            #  cdf = pplcon(q, xmin, pars, lower_tail)
#             } else {
#               cdf = pplcon(q, xmin, pars, lower_tail)
#             }
            pplcon(q, xmin, pars, lower_tail)
          }
)

#' @rdname dist_cdf-methods
#' @aliases dist_all_cdf,conpl-method
setMethod("dist_all_cdf",
          signature = signature(m="conpl"),
          definition = function(m, lower_tail=TRUE, xmax=1e5) {
            xmin = m$xmin; pars = m$pars
            if(is.null(pars)) stop("Model parameters not set.")  
            
            xmax = min(max(m$dat), xmax)
            1 - (xmin:xmax/xmin)^(-pars + 1)
          }
)





#############################################################
#ll method
#############################################################
#' @rdname dist_ll-methods
#' @aliases dist_ll,conpl-method
setMethod("dist_ll",
          signature = signature(m="conpl"),
          definition = function(m) {
            n = m$internal[["n"]]
            slx = m$internal[["slx"]]
            n*log(m$pars-1) - n*log(m$xmin) - m$pars *(slx-n*log(m$xmin))
          }
)

########################################################
#Log-likelihood 
########################################################
con_pl_ll = function(x, pars, xmin) {
  n = length(x)
  joint_prob = colSums(sapply(pars, 
                              function(i) dplcon(x, xmin, i, log=TRUE)))
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
#' @aliases dist_rand,conpl-method
setMethod("dist_rand",
          signature = signature(m="conpl"),
          definition = function(m, n="numeric") {
            rplcon(n, m$xmin, m$pars)
          }
)

#############################################################
#MLE method
#############################################################
conpl$methods(
  mle = function(set = TRUE, initialise=NULL) {
    n = internal[["n"]]
    
    if(is.null(initialise)) {
      slx = internal[["slx"]]
      theta_0 = 1 + n*(slx-log(xmin)*n)^(-1)
    } else {
      theta_0 = initialise
    }
    
    
    x = dat[dat >= xmin]
    negloglike = function(par) {
      r = -con_pl_ll(x, par, xmin)
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