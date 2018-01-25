#############################################################
#Reference Class definition
#############################################################
#' @rdname displ
#' @aliases dispois-class dispois
#' @exportClass dispois
#' @importFrom stats dpois ppois
#' @export dispois
dispois = 
  setRefClass("dispois", 
              contains="discrete_distribution",
              fields = list(
                dat = function(x)
                  if(!missing(x) && !is.null(x)) {
                    check_discrete_data(x)
                    x = sort(x)
                    tab = table(x)
                    values = as.numeric(names(tab))
                    freq = as.vector(tab)
                    
                    internal[["cum_n"]] <<- rev(cumsum(rev(freq)))
                    internal[["freq"]] <<- freq
                    internal[["values"]] <<- values
                    internal[["dat"]] <<- x
                    xmin <<- min(values)
                  } else internal[["dat"]],
                xmin = function(x) {
                  if(!missing(x) && !is.null(x)) {
                    if("estimate_xmin" %in% class(x)) {
                      pars <<- x$pars
                      x = x$xmin
                    }
                    internal[["xmin"]] <<- x
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
                  } else internal[["pars"]]
                }
              ))

#############################################################
#Initialisation
#############################################################
dispois$methods( 
  list(
    initialize = function(dat) {
      no_pars <<- 1
      ##Use the internal attribute for copying
      if(!missing(dat)) {
        check_discrete_data(dat)
        x = sort(dat)
        tab = table(x)
        values = as.numeric(names(tab))
        freq = as.vector(tab)
        
        internal[["cum_n"]] <<- rev(cumsum(rev(freq)))
        internal[["freq"]] <<- freq
        internal[["values"]] <<- values
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
#' @aliases dist_pdf,dispois-method
setMethod("dist_pdf",
          signature = signature(m="dispois"),
          definition = function(m, q=NULL, log=FALSE) {
            xmin = m$getXmin(); pars = m$getPars()
            if(is.null(q)) q = m$dat
            pdf = dpois(q, pars, log=TRUE) - ppois(xmin-0.5, pars, lower.tail=FALSE, log.p=TRUE)
            if(!log) {
              pdf = exp(pdf)
              pdf[q < xmin]  = 0
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
#' @aliases dist_cdf,dispois-method
setMethod("dist_cdf",
          signature = signature(m="dispois"),
          definition = function(m, q=NULL, lower_tail=TRUE) {
            xmin = m$getXmin(); pars = m$getPars()
            if(is.null(pars)) stop("Model parameters not set.")  
            if(is.null(q)) q = m$dat

            p = ppois(q, pars, lower.tail=lower_tail) 
            if(lower_tail){
              C = ppois(xmin-0.5, pars, lower.tail=FALSE) 
              cdf = (p/C-1/C+1)
            } else {
              C = 1-ppois(xmin, pars)
              cdf = p/C
            }
            cdf[q < xmin] = 0
            cdf
          }
)

#' @rdname dist_cdf-methods
#' @aliases dist_all_cdf,dispois-method
setMethod("dist_all_cdf",
          signature = signature(m="dispois"),
          definition = function(m, lower_tail=TRUE, xmax=1e5) {
            xmin = m$getXmin()
            xmax = max(m$dat[m$dat <= xmax])
            dist_cdf(m, q=xmin:xmax, lower_tail=lower_tail)
          }
)






#############################################################
#ll method
#############################################################
#' @rdname dist_ll-methods
#' @aliases dist_ll,dispois-method
setMethod("dist_ll",
          signature = signature(m="dispois"),
          definition = function(m) {
            xmin = m$getXmin()
            d = m$getDat()
            pois_tail_ll(d[d >= xmin], m$getPars(), xmin)
          }
)


########################################################
#Log-likelihood 
########################################################
pois_tail_ll = function(x, rate, xmin) {
  n = length(x)
  joint_prob = colSums(sapply(rate, function(i) dpois(x, i, log=TRUE)))
  prob_over = sapply(rate, function(i) ppois(xmin-1, i, 
                                             lower.tail=FALSE, log.p=TRUE))
  return(joint_prob - n*prob_over)
}


########################################################
#Rand number generator
########################################################
# Adapted from https://stat.ethz.ch/pipermail/r-help/2005-May/070680.html
#' @rdname dist_rand-methods
#' @aliases dist_rand,dispois-method
setMethod("dist_rand",
          signature = signature(m="dispois"),
          definition = function(m, n="numeric") {
            xmin = m$xmin; lambda = m$pars
            qpois(runif(n, ppois(xmin-1, lambda, lower.tail=T), 1), lambda)
          }
)


#############################################################
#MLE method
#############################################################
dispois$methods(
  mle = function(set = TRUE, initialise=NULL) {
    x = dat
    x = x[x > (xmin-0.5)]
    if(is.null(initialise))
      theta_0 = mean(x)
    else 
      theta_0 = initialise
    # Chop off values below 
    negloglike = function(par) {
      r = -pois_tail_ll(x, par, xmin)
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