#############################################################
#Reference Class definition
#############################################################
#' @rdname displ
#' @aliases conlnorm-class conlnorm
#' @exportClass conlnorm
#' @importFrom stats dlnorm plnorm
#' @export conlnorm
conlnorm = 
  setRefClass("conlnorm", 
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
conlnorm$methods( 
  list(
    initialize = function(dat) {
      no_pars <<- 2
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
#' @aliases dist_pdf,conlnorm-method
setMethod("dist_pdf",
          signature = signature(m="conlnorm"),
          definition = function(m, q=NULL, log=FALSE) {
            xmin = m$getXmin(); pars = m$getPars()
            if(is.null(q)) q = m$dat

            pdf = dlnorm(q, pars[1], pars[2], log=TRUE) - 
              plnorm(xmin, pars[1], pars[2], lower.tail=FALSE, log.p=TRUE)
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
#' @aliases dist_cdf,conlnorm-method
setMethod("dist_cdf",
          signature = signature(m="conlnorm"),
          definition = function(m, q=NULL, lower_tail=TRUE) {
            pars = m$pars; xmin = m$xmin
            if(is.null(pars)) stop("Model parameters not set.")  
            if(is.null(q)) q = m$dat
            
            if(lower_tail) {
              p = plnorm(q, pars[1], pars[2], lower.tail=lower_tail) 
              C = plnorm(xmin, pars[1], pars[2], lower.tail=FALSE) 
              pdf = (p/C-1/C+1)
            } else {
              log_p = plnorm(q, pars[1], pars[2], lower.tail=FALSE, log.p=TRUE)
              log_C = plnorm(xmin, pars[1], pars[2], lower.tail=FALSE, log.p=TRUE) 
              pdf = exp(log_p - log_C)
            }
            pdf[q < xmin] = 0
            pdf
          }
)

#' @rdname dist_cdf-methods
#' @aliases dist_all_cdf,conlnorm-method
setMethod("dist_all_cdf",
          signature = signature(m="conlnorm"),
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
#' @aliases dist_ll,conlnorm-method
setMethod("dist_ll",
          signature = signature(m="conlnorm"),
          definition = function(m) {
            q = m$dat
            n = m$internal[["n"]]; N = length(q)
            q = q[(N-n+1):N]
            
            conlnorm_tail_ll(q, m$getPars(), m$getXmin())
          }
)


########################################################
#Log-likelihood 
########################################################
conlnorm_tail_ll = function(x, pars, xmin) {
  if(is.vector(pars)) pars = t(as.matrix(pars))
  n = length(x)
  joint_prob = colSums(apply(pars, 1, 
                             function(i) dlnorm(x, i[1], i[2], log=TRUE)))
  
  prob_over = apply(pars, 1, function(i) 
    plnorm(xmin, i[1], i[2], log.p=TRUE, lower.tail=FALSE))
  joint_prob - n*prob_over
  
}


########################################################
#Rand number generator
########################################################
#' @rdname dist_rand-methods
#' @aliases dist_rand,conlnorm-method
setMethod("dist_rand",
          signature = signature(m="conlnorm"),
          definition = function(m, n="numeric") {
            xmin = m$getXmin(); pars = m$getPars()
            rns = numeric(n)
            i = 0; N = 0
            ## n-0.5 to avoid floating point sillyness.
            while (i < (n-0.5)) {
              ## Since we reject RNs less than xmin we should simulate N > n rns
              ## If we simulate N Rns (below), we will keep n-i (or reject N-(n-i))
              N = ceiling((n-i)/plnorm(xmin, pars[1L], pars[2L], lower.tail=FALSE))
              
              ## Simple rejection sampler
              x = rlnorm(N, pars[1L], pars[2L])
              x = x[x > xmin]
              if(length(x)) {
                x = x[1:min(length(x), n-i)]
                rns[(i+1L):(i+length(x))] = x
                i = i + length(x)
              }
            }
            rns
          }
)

#############################################################
#MLE method
#############################################################
conlnorm$methods(
  mle = function(set = TRUE, initialise=NULL) {
    x = dat
    x = x[x > xmin]
    if(is.null(initialise))
      theta_0 = c(mean(log(x)), sd(log(x)))
    else 
      theta_0 = initialise
    # Chop off values below 
    negloglike = function(par) {
      r = -conlnorm_tail_ll(x, par, xmin)
      if(!is.finite(r)) r = 1e12
      r
    }
    mle = suppressWarnings(optim(par=theta_0, 
                                 fn=negloglike, 
                                 method="L-BFGS-B", 
                                 lower=c(-Inf, .Machine$double.eps)))
    if(set)
      pars <<- mle$par
    class(mle) = "estimate_pars"
    names(mle)[1L] = "pars"
    mle
    
  }
)
