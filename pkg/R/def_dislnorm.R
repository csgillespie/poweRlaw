#############################################################
#Reference Class definition
#############################################################
#' @rdname displ
#' @aliases dislnorm-class dislnorm
#' @exportClass dislnorm
#' @export dislnorm
dislnorm = 
  setRefClass("dislnorm", 
              contains="discrete_distribution",
              fields = list(
                dat = function(x) {
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
                  } else internal[["dat"]]
                },
                xmin = function(x) {
                  if(!missing(x) && !is.null(x)) {
                    if("estimate_xmin" %in% class(x)) {
                      pars <<- x$pars
                      x = x$xmin
                    }
                    internal[["xmin"]] <<- x
                    if(length(internal[["values"]])) {
                      selection = min(which(internal[["values"]] >= x))
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

dislnorm$methods( 
  list(
    initialize = function(dat) {
      no_pars <<- 2
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
#' @aliases dist_pdf,dislnorm-method
setMethod("dist_pdf",
          signature = signature(m="dislnorm"),
          definition = function(m, q=NULL, log=FALSE) {
            xmin = m$getXmin(); pars = m$getPars()
            if(is.null(q)) q = m$dat
            
            l1 = plnorm(q-0.5, pars[1], pars[2], lower.tail=FALSE, log.p=TRUE)
            l2 = plnorm(q+0.5, pars[1], pars[2], lower.tail=FALSE, log.p=TRUE)
            
            pdf = l1 + log(1-exp(l2-l1)) - 
              plnorm(xmin-0.5, pars[1], pars[2], lower.tail=FALSE, log.p=TRUE)
            
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
#' @aliases dist_cdf,dislnorm-method
setMethod("dist_cdf",
          signature = signature(m="dislnorm"),
          definition = function(m, q=NULL, lower_tail=TRUE) {
            xmin = m$getXmin(); pars = m$getPars()
            if(is.null(pars)) stop("Model parameters not set.")  
            if(is.null(q)) q = m$dat
            
            ## lower_tail == TRUE numerical unstable
            ## Not sure how best to fix it
            if(lower_tail) {
              p = plnorm(q + 0.5, pars[1], pars[2], lower.tail=lower_tail) 
              C = plnorm(xmin-0.5, pars[1], pars[2], lower.tail=FALSE) 
              cdf = (p/C-1/C+1)
            } else {
              log_p = plnorm(q + 0.5, pars[1], pars[2], lower.tail=FALSE, log.p=T) 
              log_C = plnorm(xmin+0.5, pars[1], pars[2], lower.tail=FALSE, log.p=T) 
              cdf = exp(log_p-log_C)
            }
            cdf[q < xmin] = 0
            cdf
          }
)

#' @rdname dist_cdf-methods
#' @aliases dist_all_cdf,dislnorm-method
setMethod("dist_all_cdf",
          signature = signature(m="dislnorm"),
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
#' @aliases dist_ll,dislnorm-method
setMethod("dist_ll",
          signature = signature(m="dislnorm"),
          definition = function(m) {
            xmin = m$getXmin()
            d = m$getDat()
            dis_lnorm_tail_ll(d[d >= xmin], m$getPars(), xmin)
          }
)
########################################################
#Log-likelihood 
########################################################
dis_lnorm_tail_ll = function(x, pars, xmin) {
  if(is.vector(pars)) pars = t(as.matrix(pars))
  n = length(x)
  p = function(par){
    m_log = par[1]; sd_log = par[2]
    plnorm(x-0.5, m_log, sd_log, lower.tail=FALSE) - 
      plnorm(x+0.5, m_log, sd_log, lower.tail=FALSE)
  }
  joint_prob = colSums(log(apply(pars, 1, p)))
  prob_over = apply(pars, 1, function(i) 
    plnorm(xmin-0.5, i[1], i[2], 
           lower.tail=FALSE, log.p=TRUE))
  
  return(joint_prob - n*prob_over)
}

########################################################
#Rand number generator
########################################################
#' @rdname dist_rand-methods
#' @aliases dist_rand,dislnorm-method
setMethod("dist_rand",
          signature = signature(m="dislnorm"),
          definition = function(m, n="numeric") {
            xmin = m$getXmin(); pars = m$getPars()
            lower = xmin - 0.5
            rns = numeric(n)
            i = 0; N = 0
            ## n-0.5 to avoid floating point sillyness.
            while (i < (n-0.5)) {
              ## Since we reject RNs less than lower=xmin - 0.5 we should simulate >> n rns
              ## If we simulate N Rns (below), we will keep n-i (or reject N-(n-i))
              N = ceiling((n-i)/plnorm(lower, pars[1L], pars[2L], lower.tail=FALSE))
              
              ## Simple rejection sampler
              x = rlnorm(N, pars[1L], pars[2L])
              x = x[x >= lower]
              if(length(x)) {
                x = x[1:min(length(x), n-i)]
                rns[(i+1L):(i+length(x))] = x
                i = i + length(x)
              }
            }
            
            ##Round at end (more efficient)
            round(rns)
          }
)

#############################################################
#MLE method
#############################################################
dislnorm$methods(
  mle = function(set = TRUE, initialise=NULL) {
    n = internal[["n"]]
    x = dat
    x = x[x > (xmin-0.5)]
    x.log = log(x)
    if(is.null(initialise))
      theta_0 = c(mean(x.log), sd(x.log))
    else 
      theta_0 = initialise
    # Chop off values below 
    negloglike = function(par) {
      r = -dis_lnorm_tail_ll(x, par, xmin)
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