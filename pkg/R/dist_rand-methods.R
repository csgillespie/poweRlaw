#' @rdname dist_rand-methods
#' @aliases dist_rand,displ-method
setMethod("dist_rand",
          signature = signature(m="displ"),
          definition = function(m, n="numeric") {
            rpldis(n, m$xmin, m$pars)
          }
)


#' @rdname dist_rand-methods
#' @aliases dist_rand,dislnorm-method
setMethod("dist_rand",
          signature = signature(m="dislnorm"),
          definition = function(m, n="numeric") {
            xmin = m$getXmin(); pars = m$getPars()
            lower = xmin - 0.5
            rns = numeric(n)
            i = 1; N = 0
            ## n-0.5 to avoid floating point sillyness.
            while (i <= (n-0.5)) {
              ## Since we reject RNs less than lower=xmin - 0.5 we should simulate >> n rns
              ## If we simulate N Rns (below), we will keep n-i (or reject N-(n-i))
              N = ceiling((n-i)/plnorm(lower, pars[1L], pars[2L], lower.tail=FALSE))
              
              ## Simulate, then select, t
              x = rlnorm(N, pars[1L], pars[2L])
              x = x[x >= lower]
              if(length(x)) {
                x = x[1:min(length(x), n-i+1)]
                rns[i:(i+length(x)-1L)] = x
                i = i + length(x)
              }
            }
            
            ##Round at end (more efficient)
            round(rns)
          }
)


#' @rdname dist_rand-methods
#' @aliases dist_rand,disexp-method
setMethod("dist_rand",
          signature = signature(m="disexp"),
          definition = function(m, n="numeric") {
            ## Rearrange the usual formula for generating
            ## exp random numbers: -log(u)/lambda > xmin - 0.5
            u = runif(n, 0, exp(-m$pars*(m$xmin-0.5)))
            round(-log(u)/m$pars)
          }
)


#' @rdname dist_rand-methods
#' @aliases dist_rand,conpl-method
setMethod("dist_rand",
          signature = signature(m="conpl"),
          definition = function(m, n="numeric") {
            rplcon(n, m$xmin, m$pars)
          }
)
