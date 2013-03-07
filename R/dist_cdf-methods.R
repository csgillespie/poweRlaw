#' @rdname dist_cdf-methods
#' @aliases dist_cdf,displ-method
setMethod("dist_cdf",
          signature = signature(m="displ"),
          definition = function(m, 
                                q=NULL, 
                                lower.tail=TRUE,
                                cumulative=FALSE) {
              inter = m$internal
              if(cumulative) {
                xmin = m$xmin
                xmax = max(m$dat)
                  alpha = m$pars
                  v = ifelse(xmin==1, 0, sum((1:(xmin-1))^-alpha))
                  cumsum((((xmin:xmax)^-alpha))/(inter[["constant"]] - v))                
              } else if(is.null(q)) {
                  q = m$dat
                  ppldis(q, m$xmin, m$pars, lower.tail)
              } else {
                  ppldis(q, m$xmin, m$pars, lower.tail)
              } 
          }
          
)

#' @rdname dist_cdf-methods
#' @aliases dist_cdf,conpl-method
setMethod("dist_cdf",
          signature = signature(m="conpl"),
          definition = function(m, 
                                q=NULL, 
                                lower.tail=TRUE,
                                cumulative=FALSE) {
            if(cumulative) {
              xmin = m$xmin
              alpha = m$pars
              xmax = max(m$dat)
              1 - (xmin:xmax/xmin)^(-alpha + 1)
            } else if(is.null(q)) {
              q = m$dat
              pplcon(q[q >= m$xmin], m$xmin, m$pars, lower.tail)
            } else {
              pplcon(q[q >= m$xmin], m$xmin, m$pars, lower.tail)
            }
          }
)

