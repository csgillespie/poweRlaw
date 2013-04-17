#' @rdname dist_cdf-methods
#' @aliases dist_cdf,displ-method
setMethod("dist_cdf",
          signature = signature(m="displ"),
          definition = function(m, 
                                q=NULL, 
                                lower_tail=TRUE,
                                all_values=FALSE) {
              inter = m$internal
              if(all_values) {
                xmin = m$xmin
                xmax = max(m$dat)
                  alpha = m$pars
                  v = ifelse(xmin==1, 0, sum((1:(xmin-1))^-alpha))
                  cumsum((((xmin:xmax)^-alpha))/(inter[["constant"]] - v))                
              } else if(is.null(q)) {
                  q = m$dat
                  ppldis(q, m$xmin, m$pars, lower_tail)
              } else {
                  ppldis(q, m$xmin, m$pars, lower_tail)
              } 
          }
)

#' @rdname dist_cdf-methods
#' @aliases dist_cdf,conpl-method
setMethod("dist_cdf",
          signature = signature(m="conpl"),
          definition = function(m, 
                                q=NULL, 
                                lower_tail=TRUE,
                                all_values=FALSE) {
            if(all_values) {
              xmin = m$xmin
              alpha = m$pars
              xmax = max(m$dat)
              1 - (xmin:xmax/xmin)^(-alpha + 1)
            } else if(is.null(q)) {
              q = m$dat
              pplcon(q[q >= m$xmin], m$xmin, m$pars, lower_tail)
            } else {
              pplcon(q[q >= m$xmin], m$xmin, m$pars, lower_tail)
            }
          }
)

