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

setMethod("dist_cdf",
          signature = signature(m="dislnorm"),
          definition = function(m, 
                                q=NULL, 
                                lower_tail=TRUE,
                                all_values=FALSE) {
            pars = m$pars; xmin = m$xmin
            
            if(all_values) {
              xmin = m$xmin; xmax = max(m$dat)
              q = xmin:xmax
            } else if(is.null(q)) {
              q = m$dat
            } 
            p = plnorm(q-0.5, pars[1], pars[2], lower.tail=lower_tail) 
            if(lower_tail) {
              C = plnorm(xmin-0.5, pars[1], pars[2], lower.tail=FALSE) 
              (p/C-1/C+1)
            } else {
              C = 1-plnorm(xmin-0.5, pars[1], pars[2]) 
              p/C
          }
          }
)

setMethod("dist_cdf",
          signature = signature(m="dispois"),
          definition = function(m, 
                                q=NULL, 
                                lower_tail=TRUE,
                                all_values=FALSE) {
            pars = m$pars;  xmin = m$xmin
            if(all_values) {
              xmin = m$xmin; xmax = max(m$dat)
              q = xmin:xmax
            } else if(is.null(q)) {
              q = m$dat
            } 
            p = ppois(q, pars, lower.tail=lower_tail) 
            if(lower_tail){
              C = ppois(xmin-1, pars, lower.tail=FALSE) 
              (p/C-1/C+1)
            } else {
              C = 1-ppois(xmin, pars)
              p/C
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

