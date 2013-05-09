#' @rdname dist_cdf-methods
#' @aliases dist_cdf,displ-method
setMethod("dist_cdf",
          signature = signature(m="displ"),
          definition = function(m, 
                                q=NULL, 
                                lower_tail=TRUE,
                                all_values=FALSE) {
            
            xmin = m$getXmin(); pars = m$getPars()
            if(is.null(pars)) stop("Parameters not set")  

            if(all_values) {
              inter = m$internal
              xmax = max(m$dat)
              v = ifelse(xmin==1, 0, sum((1:(xmin-1))^-pars))
              cumsum((((xmin:xmax)^-pars))/(inter[["constant"]] - v))                
            } else if(is.null(q)) {
              q = m$dat
              ppldis(q, xmin, pars, lower_tail)
            } else {
              ppldis(q, xmin, pars, lower_tail)
            } 
          }
)

setMethod("dist_cdf",
          signature = signature(m="dislnorm"),
          definition = function(m, 
                                q=NULL, 
                                lower_tail=TRUE,
                                all_values=FALSE) {
            xmin = m$getXmin(); pars = m$getPars()
            if(is.null(pars)) stop("Parameters not set")  
            
            if(all_values) {
              xmax = max(m$dat)
              q = xmin:xmax
            } else if(is.null(q)) {
              q = m$dat
              q = q[q >= xmin]
            } 
            p = plnorm(q + 0.5, pars[1], pars[2], lower.tail=lower_tail) 
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
            xmin = m$getXmin()
            pars = m$getPars()
            if(is.null(pars)) stop("Parameters not set")  

            if(all_values) {
              xmax = max(m$dat)
              q = xmin:xmax
            } else if(is.null(q)) {
              q = m$dat
              q = q[q>=xmin]
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

########################
##CTN distributions
########################
#' @rdname dist_cdf-methods
#' @aliases dist_cdf,conpl-method
setMethod("dist_cdf",
          signature = signature(m="conpl"),
          definition = function(m, 
                                q=NULL, 
                                lower_tail=TRUE,
                                all_values=FALSE) {
            xmin = m$xmin
            pars = m$pars
            if(is.null(pars)) stop("Parameters not set")  
            
            if(all_values) {
              xmax = max(m$dat)
              1 - (xmin:xmax/xmin)^(-pars + 1)
            } else if(is.null(q)) {
              q = m$dat
              pplcon(q[q >= xmin], xmin, pars, lower_tail)
            } else {
              pplcon(q[q >= xmin], xmin, pars, lower_tail)
            }
          }
)

#' @rdname dist_cdf-methods
#' @aliases dist_cdf,conlnorm-method
setMethod("dist_cdf",
          signature = signature(m="conlnorm"),
          definition = function(m, 
                                q=NULL, 
                                lower_tail=TRUE,
                                all_values=FALSE) {
            pars = m$pars; xmin = m$xmin
            if(is.null(pars)) stop("Parameters not set")  
            if(all_values) {
              xmax = max(m$dat)
              q = xmin:xmax
            } else if(is.null(q)) {
              q = m$dat
              q = q[q >= xmin]
            } 
            p = plnorm(q, pars[1], pars[2], lower.tail=lower_tail) 
            if(lower_tail) {
              C = plnorm(xmin, pars[1], pars[2], lower.tail=FALSE) 
              (p/C-1/C+1)
            } else {
              C = 1-plnorm(xmin, pars[1], pars[2]) 
              p/C
            }
          }
)
