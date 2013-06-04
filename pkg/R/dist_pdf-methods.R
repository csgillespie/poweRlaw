#' @rdname dist_pdf-methods
#' @aliases dist_pdf,displ-method
setMethod("dist_pdf",
          signature = signature(m="displ"),
          definition = function(m, q=NULL) {
              xmin = m$getXmin(); pars = m$getPars()
              if(is.null(q)) q = m$dat
              q = q[q >= m$xmin]
              dpldis(q[q >= m$xmin], m$xmin, m$pars, TRUE)
          }
)

#' @rdname dist_pdf-methods
#' @aliases dist_pdf,dislnorm-method
setMethod("dist_pdf",
          signature = signature(m="dislnorm"),
          definition = function(m, q=NULL) {
            xmin = m$getXmin(); pars = m$getPars()
            if(is.null(q)) q = m$dat
            q = q[q >= m$xmin]
            log(plnorm(q-0.5, pars[1], pars[2], lower.tail=FALSE) -
                  plnorm(q+0.5, pars[1], pars[2], lower.tail=FALSE)) - 
              plnorm(xmin-0.5, pars[1], pars[2], lower.tail=FALSE, log.p=TRUE)
            
            
          }
)


#' @rdname dist_pdf-methods
#' @aliases dist_pdf,dispois-method
setMethod("dist_pdf",
          signature = signature(m="dispois"),
          definition = function(m, q=NULL) {
            xmin = m$getXmin(); pars = m$getPars()
            if(is.null(q)) q = m$dat
            q = q[q >= m$xmin]
            dpois(q, pars, log=TRUE) - 
              ppois(xmin, pars, lower.tail=FALSE, log.p=TRUE)
          }
)

#' @rdname dist_pdf-methods
#' @aliases dist_pdf,disexp-method
setMethod("dist_pdf",
          signature = signature(m="disexp"),
          definition = function(m, q=NULL) {
            xmin = m$getXmin(); pars = m$getPars()
            if(is.null(q)) q = m$dat
            q = q[q >= m$xmin]
            log(pexp(q-0.5, pars, lower.tail=FALSE) -
                  pexp(q+0.5, pars, lower.tail=FALSE)) - 
              pexp(xmin-0.5, pars, lower.tail=FALSE, log.p=TRUE)
            
          }
)





##################################################################
##CTN distributions
##################################################################
#' @rdname dist_pdf-methods
#' @aliases dist_pdf,conpl-method
setMethod("dist_pdf",
          signature = signature(m="conpl"),
          definition = function(m, q=NULL) {
            xmin = m$getXmin(); pars = m$getPars()
            if(is.null(q)) {
              q = m$dat
              n = m$internal[["n"]]; N = length(q)
              q = q[(N-n+1):N]
            } else {
              q[q >= xmin]
            }
            dplcon(q, xmin, pars, TRUE)
          }
)




#' @rdname dist_pdf-methods
#' @aliases dist_pdf,conlnorm-method
setMethod("dist_pdf",
          signature = signature(m="conlnorm"),
          definition = function(m, q=NULL) {
            xmin = m$getXmin(); pars = m$getPars()
            if(is.null(q)) {
              q = m$dat
              n = m$internal[["n"]]; N = length(q)
              q = q[(N-n+1):N]
            } else {
              q[q >= m$xmin]
            }
            dlnorm(q, pars[1], pars[2], log=TRUE) - 
              plnorm(xmin, pars[1], pars[2], lower.tail=FALSE, log.p=TRUE)
            
          }
)



#' @rdname dist_pdf-methods
#' @aliases dist_pdf,conexp-method
setMethod("dist_pdf",
          signature = signature(m="conexp"),
          definition = function(m, q=NULL) {
            xmin = m$getXmin(); pars = m$getPars()
            
            if(is.null(q)) {
              q = m$dat
              n = m$internal[["n"]]; N = length(q)
              q = q[(N-n+1):N]
            } else {
              q[q >= m$xmin]
            }
            dexp(q, pars, log=TRUE) - 
              pexp(xmin, pars, lower.tail=FALSE, log.p=TRUE)
          }
)
