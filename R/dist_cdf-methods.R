#' @rdname dist_cdf-methods
#' @aliases dist_cdf,displ-method
setMethod("dist_cdf",
          signature = signature(m="displ"),
          definition = function(m, q=NULL, lower.tail=TRUE) {
              if(is.null(q)) q = m$pl_data$x
              ppldis(q[q >= m$xmin], m$xmin, m$pars, lower.tail)
          }
)

setMethod("dist_cdf",
          signature = signature(m="conpl"),
          definition = function(m, q=NULL, lower.tail=TRUE) {
            if(is.null(q)) q = m$pl_data$x
            pplcon(q[q >= m$xmin], m$xmin, m$pars, lower.tail)
          }
)

