#' @rdname dist_cdf-methods
#' @aliases dist_cdf,displ-method
setMethod("dist_cdf",
          signature = signature(m="displ"),
          definition = function(m, q=NULL, lower.tail=FALSE) {
            if(is.null(q)) q = m$pl_data$x
            ppldis(q[q >= m$xmin], m$xmin, m$pars, TRUE)
          }
)
