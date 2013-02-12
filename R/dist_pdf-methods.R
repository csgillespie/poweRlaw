#' @rdname dist_pdf-methods
#' @aliases dist_pdf,displ-method
setMethod("dist_pdf",
          signature = signature(m="displ"),
          definition = function(m, q=NULL) {
              if(is.null(q)) q = m$pl_data$x
              dpldis(q[q >= m$xmin], m$xmin, m$pars, TRUE)
          }
)