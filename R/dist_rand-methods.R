#' @rdname dist_rand-methods
#' @aliases dist_rand,displ-method
setMethod("dist_rand",
          signature = signature(m="displ"),
          definition = function(m, n="numeric") {
            rpldis(n, m$xmin, m$pars)
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
