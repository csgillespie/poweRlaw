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

#' @rdname dist_rand-methods
#' @aliases dist_rand,dislnorm-method
setMethod("dist_rand",
          signature = signature(m="dislnorm"),
          definition = function(m, n="numeric") {
            result <- numeric()
            min <- log(m$xmin)
            while (length(result) < n) {
              candidates <- rnorm(n, m$pars[1], m$pars[2])
              result <- c(result, candidates[candidates >= min])
            }
            floor(exp(result))
          }
)

#' @rdname dist_rand-methods
#' @aliases dist_rand,disexp-method
setMethod("dist_rand",
          signature = signature(m="disexp"),
          definition = function(m, n="numeric") {
            result <- numeric()
            while (length(result) < n) {
              candidates <- rexp(n, m$pars)
              result <- c(result, candidates[candidates >= m$xmin])
            }
            floor(result)
          }
)