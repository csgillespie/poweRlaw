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
            result <- numeric(n)
            min <- round(m$xmin)
            i <- 1
            while (i <= n) {
              candidates <- round(rlnorm(n, m$pars[1], m$pars[2]))
              candidates <- candidates[candidates >= min]
              for (c in candidates) {
                result[i] <- c
                i <- i + 1
              }
            }
            result
          }
)

#' @rdname dist_rand-methods
#' @aliases dist_rand,disexp-method
setMethod("dist_rand",
          signature = signature(m="disexp"),
          definition = function(m, n="numeric") {
            u = runif(n, 0, exp(-m$pars*(m$xmin-0.5)))
            round(-log(u)/m$pars)
          }
)