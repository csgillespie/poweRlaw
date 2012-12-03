#' @rdname dist_ll-methods
#' @aliases dist_ll,displ-method
setMethod("dist_ll",
          signature = signature(m="displ"),
          definition = function(m) {
            con = m$constant
            if(m$xmin > 2) 
              con = con - colSums(vapply(m$pars, 
                              function(i) m$v^(-i), double(m$xmin-1)))
            else if(m$xmin > 1)
              con = con - 1
            log_con = log(con)
            if(is.nan(log_con)) return(-Inf)
            -m$n*log_con - m$slx*m$pars
          }
)
 