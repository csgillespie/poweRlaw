#' @rdname dist_ll-methods
#' @aliases dist_ll,displ-method
setMethod("dist_ll",
          signature = signature(m="displ"),
          definition = function(m) {
              inter = m$internal
              con = inter[["constant"]]
              if(m$xmin > 2) 
                  con = con - colSums(vapply(m$pars, 
                                             function(i) inter[["v"]]^(-i), double(m$xmin-1)))
              else if(m$xmin > 1)
                  con = con - 1
              
              log_con = log(con)
              ll = -inter[["n"]]*log_con - inter[["slx"]]*m$pars
              ll[is.nan(log_con)] = -Inf
              ll
          }
)
<<<<<<< HEAD
 

setMethod("dist_ll",
          signature = signature(m="conpl"),
          definition = function(m) {
            m$n*log(m$pars-1) - m$n*log(m$xmin) - m$pars *(m$slx-log(m$xmin))
            
          }
)

=======
>>>>>>> master
