#' @rdname dist_ll-methods
#' @aliases dist_ll,displ-method
setMethod("dist_ll",
          signature = signature(m="displ"),
          definition = function(m) {
            inter = m$internal
            con = inter[["constant"]]
            if(m$xmin > 2) 
              con = con - 
              colSums(vapply(m$pars, 
                             function(i) inter[["v"]]^(-i), double(m$xmin-1)))
            else if(m$xmin > 1)
              con = con - 1
            
            log_con = log(con)
            ll = -inter[["n"]]*log_con - inter[["slx"]]*m$pars
            ll[is.nan(log_con)] = -Inf
            ll
          }
)

#' @rdname dist_ll-methods
#' @aliases dist_ll,conpl-method
setMethod("dist_ll",
          signature = signature(m="conpl"),
          definition = function(m) {
            n = m$internal[["n"]]
            slx = m$internal[["slx"]]
            n*log(m$pars-1) - n*log(m$xmin) - m$pars *(slx-n*log(m$xmin))
          }
)

#' @rdname dist_ll-methods
#' @aliases dist_ll,dislnorm-method
setMethod("dist_ll",
  signature = signature(m="dislnorm"),
  definition = function(m) {
    #threshold = 0
    n = m$internal[["n"]]
    xmin = m$getXmin()
    pars = m$getPars()
    meanlog = pars[1]; sdlog = pars[2] 
    JointProb = sum(dlnorm.disc(m$getDat(), meanlog, sdlog, log=TRUE))
    ProbOverThreshold = plnorm(xmin-0.5, 
                               meanlog, sdlog, 
                               lower.tail=FALSE, log.p=TRUE)
    return(JointProb - n*ProbOverThreshold)
  }
)
