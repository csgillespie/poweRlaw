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
#' @aliases dist_ll,dislnorm-method
setMethod("dist_ll",
  signature = signature(m="dislnorm"),
  definition = function(m) {
    xmin = m$getXmin()
    d = m$getDat()
    disc_lnorm_tail_ll(d[d >= xmin], m$getPars(), xmin)
  }
)

#' @rdname dist_ll-methods
#' @aliases dist_ll,dislnorm-method
setMethod("dist_ll",
          signature = signature(m="dispois"),
          definition = function(m) {
            xmin = m$getXmin()
            d = m$getDat()
            pois_tail_ll(d[d >= xmin], m$getPars(), xmin)
          }
)


####
####CTN Distributions
####

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
#' @aliases dist_ll,conlnorm-method
setMethod("dist_ll",
          signature = signature(m="conlnorm"),
          definition = function(m) {
            xmin = m$getXmin()
            d = m$getDat()
            conlnorm_tail_ll(d[d >= xmin], m$getPars(), xmin)
          }
)





