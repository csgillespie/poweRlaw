#' The pl class
#' 
#' A data class. 
#' @name pl_data-class
#' @aliases pl_data
#' @docType class
#' @exportClass pl_data
#' @export pl_data
pl_data = setRefClass("pl_data", 
                      fields = list(x="numeric", 
                                    values="numeric", 
                                    freq="numeric"))



#' The distribution class
#' 
#' Distributions use this class for plotting
#' @name distribution
#' @aliases distribution-class
#' @docType class
#' @exportClass distribution
#' @export distribution
distribution = setRefClass("distribution", 
                           fields=list(
                             datatype="character", 
                             internal = "list", 
                             dat = "ANY",
                             xmin = "ANY", 
                             pars="ANY"), 
                           
)
distribution$accessors(c("xmin", "pars", "dat"))

#' Maximum likelihood estimation of the discrete power law distribution.
#' 
#' @return the mle estimate of the parameter alpha (for fixed xmin).
#' @param x data vector
#' @param xmin Lower bound of the power-law distribution. For the continuous 
#' power-law, xmin >= 0 for the discrete distribution, xmin >0
#' @param alpha The scaling parameter: alpha > 1
#' @aliases displ-class
#' @docType class
#' @aliases conpl
#' @importFrom VGAM zeta
#' @exportClass displ 
#' @export displ
#' @examples
#' data(moby)
displ = 
  setRefClass("displ", 
              contains="distribution",
              fields = list(
                dat = function(x)
                  if(!missing(x) && !is.null(x)) {
                    freq = internal[["freq"]]
                    values = internal[["values"]]
                    
                    internal[["cum_slx"]] <<-
                      rev(cumsum(log(rev(values))*rev(freq)))
                    internal[["cum_n"]] <<- rev(cumsum(rev(freq)))
                    internal[["dat"]] <<- x
                    #                     
                    xmin <<- internal[["xmin"]]
                    pars <<- internal[["pars"]]
                  } else internal[["dat"]],
                xmin = function(x) {
                  if(!missing(x) && !is.null(x)) {
                    if(class(x) == "ks_est") {
                      pars <<- x$pars
                      x = x$xmin
                    }
                    internal[["xmin"]] <<- x
                    internal[["v"]] <<- 1:(x-1)
                    
                    ##Copying the data results in floating point
                    ##Comparsion errors - need to add 
                    ##Machine precision :(
                    selection = min(which(
                      internal[["values"]] >= x))
                    internal[["slx"]] <<- internal[["cum_slx"]][selection]
                    internal[["n"]] <<- internal[["cum_n"]][selection]    
                  } else  internal[["xmin"]]
                }, 
                pars = function(x) {
                  if (!missing(x) && !is.null(x)) {
                    internal[["pars"]] <<- x
                    internal[["constant"]] <<- zeta(x)
                  } else internal[["pars"]]
                }
              ))

#' @rdname displ
#' @aliases conpl-class
#' @exportClass conpl
#' @export conpl
conpl = 
  setRefClass("conpl", 
              contains="distribution",
              fields = list(dat = function(x)
                if(!missing(x) && !is.null(x)) {
                  internal[["cum_slx"]] <<-
                    rev(cumsum(log(rev(x$values))*rev(x$freq)))
                  internal[["cum_n"]] <<- rev(cumsum(rev(x$freq)))
                  internal[["dat"]] <<- x
                  
                  ##Note sure if these two lines are needed?
                  xmin <<- internal[["xmin"]]
                  pars <<- internal[["pars"]]
                } else internal[["dat"]],
                  xmin = function(x) {
                   if(!missing(x) && !is.null(x)) {
                     if(class(x) == "ks_est") {
                       pars <<- x$pars
                       x = x$xmin
                     }
                     internal[["xmin"]] <<- x
                     selection = min(which(internal[["dat"]] >= (x- .Machine$double.eps ^ 0.5)))
                     internal[["slx"]] <<- internal[["cum_slx"]][selection]
                     internal[["n"]] <<- internal[["cum_n"]][selection]                                
                     } else  internal[["xmin"]]
                   }, 
                  pars = function(x) {
                    if (!missing(x) && !is.null(x)) {
                      internal[["pars"]] <<- x
                    } else internal[["pars"]]
                }
          )
  )























