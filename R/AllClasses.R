#' The pl class
#' 
#' A data class
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
                             pl_data = "ANY", 
                             internal = "list", 
                             xmin = "ANY", 
                             pars="ANY"))
distribution$accessors(c("xmin", "pars", "pl_data"))

#' Maximum likelihood estimation of the discrete power law distribution.
#' 
#' @return the mle estimate of the parameter alpha (for fixed xmin).
#' @param x data vector
#' @param xmin Lower bound of the power-law distribution. For the continuous 
#' power-law, xmin >= 0 for the discrete distribution, xmin >0
#' @param alpha The scaling parameter: alpha > 1
#' @aliases displ-class
#' @docType class
#' @importFrom VGAM zeta
#' @exportClass displ 
#' @export displ
#' @examples
#' data(moby)
#' pl_data = pl_data$new(moby)
#' m = displ$new(pl_data)
#' m$setXmin(7)
#' m$setPars(5)
#' dist_ll(m) #-16945
#' estimate_pars(m) #1.952
#' m$setPars(estimate_pars(m))
#' plot(pl_data)
#' plot(m)
#' lines(m)
displ = 
  setRefClass("displ", 
              contains="distribution",
              fields = list(pl_data = function(x)
                if(!missing(x) && !is.null(x)) {
                  internal[["cum_slx"]] <<-
                    rev(cumsum(log(rev(x$values))*rev(x$freq)))
                  internal[["cum_n"]] <<- rev(cumsum(rev(x$freq)))
                  internal[["pl_data"]] <<- x
                  
                  xmin <<- internal[["xmin"]]
                  pars <<- internal[["pars"]]
                } else internal[["pl_data"]],
<<<<<<< HEAD
                            xmin = function(x) {
                              if(!missing(x) && !is.null(x)) {
                                internal[["xmin"]] <<- x
                                .self$v = 1:(x-1)
                                selection = min(which(pl_data$values >= x))
                                .self$slx = internal[["cum_slx"]][selection]
                                .self$n = internal[["cum_n"]][selection]    
                              } else  internal[["xmin"]]
                            }, pars = function(x) {
                              if (!missing(x) && !is.null(x)) {
                                internal[["pars"]] <<- x
                                .self$constant = zeta(x)
                              } else internal[["pars"]]
                            }
=======
                xmin = function(x) {
                  if(!missing(x) && !is.null(x)) {
                    internal[["xmin"]] <<- x
                    internal[["v"]] <<- 1:(x-1)
                    selection = min(which(pl_data$values >= x))
                    internal[["slx"]] <<- internal[["cum_slx"]][selection]
                    internal[["n"]] <<- internal[["cum_n"]][selection]    
                  } else  internal[["xmin"]]
                }, pars = function(x) {
                  if (!missing(x) && !is.null(x)) {
                    internal[["pars"]] <<- x
                    internal[["constant"]] <<- zeta(x)
                  } else internal[["pars"]]
                }
>>>>>>> master
              ))


#' @exportClass conpl
#' @export conpl
conpl = 
  setRefClass("conpl", 
              contains="distribution",
              fields = list(pl_data = function(x)
                if(!missing(x) && !is.null(x)) {
                  internal[["cum_slx"]] <<-
                    rev(cumsum(log(rev(x$values))*rev(x$freq)))
                  internal[["cum_n"]] <<- rev(cumsum(rev(x$freq)))
                  internal[["pl_data"]] <<- x
                  
                  xmin <<- internal[["xmin"]]
                  pars <<- internal[["pars"]]
                } else internal[["pl_data"]],
                            xmin = function(x) {
                              if(!missing(x) && !is.null(x)) {
                                internal[["xmin"]] <<- x
                                selection = min(which(pl_data$values >= x))
                                .self$slx = internal[["cum_slx"]][selection]
                                .self$n = internal[["cum_n"]][selection]                                
                              } else  internal[["xmin"]]
                            }, pars = function(x) {
                              if (!missing(x) && !is.null(x)) {
                                internal[["pars"]] <<- x
                              } else internal[["pars"]]
                            }
              )
  )























