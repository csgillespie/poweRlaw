
#' Discrete and continuous power-law objects
#' 
#' The reference objects \code{displ} and \code{conpl} are used 
#' for parameter inference for discrete and continuous power-laws.
#' Each class also inherits the \code{distribution} class. These
#' are the main classes of the poweRlaw package. 
#'
#' @return a reference object
#' @param x the data vector
#' @param xmin lower bound of the power-law distribution. 
#' For the continuous  power-law, xmin >= 0 for the discrete 
#' distributions, xmin >0
#' @param pars the scaling parameter: alpha > 1
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
              contains="discrete_distribution",
              fields = list(
                dat = function(x)
                  if(!missing(x) && !is.null(x)) {
                    x = sort(x)
                    #x= round(sort(x))
                    tab = table(x)
                    values = as.numeric(names(tab))
                    freq = as.vector(tab)
                    internal[["freq"]] <<- freq
                    internal[["values"]] <<- values
                    internal[["cum_slx"]] <<-
                      rev(cumsum(log(rev(values))*rev(freq)))
                    internal[["cum_n"]] <<- rev(cumsum(rev(freq)))
                    internal[["dat"]] <<- x
                    xmin <<- min(values)
                  } else internal[["dat"]],
                xmin = function(x) {
                  if(!missing(x) && !is.null(x)) {
                    if(class(x) == "estimate_xmin") {
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
                    if(class(x) == "estimate_pars") x = x$pars            
                    internal[["pars"]] <<- x
                    internal[["constant"]] <<- zeta(x)
                  } else internal[["pars"]]
                }
              ))
#' @rdname displ
#' @aliases dislnorm-class
#' @exportClass dislnorm
#' @export dislnorm
dislnorm = 
  setRefClass("dislnorm", 
              contains="discrete_distribution",
              fields = list(
                dat = function(x)
                  if(!missing(x) && !is.null(x)) {
                    x = sort(x)
                    tab = table(x)
                    values = as.numeric(names(tab))
                    freq = as.vector(tab)
                    
                    internal[["cum_n"]] <<- rev(cumsum(rev(freq)))
                    internal[["freq"]] <<- freq
                    internal[["values"]] <<- values
                    internal[["dat"]] <<- x
                    
                    xmin <<- min(values)
                  } else internal[["dat"]],
                xmin = function(x) {
                  if(!missing(x) && !is.null(x)) {
                    if(class(x) == "estimate_xmin") {
                      pars <<- x$pars
                      x = x$xmin
                    }
                    selection = min(which(
                      internal[["values"]] >= x))
                    internal[["n"]] <<- internal[["cum_n"]][selection]            
                    internal[["xmin"]] <<- x
                  } else  internal[["xmin"]]
                }, 
                pars = function(x) {
                  if (!missing(x) && !is.null(x)) {
                    if(class(x) == "estimate_pars") x = x$pars            
                    internal[["pars"]] <<- x            
                  } else internal[["pars"]]
                }
              ))


#' @rdname displ
#' @aliases dispois-class
#' @exportClass dispois
#' @export dispois
dispois = 
  setRefClass("dispois", 
              contains="discrete_distribution",
              fields = list(
                dat = function(x)
                  if(!missing(x) && !is.null(x)) {
                    x = sort(dat)
                    tab = table(x)
                    values = as.numeric(names(tab))
                    freq = as.vector(tab)
                    
                    internal[["cum_n"]] <<- rev(cumsum(rev(freq)))
                    internal[["freq"]] <<- freq
                    internal[["values"]] <<- values
                    internal[["dat"]] <<- x
                    xmin <<- min(values)
                  } else internal[["dat"]],
                xmin = function(x) {
                  if(!missing(x) && !is.null(x)) {
                    if(class(x) == "estimate_xmin") {
                      pars <<- x$pars
                      x = x$xmin
                    }
                    selection = min(which(
                      internal[["values"]] >= x))
                    internal[["slx"]] <<- internal[["cum_slx"]][selection]
                    internal[["n"]] <<- internal[["cum_n"]][selection]            
                    internal[["xmin"]] <<- x
                  } else  internal[["xmin"]]
                }, 
                pars = function(x) {
                  if (!missing(x) && !is.null(x)) {
                    if(class(x) == "estimate_pars") x = x$pars            
                    internal[["pars"]] <<- x            
                  } else internal[["pars"]]
                }
              ))

