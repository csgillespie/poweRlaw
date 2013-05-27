#' Heavy-tailed distributions
#' 
#' The poweRlaw package supports a number of distributions:
#' \describe{
#' \item{displ}{Discrete power-law}
#' \item{dislnorm}{Discrete log-normal}
#' \item{dispois}{Discrete Poisson}
#' \item{conpl}{Continuous power-law}
#' \item{conlnorm}{Continuous log-normal}
#' \item{conexp}{Continuous exponential}}
#' Each object inherits the \code{discrete_distribution} or
#' \code{ctn_distribution} class. 
#'
#' @section Fields:
#' 
#' Each distribution object has four fields. However, the object 
#' is typically created by passing 
#' data, using the \code{dat} field. Each field has standard 
#' setters and getters.
#' \describe{
#' \item{dat}{The data set.}
#' \item{xmin}{The lower threshold, xmin. Typically set after initialisation. 
#' For the continuous  power-law, xmin >= 0 for the discrete 
#' distributions, xmin >0}
#' \item{pars}{A parameter vector. Typically set after initialisation. For power-laws
#' pars > 1. The lognormal distribution has two parameters.}
#' \item{internal}{A list. This list differs between objects and shouldn't be altered.}}
#' @param ... The object is typically created by passing 
#' data using the \code{dat} field. 
#' Each field has standard setters and getters.
#' 
#' @return a reference object
#' @aliases displ-class
#' @docType class
#' @aliases conpl
#' @importFrom VGAM zeta
#' @exportClass displ 
#' @export displ
#' @examples
#' data(moby)
#' m = displ$new(moby)
#' m$getXmin()
#' m$getPars()
#' #Set Xmin and parameter
#' m$setXmin(2)
#' m$setPars(2)
#' #Plot the data
#' plot(m)
#' #Add fitted distributions
#' lines(m)
displ = 
  setRefClass("displ", 
              contains="discrete_distribution",
              fields = list(
                dat = function(x)
                  if(!missing(x) && !is.null(x)) {
                    check_discrete_data(x)
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
                    check_discrete_data(x)
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
                    check_discrete_data(x)
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

