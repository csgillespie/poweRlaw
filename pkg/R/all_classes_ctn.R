#' @rdname displ
#' @aliases conpl-class conpl
#' @exportClass conpl
#' @export conpl
conpl = 
  setRefClass("conpl", 
              contains="ctn_distribution",
              fields = list(
                dat = function(x) {
                  if(!missing(x) && !is.null(x)) {
                    check_ctn_data(x)
                    d = sort(x)
                    internal[["cum_slx"]] <<-
                      rev(cumsum(log(rev(d))))
                    internal[["cum_n"]] <<- length(d):1
                    internal[["dat"]] <<- sort(d)
                    xmin <<- d[1]
                  } else internal[["dat"]]
                },
                xmin = function(x) {
                  if(!missing(x) && !is.null(x)) {
                    if("estimate_xmin" %in% class(x)) {
                      pars <<- x$pars
                      x = x$xmin
                    }
                    internal[["xmin"]] <<- x
                    if(length(internal[["dat"]])) {
                      selection = min(which(internal[["dat"]] >= (x- .Machine$double.eps ^ 0.5)))
                      internal[["slx"]] <<- internal[["cum_slx"]][selection]
                      internal[["n"]] <<- internal[["cum_n"]][selection]
                    }
                  } else  internal[["xmin"]]
                }, 
                pars = function(x) {
                  if (!missing(x) && !is.null(x)) {
                    if("estimate_pars" %in% class(x)) x = x$pars
                    internal[["pars"]] <<- x
                  } else internal[["pars"]]
                }
              )
  )


#' @rdname displ
#' @aliases conlnorm-class conlnorm
#' @exportClass conlnorm
#' @export conlnorm
conlnorm = 
  setRefClass("conlnorm", 
              contains="ctn_distribution",
              fields = list(
                dat = function(x) {
                  if(!missing(x) && !is.null(x)) {
                    check_ctn_data(x)
                    d = sort(x)
                    internal[["cum_n"]] <<- length(d):1
                    internal[["dat"]] <<- sort(d)
                    xmin <<- d[1]
                  } else internal[["dat"]]
                },
                xmin = function(x) {
                  if(!missing(x) && !is.null(x)) {
                    if("estimate_xmin" %in% class(x)) {
                      pars <<- x$pars
                      x = x$xmin
                    }
                    internal[["xmin"]] <<- x
                    if(length(internal[["dat"]])) {
                      selection = min(which(internal[["dat"]] >= (x- .Machine$double.eps ^ 0.5)))
                      internal[["n"]] <<- internal[["cum_n"]][selection]      
                    }
                  } else  internal[["xmin"]]
                }, 
                pars = function(x) {
                  if (!missing(x) && !is.null(x)) {
                    if("estimate_pars" %in% class(x)) x = x$pars
                    internal[["pars"]] <<- x
                  } else internal[["pars"]]
                }
              )
  )


#' @rdname displ
#' @aliases conexp-class conexp
#' @exportClass conexp
#' @export conexp
conexp = 
  setRefClass("conexp", 
              contains="ctn_distribution",
              fields = list(
                dat = function(x) {
                  if(!missing(x) && !is.null(x)) {
                    check_ctn_data(x)
                    d = sort(x)
                    internal[["cum_n"]] <<- length(d):1
                    internal[["dat"]] <<- sort(d)
                    xmin <<- d[1]
                  } else internal[["dat"]]
                },
                xmin = function(x) {
                  if(!missing(x) && !is.null(x)) {
                    if("estimate_xmin" %in% class(x)) {
                      pars <<- x$pars
                      x = x$xmin
                    }
                    internal[["xmin"]] <<- x
                    if(length(internal[["dat"]])) {
                      selection = min(which(internal[["dat"]] >= (x- .Machine$double.eps ^ 0.5)))
                      internal[["n"]] <<- internal[["cum_n"]][selection]      
                    }
                    
                  } else  internal[["xmin"]]
                }, 
                pars = function(x) {
                  if (!missing(x) && !is.null(x)) {
                    if("estimate_pars" %in% class(x)) x = x$pars
                    internal[["pars"]] <<- x
                  } else internal[["pars"]]
                }
              )
  )


















