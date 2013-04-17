
#' @rdname displ
#' @aliases conpl-class
#' @exportClass conpl
#' @export conpl
conpl = 
  setRefClass("conpl", 
              contains="distribution",
              fields = list(dat = function(x)
                if(!missing(x) && !is.null(x)) {
                  d = sort(x)
                  internal[["cum_slx"]] <<-
                    rev(cumsum(log(rev(d))))
                  internal[["cum_n"]] <<- length(d):1
                  internal[["dat"]] <<- sort(d)
                  xmin <<- d[1]
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























