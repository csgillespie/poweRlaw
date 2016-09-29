#' Generic show method for distribution objects
#' 
#' The distribution objects have an internal structure that is used for caching purposes. 
#' Using the default \code{show} method gives the illusion of duplicate values. 
#' This show method aims to avoid this confusion.
#' @param object A distribution object.
#' @importFrom methods classLabel show
#' @export
setMethod("show", 
          signature = signature(object="distribution"),
          definition = function(object) {
            msg = paste("Reference class object of class", classLabel(class(object)), "\n")
            cat(msg)
            cat('Field "xmin": \n'); print(object$getXmin())
            cat('Field "pars": \n'); print(object$getPars())
            cat('Field "no_pars": \n'); print(object$no_pars)
          }
)
