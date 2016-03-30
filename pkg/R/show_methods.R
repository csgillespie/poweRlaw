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
            msg = paste("Reference class object of class", classLabel(class(object)))
            message(msg)
            message('Field "xmin": '); print(object$getXmin())
            message('Field "pars": '); print(object$getPars())
            message('Field "no_pars": '); print(object$no_pars)
          }
)
