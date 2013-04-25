#' The pl class
#' 
#' This class is depreciated. Do not use
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
#' This is the base class for other distributions. There should be
#' no need to create a \code{distribution} object. 
#' @name distribution
#' @aliases distribution-class
#' @docType class
#' @exportClass distribution
#' @export distribution
distribution = setRefClass("distribution", 
                           fields=list(
                             dat = "ANY",
                             internal = "list", 
                             xmin = "ANY", 
                             pars="ANY",
                             no_pars="numeric"), 
                           
)
distribution$accessors(c("xmin", "pars", "dat", "no_pars"))


discrete_distribution = setRefClass("discrete_distribution", 
                                    contains="distribution")
ctn_distribution = setRefClass("ctn_distribution", 
                               contains="distribution")





