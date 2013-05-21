#' The pl class
#' 
#' This class is depreciated. Do not use
#' @name pl_data-class
#' @aliases pl_data
#' @docType class
#' @param ... This class is depreciated.
#' @exportClass pl_data
pl_data = setRefClass("pl_data", 
                      fields = list(x="numeric", 
                                    values="numeric", 
                                    freq="numeric"))


distribution = setRefClass("distribution", 
                           fields=list(
                             dat = "ANY",
                             internal = "list", 
                             xmin = "ANY", 
                             pars="ANY",
                             no_pars="numeric"), 
                           
)
distribution$accessors(c("xmin", "pars", "dat", "no_pars"))


discrete_distribution = setRefClass("discrete_distribution", contains="distribution")
ctn_distribution = setRefClass("ctn_distribution", contains="distribution")





