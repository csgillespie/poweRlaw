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





