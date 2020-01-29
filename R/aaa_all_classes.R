distribution = setRefClass("distribution", 
                           fields = list(
                             dat = "ANY",
                             internal = "list", 
                             xmin = "ANY", 
                             pars = "ANY",
                             no_pars = "numeric"))

###############################
# Set get and set methods
# These will be inherited
###############################
distribution$accessors(c("xmin", "pars", "dat", "no_pars"))

#############################################################
# In the plotting functions we do slightly 
# different things for continuous and discrete distributions
#############################################################
discrete_distribution = setRefClass("discrete_distribution", contains = "distribution")
ctn_distribution = setRefClass("ctn_distribution", contains = "distribution")
