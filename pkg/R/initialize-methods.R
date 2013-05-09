##Ref class initialisers

pl_data$methods( 
  list(
    initialize = function(x = numeric()) {
      message("This class is depreciated. You should now create
              distribution objects directly: displ$new(x) where
              x is a vector of numbers.\n
              See vignette('poweRlaw')")
      stop()
    }
  )
)

#######################################################
#Power-law distributions
#######################################################
#       if( all(x==floor(x)) && is.vector(x) ){ fdattype<-"integer" }
#       if( all(x==floor(x)) && min(x) > 1000 && length(x) > 100 ){ fdattype <- "real" }
displ$methods( 
  list(
    initialize = function(dat) {
      #datatype <<- "discrete"
      no_pars <<- 1
      ##Use the internal attribute for copying
      if(!missing(dat)) {
        x = sort(dat)
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
      }
    }
  )
)

dislnorm$methods( 
  list(
    initialize = function(dat) {
      no_pars <<- 2
      ##Use the internal attribute for copying
      if(!missing(dat)) {
        x = sort(dat)
        tab = table(x)
        values = as.numeric(names(tab))
        freq = as.vector(tab)
        internal[["cum_n"]] <<- rev(cumsum(rev(freq)))
        internal[["freq"]] <<- freq
        internal[["values"]] <<- values
        internal[["dat"]] <<- x
        xmin <<- min(values)
      }
    }
  )
)

dispois$methods( 
  list(
    initialize = function(dat) {
      no_pars <<- 1
      ##Use the internal attribute for copying
      if(!missing(dat)) {
        x = sort(dat)
        tab = table(x)
        values = as.numeric(names(tab))
        freq = as.vector(tab)
        
        internal[["cum_n"]] <<- rev(cumsum(rev(freq)))
        internal[["freq"]] <<- freq
        internal[["values"]] <<- values
        internal[["dat"]] <<- x
        xmin <<- min(values)
      }
    }
  )
)
###############################################
###CTN Distributions
##############################################
conpl$methods( 
  list(
    initialize = function(dat) {
      no_pars <<- 1
      ##Use the internal attribute for copying
      if(!missing(dat)) {
        d = sort(dat)
        internal[["cum_slx"]] <<-
          rev(cumsum(log(rev(d))))
        internal[["cum_n"]] <<- length(d):1
        internal[["dat"]] <<- sort(d)
        xmin <<- d[1]
      }
    }
  )
)

conlnorm$methods( 
  list(
    initialize = function(dat) {
      no_pars <<- 2
      ##Use the internal attribute for copying
      if(!missing(dat)) {
        d = sort(dat)
        internal[["cum_n"]] <<- length(d):1
        internal[["dat"]] <<- sort(d)
        xmin <<- d[1]
      }
    }
  )
)

