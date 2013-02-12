##Discrete Power-law
displ$methods(
    mle = function(set = TRUE) {
        n = internal[["n"]]
        slx = internal[["slx"]]
        mle = 1 + n*sum(slx - log(xmin-1/2)*n)^(-1)
        if(set)
            pars <<- mle
        mle
    }
)

##CTN Power-law
conpl$methods(
  mle = function(set = TRUE) {
    mle = 1 + n*(slx-log(xmin)*n)^(-1)
    if(set)
      pars <<- mle
    mle
  }
)
