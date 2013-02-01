displ$methods(
  mle = function(set = TRUE) {
    mle = 1 + n*sum(slx - log(xmin-1/2)*n)^(-1)
    if(set)
      pars <<- mle
    mle
  }
)

conpl$methods(
  mle = function() {
    pars <<- 1 + n*(slx-log(xmin)*n)^(-1)
    pars
  }
)
