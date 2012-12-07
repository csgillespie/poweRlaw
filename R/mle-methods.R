displ$methods(
  mle = function() {
    pars <<- 1 + n*sum(slx - log(xmin-1/2)*n)^(-1)
    pars
  }
)

conpl$methods(
  mle = function() {
    pars <<- 1 + n*(slx-log(xmin)*n)^(-1)
    pars
  }
)
