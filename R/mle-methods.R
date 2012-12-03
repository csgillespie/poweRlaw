
displ$methods(
  mle = function() {
    pars <<- 1 + n*sum(slx - log(xmin-1/2)*n)^(-1)
    pars
  }
)
