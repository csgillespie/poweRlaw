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

##Need a try catch

dislnorm$methods(
  mle = function(set = TRUE) {
    n = internal[["n"]]
    x = m$dat
    x = x[x > (xmin-1)]
    x.log = log(x)
    theta_0 = c(mean(x.log), sd(x.log))
    # Chop off values below 
    negloglike = function(theta) {
      if(any(theta <= 0)) r = .Machine$double.xmax
      else
        r = -lnorm.tail.disc.loglike(x,theta[1],theta[2],xmin)
      if(!is.finite(r)) r = .Machine$double.xmax
      r
    }
    mle = nlm(f=negloglike, p=theta_0)
    if(set)
      pars <<- mle$estimate
    mle$estimate
  
  }
)





##CTN Power-law
conpl$methods(
  mle = function(set = TRUE) {
    n = internal[["n"]]
    slx = internal[["slx"]]
    mle = 1 + n*(slx-log(xmin)*n)^(-1)
    if(set)
      pars <<- mle
    mle
  }
)
