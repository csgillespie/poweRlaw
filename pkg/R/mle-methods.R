##Discrete Power-law
displ$methods(
    mle = function(set = TRUE, initialise=NULL) {
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
  mle = function(set = TRUE, initialise=NULL) {
    n = internal[["n"]]
    x = dat
    x = x[x > (xmin-1)]
    x.log = log(x)
    if(is.null(initialise))
      theta_0 = c(mean(x.log), sd(x.log))
    else 
      theta_0 = initialise
    # Chop off values below 
    negloglike = function(par) {
      if(any(par <= 0)) r = .Machine$double.xmax
      else
        r = -lnorm.tail.disc.loglike(x, par[1], par[2],xmin)
      if(!is.finite(r)) r = .Machine$double.xmax
      r
    }
    #mle = nlm(f=negloglike, p=theta_0)
    mle = optim(par=theta_0, fn=negloglike)
    if(set)
      pars <<- mle$par
    mle$par
  
  }
)





##CTN Power-law
conpl$methods(
  mle = function(set = TRUE, initialise=NULL) {
    n = internal[["n"]]
    slx = internal[["slx"]]
    mle = 1 + n*(slx-log(xmin)*n)^(-1)
    if(set)
      pars <<- mle
    mle
  }
)
