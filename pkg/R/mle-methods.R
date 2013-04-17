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

dislnorm$methods(
  mle = function(set = TRUE) {
    n = internal[["n"]]
    x.log = log(dat)
    theta_0 = c(mean(x.log), sd(x.log))
    # Chop off values below 
    ##SLOW MUST cache
    x = rep(internal[["values"]], internal[["freq"]])
    negloglike = function(theta) {-lnorm.tail.disc.loglike(x,theta[1],theta[2],xmin)}
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
