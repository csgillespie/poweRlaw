##Discrete Power-law
displ$methods(
    mle = function(set = TRUE, initialise=NULL) {
        n = internal[["n"]]
        slx = internal[["slx"]]
        mle = 1 + n*sum(slx - log(xmin-1/2)*n)^(-1)
        if(set)
            pars <<- mle
        mle = list(pars=mle)
        class(mle) = "estimate_pars"
        mle
    }
)

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
      r = -disc_lnorm_tail_ll(x, par, xmin)
      if(!is.finite(r)) r = 1e12
      r
    }
    
    mle = optim(par=theta_0, 
                fn=negloglike, 
                method="L-BFGS-B", 
                lower=c(-Inf, .Machine$double.eps))
    if(set)
      pars <<- mle$par
    class(mle) = "estimate_pars"
    names(mle)[1] = "pars"
    mle
  }
)

dispois$methods(
  mle = function(set = TRUE, initialise=NULL) {
    x = dat
    x = x[x > (xmin-1)]
    if(is.null(initialise))
      theta_0 = mean(x)
    else 
      theta_0 = initialise
    # Chop off values below 
    negloglike = function(par) {
      r = -pois_tail_ll(x, par, xmin)
      if(!is.finite(r)) r = 1e12
      r
    }
    mle = optim(par=theta_0, fn=negloglike, method="L-BFGS-B", lower=0)
    if(set)
      pars <<- mle$par
    class(mle) = "estimate_pars"
    names(mle)[1] = "pars"
    mle

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
    mle = list(pars=mle)
    class(mle) = "estimate_pars"
    mle
  }
)

conlnorm$methods(
  mle = function(set = TRUE, initialise=NULL) {
    x = dat
    x = x[x > xmin]
    if(is.null(initialise))
      theta_0 = c(mean(log(x)), sd(log(x)))
    else 
      theta_0 = initialise
    # Chop off values below 
    negloglike = function(par) {
      r = -conlnorm_tail_ll(x, par, xmin)
      if(!is.finite(r)) r = 1e12
      r
    }
    mle = optim(par=theta_0, 
                fn=negloglike, 
                method="L-BFGS-B", 
                lower=c(-Inf, .Machine$double.eps))
    if(set)
      pars <<- mle$par
    class(mle) = "estimate_pars"
    names(mle)[1] = "pars"
    mle
    
  }
)













