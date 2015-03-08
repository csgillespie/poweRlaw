#' Discrete powerlaw distribution
#' 
#' Density, distribution function and random number generation 
#' for the discrete power law distribution with parameters xmin and alpha.
#' @param x,q vector of quantiles. The discrete 
#' power-law distribution is defined for x > xmin.
#' @param xmin The lower bound of the power-law distribution. 
#' For the continuous power-law, xmin >= 0. 
#' for the discrete distribution, xmin > 0.
#' @param alpha The scaling parameter: alpha > 1.
#' @param log logical (default FALSE) if TRUE, log values are returned.
#' @param lower.tail logical; 
#' if TRUE (default), probabilities are \eqn{P[X \le x]}, 
#' otherwise, \eqn{P[X > x]}.
#' @return dpldis returns the denisty, ppldis returns the distribution function 
#' and rpldis return random numbers.
#' @note The naming of these functions mirrors standard R functions, i.e. dnorm.
#' When alpha is close to one, generating random number can be very slow.
#' @references Clauset, Aaron, Cosma Rohilla Shalizi, and Mark EJ Newman. 
#' "Power-law distributions in empirical data." SIAM review 51.4 (2009): 661-703.
#' @export
#' @examples
#' xmin = 1; alpha = 2
#' x = xmin:100
#' 
#' plot(x, dpldis(x, xmin, alpha), type="l")
dpldis = function(x, xmin, alpha, log=FALSE) {
  x = x[round(x) >= round(xmin)]
  xmin = floor(xmin)
  constant = zeta(alpha)
  if(xmin > 1) constant = constant - sum((1:(xmin-1))^(-alpha))
  
  if(log) {
    pdf = -alpha*log(x) - log(constant)
    pdf[round(x) < round(xmin)] = -Inf
  } else {
    pdf = x^(-alpha)/constant
    pdf[round(x) < round(xmin)] = 0
  }
  pdf
  
}

#'@rdname dpldis
#'@export
#'@details The Clausett, 2009 paper provides an algorithm for generating discrete random numbers. However, if this
#'algorithm is implemented in R, it gives terrible performance. This is because the algorithm involves "growing vectors". 
#'Another problem is when alpha is close to 1, this can result in very large random number being generated (which means we need 
#'to calculate the discrete CDF for very large values). 
#'
#'The algorithm provided in this package generates true 
#'discrete random numbers up to 10,000 then switches to
#'using continuous random numbers. This switching point can altered by 
#'changing the \code{discrete_max} argument. 
#'
#'In order to get a efficient power-law discrete random number generator, the algorithm needs to be implemented in C.
#'@examples
#' plot(x, ppldis(x, xmin, alpha), type="l", main="Distribution function")
#' dpldis(1, xmin, alpha)
ppldis = function(q, xmin, alpha, lower.tail=TRUE) {
  #    q = q[round(q) >= round(xmin)]
  xmin = floor(xmin)
  constant = zeta(alpha)
  if(xmin > 1) 
    constant = constant - sum((1:(xmin-1))^(-alpha))
  cdf = 1-(constant - sapply(q, function(i) sum((xmin:i)^(-alpha))))/constant
  if(!lower.tail)
    cdf = 1 - (cdf - dpldis(q, xmin, alpha))
  cdf[round(q) < round(xmin)] = 0
  cdf
}


#' @param n Number of observations. If \code{length(n) > 1}, the length is taken to be the number required.
#' @param discrete_max The value when we switch from the discrete random numbers to a CTN approximation.
#' @rdname dpldis
#' @export
#' @examples
#' 
#' ###############################################
#' ## Random number generation                   #
#' ###############################################
#' n = 1e5
#' x1 = rpldis(n, xmin, alpha)
#' ## Compare with exact (dpldis(1, xmin, alpha))
#' sum(x1==1)/n
#' ## Using only the approximation
#' x2 = rpldis(n, xmin, alpha, 0)
#' sum(x2==1)/n
#' 
rpldis = function(n, xmin, alpha, discrete_max = 10000) {
  ## Initialise parameters
  if(length(n) > 1L) n = length(n)
  
  xmin = floor(xmin)
  u = runif(n)
  
  ## Work out CDF
  if(discrete_max > 0.5) {
    constant = zeta(alpha)
    if(xmin > 1) constant = constant - sum((1:(xmin-1))^(-alpha))
    cdf = c(0, 1-(constant - cumsum((xmin:discrete_max)^(-alpha)))/constant)
    
    ## Due to numerical instability
    ## Not enough precision to exactly calculate the CDF
    dups = duplicated(cdf, fromLast=TRUE) 
    if(any(dups)) cdf = cdf[1:which.min(!dups)]
    
    ## Simulate using look up method 
    rngs = as.numeric(cut(u,cdf)) + xmin - 1
    
    ## Fill in blanks using Clausett approximation
    is_na = is.na(rngs)
    if(any(is_na)) rngs[is_na] = 
      floor((xmin-0.5)*(1-u[is_na])^(-1/(alpha-1))+0.5)
  } else {
    ## Using only the approximation
    rngs = floor((xmin-0.5)*(1-u)^(-1/(alpha-1))+0.5)
  }
  rngs
}
