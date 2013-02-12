#' The discrete powerlaw distribution. 
#' 
#' Density, distribution function and random number generation for the discrete power law distribution with parameters xmin and alpha.
#' @param x,q vector of quantiles. The discrete 
#' power-law distribution is defined for x > xmin
#' @param xmin The lower bound of the power-law distribution. 
#' For the continuous power-law, xmin >= 0 
#' for the discrete distribution, xmin >0
#' @param alpha The scaling parameter: alpha > 1
#' @param log logical (default FALSE) if TRUE, log values are returned
#' @param lower.tail logical; 
#' if TRUE (default), probabilities are \eqn{P[X \le x]}, 
#' otherwise, \eqn{P[X > x]}.
#' @return dpldis returns the denisty, ppldis returns the distribution function 
#' and rpldis return random numbers.
#' @note The naming of these functions mirrors standard R functions, i.e. dnorm.
#' When alpha is close to one, generating random number can be very slow.
#' @export
#' @examples
#' xmin = 1; alpha = 1.5
#' x = xmin:100
#' plot(x, dpldis(x, xmin, alpha), type="l")
dpldis = function(x, xmin, alpha, log=FALSE) {
    x = x[x>=xmin]
    xmin = floor(xmin)
    constant = zeta(alpha)
    if(xmin > 1) constant = constant - sum((1:(xmin-1))^(-alpha))
    
    if(log)
        -alpha*log(x) - log(constant)
    else
        x^(-alpha)/constant
}

#'@rdname dpldis
#'@export
#'@details The Clausett, 2009 paper provides an algorithm for generating discrete random numbers. However, if this
#'algorithm is implemented in R, it gives terrible performance. This is because the algorithm involves "growing vectors". 
#'Another problem is when alpha is close to 1, this can result in very large random number being generated (which means we need 
#'to calculate the discrete CDF). 
#'
#'The algorithm provided in this package generates true discrete random numbers up to 50000, then switches to using the
#'continuous CDF. 
#'
#'In order to get a efficient power-law discrete random number generator, the algorithm needs to be implemented in 
#'C.
#'@examples
#' plot(x, ppldis(x, xmin, alpha), type="l", main="Distribution function")
#' rpldis(x, xmin, alpha)
ppldis = function(q, xmin, alpha, lower.tail=TRUE) {
    #x = x[x>=xmin]
    xmin = floor(xmin)
    constant = zeta(alpha)
    if(xmin > 1) 
        constant = constant - sum((1:(xmin-1))^(-alpha))
    cdf = 1-(constant - sapply(q, function(i) sum((xmin:i)^(-alpha))))/constant
    if(lower.tail)
        cdf
    else
        1 - (cdf - dpldis(q, xmin, alpha)) 
}


my_ppldis_cumsum = function(xmin, alpha, incr) {
    xmin = floor(xmin)
    alpha = alpha
    constant = zeta(alpha)
    if(xmin > 1) 
        constant = constant - sum((1:(xmin-1))^(-alpha))
    upper = 0
    xstart = xmin; xend = xmin + incr
    cdf = function()  {
        cdf = 1-(constant - cumsum((xstart:xend)^(-alpha)))/constant+ upper
        upper <<- cdf[length(cdf)-1]
        xstart <<- xend; xend <<- xend + 2*incr
        return(cdf)
    }
    get_xstart = function() xstart
    get_xend = function() xend
    get_alpha = function() alpha
    get_xmin = function() xmin
    list(cdf = cdf, get_xstart=get_xstart, get_xend=get_xend, 
         get_alpha= get_alpha, get_xmin=get_xmin)
}


rng = function(u, pp) {
    xend = pp$get_xend()
    if(!length(u))
        return(NULL)
    else if(xend > 50000) {
        xmin = pp$get_xmin(); alpha = pp$get_alpha()
        rngs = floor(xmin*(1-u)^(-1/(alpha-1)))
    } else {
        xstart = pp$get_xstart(); xend = pp$get_xend()
        cdf = pp$cdf()
        rngs = colSums(sapply(u, ">", cdf)) + xstart
        rngs[rngs == (xend+1)] = rng(u[rngs==(xend+1)], pp)
    }
    return(rngs)
}

#' @param n number of observations.
#' @rdname dpldis
#' @export
rpldis = function(n, xmin, alpha) {
    u = runif(n)
    pp = my_ppldis_cumsum(xmin, alpha, 10000)
    rng(u, pp)
}


