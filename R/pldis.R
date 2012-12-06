#' The discrete powerlaw distribution. 
#' 
#' Density, distribution function and random number generation for the discrete power law distribution with parameters xmin and alpha.
#' @param x,q vector of quantiles. The power-law distribution is defined for x >= xmin
#' @param xmin Lower bound of the power-law distribution. For the continuous 
#' power-law, xmin >= 0 for the discrete distribution, xmin >0
#' @param alpha The scaling parameter: alpha > 1
#' @param log logical; if TRUE, log values are returned
#' @param lower.tail logical; 
#' if TRUE (default), probabilities are \eqn{P[X \le x]}, 
#' otherwise, \eqn{P[X > x]}.
#' @return dplcon gives the denisty and pplcon gives the distribution function.
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
#'@examples
#' plot(x, ppldis(x, xmin, alpha), type="l", main="Distribution function")
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

#'@rdname dpldis
#'@param xmax upper limit for the CDF
#'@note The function ppldis takes in a vector, q, and calculates the CDF at each of those points. Whereas, ppldis_cumsum calculates the CDF from xmin to xmax with a step size of 1. This is much quicker than passing a sequence vector to ppldis.
#'@export
#'@examples
#' plot(x, ppldis(x, xmin, alpha), type="l", main="Distribution function")
ppldis_cumsum = function(xmax, xmin, alpha) {
    xmin = floor(xmin)
    constant = zeta(alpha)
    if(xmin > 1) 
        constant = constant - sum((1:(xmin-1))^(-alpha))
    1-(constant - cumsum((xmin:xmax)^(-alpha)))/constant
}



#'@rdname dpldis
#'@param n number of observations.
#'@note This discrete random number generator is pretty inefficient
#'@export
#'@examples
#' rpldis(100, xmin, alpha)
rpldis = function(n, xmin, alpha, xmax=20000) {
    u = runif(n)
    pp = ppldis_cumsum(xmax, xmin, alpha)
    colSums(sapply(u, ">", pp)) + xmin
}

#Simulates a single random number
# rpldis_1 = function(xmin, alpha) {
#     x2 = 2*xmin
#     x1 = xmin
#     u = runif(1)
#     while(ppldis(x2, xmin, alpha) < u) {
#         x1 = x2
#         x2 = 2*x1
#     }
#     pp = ppldis(x1:x2, xmin, alpha) < u
#     x1 + sum(pp)
# }
