#' @title The continuous powerlaw distribution
#' 
#' @description Density and distribution function of the continuous power-law distribution, with
#' parameters xmin and alpha.
#' @inheritParams ppldis
#' @param x,q vector of quantiles. The discrete 
#' power-law distribution is defined for x > xmin
#' @return dplcon gives the denisty and pplcon gives the distribution function.
#' @export
#' @examples
#' xmin = 1; alpha = 1.5
#' x = seq(xmin, 10, length.out=1000)
#' plot(x, dplcon(x, xmin, alpha), type="l")
dplcon = function(x, xmin, alpha, log=FALSE) {
#   tol = 1e-10
#   x = x[(x+tol) >= xmin]
  if(log){
    pdf = log(alpha-1) - log(xmin) - alpha*(log(x/xmin))
    pdf[x < xmin] = -Inf
  } else {
    pdf = (alpha-1)/xmin * (x/xmin)^(-alpha)
    pdf[x < xmin] = 0
  }
  pdf
}


#'@rdname dplcon
#'@export
#'@examples
#' plot(x, pplcon(x, xmin, alpha), type="l", main="Distribution function")
pplcon = function(q, xmin, alpha, lower.tail=TRUE) {
#   tol = 1e-10
#   q = q[(q+tol) >= xmin]
  cdf = 1 - (q/xmin)^(-alpha + 1)
  if(!lower.tail)
    cdf = 1 - cdf#(cdf - dplcon(q, xmin, alpha)) 
  cdf[q < round(xmin)] = 0
  cdf
}


#'@rdname dplcon
#'@note The discrete random number generator is very inefficient
#'@export
#'@examples
#' n = 1000
#' con_rns = rplcon(n, xmin, alpha)
#' con_rns = sort(con_rns)
#' p = rep(1/n, n)
#' #Zipfs plot
#' plot(con_rns, rev(cumsum(p)), log="xy", type="l")
rplcon = function(n, xmin, alpha) {
  xmin*(1-runif(n))^(-1/(alpha-1))
}




