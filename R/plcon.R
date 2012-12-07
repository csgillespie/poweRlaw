#' @title The continuous powerlaw distribution
#' 
#' Density and distribution function of the continuous power-law distribution, with
#' parameters xmin and alpha.
#' @inheritParams ppldis
#' @return dplcon gives the denisty and pplcon gives the distribution function.
#' @export
#' @examples
#' xmin = 1; alpha = 1.5
#' x = seq(xmin, 10, length.out=1000)
#' plot(x, dplcon(x, xmin, alpha), type="l")
dplcon = function(x, xmin, alpha, log=FALSE) {
    x = x[x>=xmin]
    if(log)
        log(alpha-1) - log(xmin) - alpha*(log(x/xmin))
    else
        (alpha-1)/xmin * (x/xmin)^(-alpha)
}


#'@rdname dplcon
#'@export
#'@examples
#' plot(x, pplcon(x, xmin, alpha), type="l", main="Distribution function")
pplcon = function(q, xmin, alpha, lower.tail=TRUE) {
    #x = x[x>=xmin]
  cdf = 1 - (q/xmin)^(-alpha + 1)
  if(lower.tail)
    cdf
  else
    1 - (cdf - dplcon(q, xmin, alpha)) 
    
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












