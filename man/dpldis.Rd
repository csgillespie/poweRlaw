\name{dpldis}
\alias{dpldis}
\alias{ppldis}
\alias{ppldis_cumsum}
\alias{rpldis}
\title{The discrete powerlaw distribution.}
\usage{
  dpldis(x, xmin, alpha, log = FALSE)

  ppldis(q, xmin, alpha, lower.tail = FALSE)

  ppldis_cumsum(xmax, xmin, alpha)

  rpldis(n, xmin, alpha, xmax = 20000)
}
\arguments{
  \item{x,q}{vector of quantiles. The power-law
  distribution is defined for x >= xmin}

  \item{xmin}{Lower bound of the power-law distribution.
  For the continuous power-law, xmin >= 0 for the discrete
  distribution, xmin >0}

  \item{alpha}{The scaling parameter: alpha > 1}

  \item{log}{logical; if TRUE, log values are returned}

  \item{lower.tail}{logical; if TRUE (default),
  probabilities are \eqn{P[X \le x]}, otherwise, \eqn{P[X >
  x]}.}

  \item{xmax}{upper limit for the CDF}

  \item{n}{number of observations.}
}
\value{
  dplcon gives the denisty and pplcon gives the
  distribution function.
}
\description{
  Density, distribution function and random number
  generation for the discrete power law distribution with
  parameters xmin and alpha.
}
\note{
  The function ppldis takes in a vector, q, and calculates
  the CDF at each of those points. Whereas, ppldis_cumsum
  calculates the CDF from xmin to xmax with a step size of
  1. This is much quicker than passing a sequence vector to
  ppldis.

  This discrete random number generator is pretty
  inefficient
}
\examples{
xmin = 1; alpha = 1.5
x = xmin:100
plot(x, dpldis(x, xmin, alpha), type="l")
plot(x, ppldis(x, xmin, alpha), type="l", main="Distribution function")
plot(x, ppldis(x, xmin, alpha), type="l", main="Distribution function")
rpldis(100, xmin, alpha)
}

