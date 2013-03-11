\name{dpldis}
\alias{dpldis}
\alias{ppldis}
\alias{rpldis}
\title{Discrete powerlaw distribution.}
\usage{
  dpldis(x, xmin, alpha, log = FALSE)

  ppldis(q, xmin, alpha, lower.tail = TRUE)

  rpldis(n, xmin, alpha, discrete_max = 1e+05)
}
\arguments{
  \item{x,q}{vector of quantiles. The discrete power-law
  distribution is defined for x > xmin}

  \item{xmin}{The lower bound of the power-law
  distribution. For the continuous power-law, xmin >= 0 for
  the discrete distribution, xmin >0}

  \item{alpha}{The scaling parameter: alpha > 1}

  \item{log}{logical (default FALSE) if TRUE, log values
  are returned}

  \item{lower.tail}{logical; if TRUE (default),
  probabilities are \eqn{P[X \le x]}, otherwise, \eqn{P[X >
  x]}.}

  \item{n}{number of observations.}

  \item{discrete_max}{The value when we switch from the
  discrete random numbers to a CTN approximation}
}
\value{
  dpldis returns the denisty, ppldis returns the
  distribution function and rpldis return random numbers.
}
\description{
  Density, distribution function and random number
  generation for the discrete power law distribution with
  parameters xmin and alpha.
}
\details{
  The Clausett, 2009 paper provides an algorithm for
  generating discrete random numbers. However, if this
  algorithm is implemented in R, it gives terrible
  performance. This is because the algorithm involves
  "growing vectors". Another problem is when alpha is close
  to 1, this can result in very large random number being
  generated (which means we need to calculate the discrete
  CDF).

  The algorithm provided in this package generates true
  discrete random numbers up to 1e5 then switches to using
  continuous random numbers. This switching point can
  altered by changing the \code{discrete_max} argument.

  In order to get a efficient power-law discrete random
  number generator, the algorithm needs to be implemented
  in C.
}
\note{
  The naming of these functions mirrors standard R
  functions, i.e. dnorm. When alpha is close to one,
  generating random number can be very slow.
}
\examples{
xmin = 1; alpha = 1.5
x = xmin:100
plot(x, dpldis(x, xmin, alpha), type="l")
plot(x, ppldis(x, xmin, alpha), type="l", main="Distribution function")
rpldis(x, xmin, alpha)
}

