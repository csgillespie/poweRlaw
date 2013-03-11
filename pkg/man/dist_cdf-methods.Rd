\docType{methods}
\name{dist_cdf}
\alias{dist_cdf}
\alias{dist_cdf,conpl-method}
\alias{dist_cdf,displ-method}
\alias{dist_data_cdf}
\alias{dist_data_cdf,conpl-method}
\alias{dist_data_cdf,displ-method}
\title{The cumulative distribution function}
\arguments{
  \item{m}{a reference class distribution object.}

  \item{q}{a vector values where the function will be
  evaluated. If \code{q} is \code{NULL} (default), then the
  data values will be used.}

  \item{lower.tail}{logical; if \code{TRUE} (default),
  probabilities are \eqn{P[X \le x]}, otherwise, \eqn{P[X >
  x]}.}
}
\description{
  This is generic function for distribution reference
  objects. This function calculates the cumulative
  probability density for the current parameters and xmin
  value.
}
\note{
  This method does *not* alter the internal state of the
  distribubtion objects.
}
\examples{
data(moby_sample)
m = displ$new(moby_sample)
m$setXmin(7);m$setPars(2)
#CDF at a particular value
dist_cdf(m, 10:15)
dist_cdf(m) #at the data values
}

