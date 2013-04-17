\docType{methods}
\name{dist_data_cdf}
\alias{dist_data_cdf}
\alias{dist_data_cdf,conpl-method}
\alias{dist_data_cdf,displ-method}
\title{The data cumulative distribution function}
\arguments{
  \item{m}{a reference class distribution object.}

  \item{lower_tail}{logical; if \code{TRUE} (default),
  probabilities are \eqn{P[X \le x]}, otherwise, \eqn{P[X >
  x]}.}

  \item{all_values}{logical; if \code{FALSE} (default),
  calculated at observed data values}
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
dist_data_cdf(m)
}

