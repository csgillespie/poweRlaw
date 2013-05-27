\docType{methods}
\name{dist_data_cdf}
\alias{dist_data_cdf}
\alias{dist_data_cdf,ctn_distribution-method}
\alias{dist_data_cdf,discrete_distribution-method}
\title{The data cumulative distribution function}
\arguments{
  \item{m}{a reference class distribution object.}

  \item{lower_tail}{logical; if \code{TRUE} (default),
  probabilities are \eqn{P[X \le x]}, otherwise, \eqn{P[X >
  x]}.}

  \item{all_values}{logical; if \code{FALSE} (default). If
  \code{TRUE}, then the cdf is evaluated at points xmin,
  xmin+1, ..., xmax.}
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
dist_data_cdf(m)
}

