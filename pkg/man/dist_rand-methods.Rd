\docType{methods}
\name{dist_rand}
\alias{dist_rand}
\alias{dist_rand,conpl-method}
\alias{dist_rand,displ-method}
\title{Random number generation for the distribution objects}
\arguments{
  \item{m}{a distribution reference object.}

  \item{n}{number of observations to be generated.}
}
\value{
  n random numbers
}
\description{
  This is generic function for generating random numbers
  from the underlying distribution of the distribution
  reference objects. This function generates \code{n}
  random numbers using the parameters and xmin values found
  in the associated reference object.
}
\note{
  This method does *not* alter the internal state of the
  distribubtion object.
}
\examples{
data(moby_sample)
m = displ$new(moby_sample)
m$setXmin(7);m$setPars(2)
dist_rand(m, 5)
}
\seealso{
  \code{\link{dist_cdf}}, \code{\link{dist_pdf}} and
  \code{\link{dist_ll}}
}

