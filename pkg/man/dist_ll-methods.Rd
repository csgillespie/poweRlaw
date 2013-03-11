\docType{methods}
\name{dist_ll}
\alias{dist_ll}
\alias{dist_ll,conpl-method}
\alias{dist_ll,displ-method}
\title{The log-likelihood function}
\arguments{
  \item{m}{The distribution reference object.}
}
\value{
  The log-likelihood
}
\description{
  This is generic function for distribution reference
  objects. This function calculates the log-likelihood for
  the current parameters and xmin value.
}
\note{
  This method does *not* alter the internal state of the
  distribubtion objects.
}
\examples{
data(moby_sample)
m = displ$new(moby_sample)
m$setXmin(7);m$setPars(2)
dist_ll(m)
}
\seealso{
  \code{\link{dist_cdf}}, \code{\link{dist_pdf}} and
  \code{\link{dist_rand}}
}

