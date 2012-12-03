\docType{methods}
\name{dist_pdf}
\alias{dist_pdf}
\alias{dist_pdf,displ-method}
\title{PDF for the distribution objects.}
\arguments{
  \item{m}{The distribution reference object.}

  \item{q}{a vector values where the function will be
  evaluated. If \code{q} is \code{NULL} (default), then the
  data value will be used.}
}
\value{
  The probability density (or mass) function
}
\description{
  This is generic function for distribution reference
  objects. This function calculates the probability density
  for the current parameters and xmin value.
}
\note{
  This method does *not* alter the internal state of the
  distribubtion objects.
}
\examples{
data(moby_sample)
pl_d = pl_data$new(moby_sample)
m = displ$new(pl_d)
m$setXmin(7);m$setPars(2)
dist_pdf(m)
}
\seealso{
  \code{\link{dist_cdf}}, \code{\link{dist_ll}} and
  \code{\link{dist_rand}}
}

