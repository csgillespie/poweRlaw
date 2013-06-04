\docType{methods}
\name{dist_pdf}
\alias{dist_pdf}
\alias{dist_pdf,conexp-method}
\alias{dist_pdf,conlnorm-method}
\alias{dist_pdf,conpl-method}
\alias{dist_pdf,disexp-method}
\alias{dist_pdf,dislnorm-method}
\alias{dist_pdf,displ-method}
\alias{dist_pdf,dispois-method}
\title{The probability density function}
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
  function (pdf) for the current parameters and xmin value.
}
\note{
  This method does *not* alter the internal state of the
  distribubtion objects.
}
\examples{
data(moby_sample)
m = displ$new(moby_sample)
m$setXmin(7);m$setPars(2)
dist_pdf(m)
}
\seealso{
  \code{\link{dist_cdf}}, \code{\link{dist_ll}} and
  \code{\link{dist_rand}}
}

