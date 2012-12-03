\docType{methods}
\name{dist_rand}
\alias{dist_rand}
\alias{dist_rand,displ-method}
\title{Random numbers for the distribution objects.}
\arguments{
  \item{m}{The distribution reference object.}

  \item{n}{number of observations to be generated.}
}
\value{
  n random numbers
}
\description{
  This is generic function for distribution reference
  objects. This function generates N random number using
  the parameters and xmin values found in the reference
  object.
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
dist_rand(m, 5)
}
\seealso{
  \code{\link{dist_cdf}}, \code{\link{dist_pdf}} and
  \code{\link{dist_ll}}
}

