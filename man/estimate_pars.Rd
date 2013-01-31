\name{estimate_pars}
\alias{estimate_pars}
\title{Estimates the distributions using mle.}
\usage{
  estimate_pars(m)
}
\arguments{
  \item{m}{A reference class object that contains the
  data.}
}
\value{
  returns a vector of the mles.
}
\description{
  \code{estimate_pars} estimates the distribution's
  parameters using their maximum likelihood estimator. This
  estimate is conditional on the current xmin value.
}
\examples{
data(moby_sample)
pl_d = pl_data$new(moby_sample)
m = displ$new(pl_d)
estimate_xmin(m)
m$setXmin(7)
estimate_pars(m)
}

