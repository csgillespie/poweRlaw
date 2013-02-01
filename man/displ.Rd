\docType{class}
\name{displ}
\alias{displ}
\alias{displ-class}
\title{Maximum likelihood estimation of the discrete power law distribution.}
\arguments{
  \item{x}{data vector}

  \item{xmin}{Lower bound of the power-law distribution.
  For the continuous power-law, xmin >= 0 for the discrete
  distribution, xmin >0}

  \item{alpha}{The scaling parameter: alpha > 1}
}
\value{
  the mle estimate of the parameter alpha (for fixed xmin).
}
\description{
  Maximum likelihood estimation of the discrete power law
  distribution.
}
\examples{
data(moby)
pl_data = pl_data$new(moby)
m = displ$new(pl_data)
m$setXmin(7)
m$setPars(5)
dist_ll(m) #-16945
estimate_pars(m) #1.952
m$setPars(estimate_pars(m))
plot(pl_data)
plot(m)
lines(m)
}

