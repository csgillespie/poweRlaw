\name{bootstrap_xmin}
\alias{bootstrap_xmin}
\alias{estimate_xmin}
\alias{get_KS_statistic}
\title{Estimates the cut-off point}
\usage{
  bootstrap_xmin(m, no_of_sims = 1000, threads = 1)

  get_KS_statistic(m)

  estimate_xmin(m, xmins = NULL, pars = NULL)
}
\arguments{
  \item{threads}{number of concurrent threads used during
  the bootstrap}

  \item{no_of_sims}{number of bootstrap simulations}

  \item{m}{A reference class object that contains data}

  \item{pars}{default NULL. A vector of parameters used to
  optimise over. Otherwise, for each value of xmin, the
  density pa will be estiamted using the associated mle:
  m$mle()}

  \item{xmins}{a vector of possible values of xmin to
  explore. The default, xmins=NULL, results in exploring
  all possible xmin values.}
}
\value{
  the mle
}
\description{
  get_KS_statistics calculates the Kolmogorov-Smirnov or KS
  statistic. This is the maximum distance between the data
  CDF and fitted model CDF. This corresponds to expression
  (3.9) in the Newman, et al, 2009 paper. The
  Kolmogorov-Smirnov statistic is used when estimating the
  cut-off, xmin.

  estimate_xmin This function implements a goodness-of-fit
  based approach for estimating the lower cutoff for the
  scaling region. This method is described in Clauset,
  Shalizi, Newman (2007)
}
\note{
  Adapted from Laurent Dubroca's code found at
  http://tuvalu.santafe.edu/~aaronc/powerlaws/plfit.r
}
\examples{
data(moby_sample)
pl_d = pl_data$new(moby_sample)
m = displ$new(pl_d)
estimate_xmin(m)
estimate_xmin(m, xmins=c(10, 11, 12))
############################
##Bootstrap examples
bootstrap_xmin(m, no_of_sims=2, threads=1)
}

