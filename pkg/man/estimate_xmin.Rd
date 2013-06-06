\name{bootstrap_p}
\alias{bootstrap}
\alias{bootstrap_p}
\alias{estimate_xmin}
\alias{get_KS_statistic}
\title{Estimates the lower bound (xmin)}
\usage{
  bootstrap_p(m, xmins = NULL, pars = NULL,
    no_of_sims = 100, threads = 1, data_max = 1e+05)

  bootstrap(m, xmins = NULL, pars = NULL, no_of_sims = 100,
    threads = 1, data_max = 1e+05)

  get_KS_statistic(m)

  estimate_xmin(m, xmins = NULL, pars = NULL,
    data_max = 1e+05)
}
\arguments{
  \item{m}{A reference class object that contains the
  data.}

  \item{pars}{default NULL. A vector of parameters used to
  optimise over. Otherwise, for each value of xmin, the mle
  will be used, i.e. \code{estimate_pars(m)}. For small
  samples, the mle may be biased.}

  \item{xmins}{default NULL. A vector of possible values of
  xmin to explore. The default, \code{xmins=NULL}, results
  in exploring all possible xmin values.}

  \item{data_max}{default 1e5. When estimating xmin for
  discrete distributions, the search space when comparing
  the data_cdf and distribution_cdf runs from 1:data_max}

  \item{threads}{number of concurrent threads used during
  the bootstrap.}

  \item{no_of_sims}{number of bootstrap simulations. When
  \code{no_of_sims} is large, this can take a while to
  run.}
}
\description{
  When fitting heavy tailed distributions, sometimes it is
  necessary to estimate the lower threshold, xmin. The
  lower bound is estimated by calculating the minimising
  the Kolmogorov-Smirnoff statistic (as described in
  Clauset, Shalizi, Newman (2009)). \describe{
  \item{\code{get_KS_statistic}}{Calculates the KS
  statistic for a particular value of xmin}
  \item{\code{estimate_xmin}}{Estimates the optimal lower
  cutoff using a goodness-of-fit based approach}
  \item{\code{bootstrap}}{Estimates the unncertainity in
  the xmin and parameter values via bootstraping.}
  \item{\code{bootstrap_p}}{Performs a bootstrapping
  hypothesis test to determine whether a power law
  distribution is plausible. This function only available
  for power law distribution objects.}}
}
\note{
  Adapted from Laurent Dubroca's code found at
  \url{http://tuvalu.santafe.edu/~aaronc/powerlaws/plfit.r}
}
\examples{
###################################################
# Load the data set and create distribution object#
###################################################
x = 1:10
m = displ$new(x)

###################################################
# Estimate xmin and pars                          #
###################################################
est = estimate_xmin(m)
m$setXmin(est)

###################################################
# Bootstrap examples                              #
###################################################
bootstrap(m, no_of_sims=1, threads=1)
bootstrap_p(m, no_of_sims=1, threads=1)
}

