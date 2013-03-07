\name{bootstrap_xmin}
\alias{bootstrap_xmin}
\alias{estimate_xmin}
\alias{get_KS_statistic}
\title{Estimates the lower bound (xmin)}
\usage{
  bootstrap_xmin(m, xmins = NULL, pars = NULL,
    no_of_sims = 100, threads = 1, data_max = 1e+06)

  get_KS_statistic(m)

  estimate_xmin(m, xmins = NULL, pars = NULL,
    data_max = 1e+05)
}
\arguments{
  \item{threads}{number of concurrent threads used during
  the bootstrap}

  \item{no_of_sims}{number of bootstrap simulations. This
  can take a while to run}

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
  discrete distributions, a the search space when comparing
  the data_cdf and distribution_cdf runs from 1:data_max}
}
\value{
  \code{estimate_xmin} returns a vector containing the
  optimial parameter value, xmin and the associated KS
  statistic.
}
\description{
  \code{bootstrap_xmin} estimates unncertainity in the xmin
  and parameter values using bootstraping. This function
  runs in parallel with the number of threads specficied by
  the \code{threads} argument.

  \code{get_KS_statistic} calculates the Kolmogorov-Smirnov
  or KS statistic. This is the maximum distance between the
  data CDF and fitted model CDF. This corresponds to
  expression (3.9) in the Newman, et al, 2009 paper. The
  Kolmogorov-Smirnov statistic is used when estimating the
  cut-off, xmin.

  \code{estimate_xmin} estimates the lower cutoff using a
  goodness-of-fit based approach. This method is described
  in Clauset, Shalizi, Newman (2009)
}
\note{
  Adapted from Laurent Dubroca's code found at
  http://tuvalu.santafe.edu/~aaronc/powerlaws/plfit.r
}
\examples{
data(moby_sample)
m = displ$new(moby_sample)
estimate_xmin(m)
estimate_xmin(m, xmins=10:12)
############################
##Bootstrap examples
bootstrap_xmin(m, no_of_sims=3, threads=1)
}

