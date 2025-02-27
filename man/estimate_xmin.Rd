% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bootstrap.R, R/bootstrap_p.R, R/estimate_xmin.R
\name{get_bootstrap_sims}
\alias{get_bootstrap_sims}
\alias{bootstrap}
\alias{get_bootstrap_p_sims}
\alias{bootstrap_p}
\alias{get_distance_statistic}
\alias{estimate_xmin}
\title{Estimating the lower bound (xmin)}
\usage{
get_bootstrap_sims(m, no_of_sims, seed, threads = 1)

bootstrap(
  m,
  xmins = NULL,
  pars = NULL,
  xmax = 1e+05,
  no_of_sims = 100,
  threads = 1,
  seed = NULL,
  distance = "ks"
)

get_bootstrap_p_sims(m, no_of_sims, seed, threads = 1)

bootstrap_p(
  m,
  xmins = NULL,
  pars = NULL,
  xmax = 1e+05,
  no_of_sims = 100,
  threads = 1,
  seed = NULL,
  distance = "ks"
)

get_distance_statistic(m, xmax = 1e+05, distance = "ks")

estimate_xmin(m, xmins = NULL, pars = NULL, xmax = 1e+05, distance = "ks")
}
\arguments{
\item{m}{A reference class object that contains the data.}

\item{no_of_sims}{number of bootstrap simulations. When \code{no_of_sims} is large, this can
take a while to run.}

\item{seed}{default \code{NULL}. An integer to be supplied to \code{set.seed}, or \code{NULL}
not to set reproducible seeds. This argument is passed \code{clusterSetRNGStream}.}

\item{threads}{number of concurrent threads used during the bootstrap.}

\item{xmins}{default \code{1e5}. A vector of possible values
of xmin to explore. When a single value is passed, this represents
the maximum value to search, i.e. by default we search from
(1, 1e5). See details for further information.}

\item{pars}{default \code{NULL}. A vector or matrix (number of columns equal
to the number of parameters) of parameters used to #' optimise over.
Otherwise, for each value of \code{xmin}, the mle will be used, i.e. \code{estimate_pars(m)}.
For small samples, the mle may be biased.}

\item{xmax}{default \code{1e5}. The maximum x value calculated when working out the CDF.
See details for further
information.}

\item{distance}{A string containing the distance measure (or measures) to calculate.
Possible values are \code{ks} or \code{reweight}. See details for further information.}
}
\description{
When fitting heavy tailed distributions, sometimes it
is necessary to estimate the lower threshold, xmin. The
lower bound is estimated by minimising the
Kolmogorov-Smirnoff statistic
(as described in Clauset, Shalizi, Newman (2009)).
\describe{
\item{\code{get_KS_statistic}}{Calculates the KS statistic for a particular value of xmin.}
\item{\code{estimate_xmin}}{Estimates the optimal lower cutoff using a
goodness-of-fit based approach. This function may issue \code{warnings}
when fitting lognormal, Poisson or Exponential distributions. The
warnings occur for large values of \code{xmin}. Essentially, we are discarding
the bulk of the distribution and cannot calculate the tails to enough
accuracy.}
\item{\code{bootstrap}}{Estimates the uncertainty in the xmin and parameter values
via bootstrapping.}
\item{\code{bootstrap_p}}{Performs a bootstrapping hypothesis test to determine
whether a suggested
(typically power law) distribution is plausible. This is only available for distributions that
have \code{dist_rand} methods available.}}
}
\details{
When estimating \code{xmin} for discrete distributions, the search space when
comparing the data-cdf (empirical cdf)
and the distribution_cdf runs from xmin to \code{max(x)}
where \code{x} is the data set. This \strong{can} often be
computationally brutal. In particular, when bootstrapping
we generate random numbers from the power law distribution,
which has a long tail.

To speed up computations for discrete distributions it is sensible to put an
upper bound, i.e. \code{xmax} and/or explicitly give values of where to search, i.e. \code{xmin}.

Occasionally bootstrapping can generate strange situations. For example,
all values in the simulated data set are less then \code{xmin}. In this case,
the estimated distance measure will be \code{Inf} and the parameter values, \code{NA}.

There are other possible distance measures that can be calculated. The default is the
Kolomogorov Smirnoff statistic (\code{KS}). This is equation 3.9 in the CSN paper. The
other measure currently available is \code{reweight}, which is equation 3.11.
}
\note{
Adapted from Laurent Dubroca's code
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
\dontrun{
bootstrap(m, no_of_sims=1, threads=1)
bootstrap_p(m, no_of_sims=1, threads=1)
}
}
