\docType{data}
\name{bootstrap_moby}
\alias{bootstrap_moby}
\alias{bootstrap_p_moby}
\title{Bootstrap results for the full moby data set}
\format{A list}
\source{
  M. E. J. Newman, "Power laws, Pareto distributions and
  Zipf's law." Contemporary Physics 46, 323 (2005). See
  http://tuvalu.santafe.edu/~aaronc/powerlaws/data.htm for
  further details.
}
\description{
  The output from running \code{bootstrap} on the full moby
  data set is found in the data set \code{bootstrap_moby}.
  The output from running \code{bootstrap_p} on the full
  moby data set is found in the data set
  \code{bootstrap_p_moby}.

  The \code{bootstrap_moby} values correspond to the first
  row of table 6.1 in the Clauset et al paper: \describe{
  \item{\code{bootstrap_moby$gof}}{the K-S statistic}
  \item{\code{bootstrap_moby$bootstraps}}{a data frame for
  the optimal values from the bootstrapping procedure.
  Column 1: K-S, Column 2: xmin, Column 3: alpha. So
  standard deviation of column 2 and 3 is 2.2 and 0.033
  (the paper gives 2 and 0.02 respectively).} }

  The \code{bootstrap_p_moby} gives the p-value for the
  hypthothesis test of whether the data follows a
  power-law. For this simulation study, we get a value of
  0.4306 (the paper gives 0.49).
}
\keyword{datasets}

