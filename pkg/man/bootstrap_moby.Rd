\docType{data}
\name{bootstrap_moby}
\alias{bootstrap_moby}
\title{Bootstrap results for the full moby data set}
\format{A list}
\source{
  M. E. J. Newman, "Power laws, Pareto distributions and
  Zipf's law." Contemporary Physics 46, 323 (2005). See
  http://tuvalu.santafe.edu/~aaronc/powerlaws/data.htm for
  further details.
}
\description{
  The output from running bs = bootstrap_xmin(m,
  no_of_sims=1000, threads=2)

  The following values correspond to the first row of table
  6.1 in the Clauset et al paper: bs[[1]] gives the p-value
  (paper give 0.49) bs[[2]] the K-S statistic bs[[3]] a
  data frame for the optimal values from the bootstrapping
  procedure. Column 1: K-S, Column 2: xmin, Column 3: alpha
  sd(bs[[3]][,2]) gives 3 (paper gives 2) sd(bs[[3]][,3])
  gives 0.027 (paper gives 0.02)
}
\keyword{datasets}

