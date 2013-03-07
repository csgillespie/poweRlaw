#' @name blackouts
#' @title Electrical blackout
#' @description The numbers of customers affected in electrical blackouts in the United States between 1984 and 2002.
#' @docType data
#' @format A vector
#' @source M. E. J. Newman, "Power laws, Pareto distributions and Zipf's law." Contemporary Physics 46, 323 (2005). See http://tuvalu.santafe.edu/~aaronc/powerlaws/data.htm for further details.
NULL

#' @name moby
#' @aliases moby_sample
#' @title Moby Dick word count
#' @description The frequency of occurrence of unique words in the novel Moby Dick by Herman Melville. 
#' 
#' The data set moby_sample is 2000 values
#' sampled from the moby data set.
#' @docType data
#' @format A vector
#' @source M. E. J. Newman, "Power laws, Pareto distributions and Zipf's law." Contemporary Physics 46, 323 (2005). See http://tuvalu.santafe.edu/~aaronc/powerlaws/data.htm for further details.
NULL

#' @name bootstrap_moby
#' @aliases bootstrap_moby
#' @title Bootstrap results for the full moby data set
#' @description The output from running
#' bs = bootstrap_xmin(m, no_of_sims=1000, threads=2) 
#' 
#' The following values correspond to the first row of
#' table 6.1 in the Clauset et al paper:
#' bs[[1]] gives the p-value (paper give 0.49)
#' bs[[2]] the K-S statistic
#' bs[[3]] a data frame for the optimal values from the bootstrapping procedure. Column 1: K-S, Column 2: xmin, Column 3: alpha
#' sd(bs[[3]][,2]) gives 3 (paper gives 2)
#' sd(bs[[3]][,3]) gives 0.027 (paper gives 0.02)
#' @docType data
#' @format A list
#' @source M. E. J. Newman, "Power laws, Pareto distributions and Zipf's law." Contemporary Physics 46, 323 (2005). See http://tuvalu.santafe.edu/~aaronc/powerlaws/data.htm for further details.
NULL

