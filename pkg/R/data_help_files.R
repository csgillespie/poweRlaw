#' @name moby
#' @aliases moby_sample
#' @title Moby Dick word count
#' @description The frequency of occurrence of unique words in the novel Moby Dick by Herman Melville. 
#' 
#' The data set moby_sample is 2000 values
#' sampled from the moby data set.
#' @docType data
#' @format A vector
#' @source M. E. J. Newman, "Power laws, Pareto distributions and Zipf's law." 
#' Contemporary Physics 46, 323 (2005). See http://tuvalu.santafe.edu/~aaronc/powerlaws/data.htm for further details.
NULL


#' @name bootstrap_moby
#' @aliases bootstrap_moby bootstrap_p_moby
#' @title Example bootstrap results for the full Moby Dick data set
#' @description To explore the uncertainity in the model fit, this package provides a \code{bootstrap} function.
#' \describe{
#' \item{bootstrap_moby}{The output from running 5000 bootstraps on the full Moby Dick data set (for a discrete power law) 
#' using the \code{bootstrap} function. }
#' \item{bootstrap_p_moby}{The output from running 5000 bootstraps on the full Moby Dick data set (for a discrete power law) 
#' using the \code{bootstrap_p} function.}}
#' The \code{bootstrap_moby} values correspond to the first row of
#' table 6.1 in the Clauset et al paper:
#' \describe{
#' \item{\code{bootstrap_moby$gof}}{the K-S statistic}
#' \item{\code{bootstrap_moby$bootstraps}}{a data frame for the optimal values from the bootstrapping procedure. 
#' Column 1: K-S, Column 2: xmin, Column 3: alpha. 
#' So standard deviation of column 2 and 3 is 2.2 and 0.033 (the paper gives 2 and 0.02 respectively).}
#' }
#' 
#' The \code{bootstrap_p_moby} gives the p-value for the hypothesis
#' test of whether the data follows a power-law. For this simulation study, 
#' we get a value of 0.43 (the paper gives 0.49).
#' @docType data
#' @seealso \code{moby}, \code{bootstrap}, \code{bootstrap_p}
#' @format A list
#' @source M. E. J. Newman, "Power laws, Pareto distributions and Zipf's law." 
#' Contemporary Physics 46, 323 (2005). See http://tuvalu.santafe.edu/~aaronc/powerlaws/data.htm for further details.
#' @examples
#' ## Generate the bootstrap_moby data set
#' \dontrun{
#' data(moby)
#' m = displ$new(moby)
#' bs = bootstrap(m, no_of_sims=5000, threads=4, seed=1)
#' }
#' 
#' #' ## Generate the bootstrap_p_moby data set
#' \dontrun{
#' bs_p = bootstrap_p(m, no_of_sims=5000, threads=4, seed=1)
#' }
#' 
NULL

    
   

#' @name native_american           
#' @aliases NativeAmerican USAmerican us_american
#' @title Casualities in the American Indian Wars (1776 and 1890)
#' @description These data files contain the observed casualties in the American Indian Wars. The data sets 
#' \code{native_american} and \code{us_american} contain the casualties on the Native American and US American 
#' sides respectively. Each data set is a data frame, with two columns: the number of casualities and the
#' conflict date.
#' 
#' @docType data
#' @format Data frame
#' @source Friedman, Jeffrey A. "Using Power Laws to Estimate Conflict Size." The Journal of conflict resolution (2014).
NULL



#' @name population
#' @aliases Population
#' @title City boundaries and the universality of scaling laws
#' @description This data set contains the population size of cities and towns in England. For further details
#' on the algorithm used to determine city boundries, see the referenced paper.
#' 
#' @docType data
#' @format vector
#' @source Arcaute, Elsa, et al. "City boundaries and the universality of scaling laws." 
#' arXiv preprint arXiv:1301.1674 (2013).
NULL

#' @name swiss_prot
#' @aliases Swiss_prot
#' @title Word frequency in the Swiss-Prot database
#' @description This dataset contains all the words extracted from the 
#' Swiss-Prot version 9 data (with the resulting frequency for each word).
#' Other datasets for other database versions can be obtained by contacting 
#' Michael Bell 
#' (http://homepages.cs.ncl.ac.uk/m.j.bell1/annotationQualityPaper.php) 
#' 
#' Full details in http://arxiv.org/abs/arXiv:1208.2175v1
#' @docType data
#' @format data frame
#' @source Bell, MJ, Gillespie, CS, Swan, D, Lord, P. 
#' An approach to describing and analysing bulk biological annotation 
#' quality: A case study using UniProtKB. 
#' Bioinformatics 2012, 28, i562-i568.
NULL









