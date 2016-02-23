#' Vuong's test for non-nested models
#' 
#' Since it is possible to fit power law models to any data set, 
#' it is recommended that alternative distributions are considered. 
#' A standard technique is to use Vuong's test.
#' This is a likelihood ratio test for model selection using the
#' Kullback-Leibler criteria. 
#' The test statistic, \code{R}, is the ratio of the
#' log-likelihoods of the data between the two competing models. 
#' The sign of \code{R} indicates which model is better. 
#' Since the value of \code{R} is esimated, 
#' we use the method proposed by Vuong, 1989 to select the model.

#' This function compares two models. 
#' The null hypothesis is that both classes of distributions are 
#' equally far from the true distribution.  If this is true, the
#' log-likelihood ratio should (asymptotically) have a Normal distribution
#' with mean zero. The test statistic is the sample average of the 
#' log-likelihood ratio, standardized by a consistent estimate of its standard
#' deviation.  
#' If the null hypothesis is false, and one class of distributions is 
#' closer to the "truth", the test statistic goes to +/-infinity 
#' with probability 1, indicating the better-fitting class of distributions. 
#' 
#' @return This function returns
#' \describe{
#' \item{\code{test_statistic}}{The test statistic.}
#' \item{\code{p_one_sided}}{A one-sided p-value, which is an upper limit
#' on getting that small a log-likelihood ratio if the 
#' first distribution, \code{d1}, is actually true.}
#' \item{\code{p_two_sided}}{A two-sided p-value, which is the probability 
#' of getting a log-likelihood ratio which deviates that much from zero 
#' in either direction, if the two distributions are actually equally good.}
#' \item{\code{ratio}}{A data frame with two columns. The first column is 
#' the \code{x} value and second column is the difference in 
#' log-likelihoods.}}
#' @param d1 A distribution object
#' @param d2 A distribution object
#' @note Code initially based on R code developed by 
#' Cosma Rohilla Shalizi (\url{http://bactra.org/}). 
#' Also see Appendix C in Clauset et al, 2009.
#' @references Vuong, Quang H. (1989): 
#' "Likelihood Ratio Tests for Model Selection and Non-Nested Hypotheses", 
#' Econometrica 57: 307--333.
#' @importFrom stats sd pnorm
#' @export
#' @examples
#' ########################################################
#' # Example data                                         #
#' ########################################################
#' x = rpldis(100, xmin=2, alpha=3)
#' 
#' ########################################################
#' ##Continuous power law                                 #
#' ########################################################
#' m1 = conpl$new(x)
#' m1$setXmin(estimate_xmin(m1))
#' 
#' ########################################################
#' ##Exponential           
#' ########################################################
#' m2 = conexp$new(x)
#' m2$setXmin(m1$getXmin())
#' est2 = estimate_pars(m2)
#' m2$setPars(est2$pars)
#' 
#' ########################################################
#' ##Vuong's test                                         #
#' ########################################################
#' comp = compare_distributions(m1, m2)
#' plot(comp)
#' @export
compare_distributions = function(d1, d2) {
  
  if(!(inherits(d1, "discrete_distribution") == inherits(d2, "discrete_distribution")))
    stop("You seem to be comparing a discrete distribution with a continuous distribution")
  
  xmin1 = d1$getXmin(); xmin2 = d2$getXmin()
  if(xmin1 != xmin2)
    stop("Lower threshold, xmin, should be the same in both models")
  q = d1$getDat(); q = q[q >= d1$getXmin()]
  ll_ratio_pts = dist_pdf(d1, q, log=TRUE) - dist_pdf(d2, q, log=TRUE)
  
  m = mean(ll_ratio_pts); s = sd(ll_ratio_pts)
  v = sqrt(length(ll_ratio_pts))*m/s
  p1 = pnorm(v)
  
  if (p1 < 0.5) {p2 = 2*p1} else {p2 = 2*(1-p1)}
  
  l = list(test_statistic = v, 
           p_one_sided = p1, p_two_sided=p2, 
           ratio = data.frame(x=q, ratio=ll_ratio_pts))
  class(l) = "compare_distributions"
  return(l)
}
