#' Vuong's test for non-nested models
#' 
#' 
#' This function compares two models. The null hypothesis is that both classes of
#' distributions are equally far (in the Kullback-Leibler divergence/relative
#' entropy sense) from the true distribution.  If this is true, the
#' log-likelihood ratio should (asymptotically) have a Normal distribution
#' with mean zero. The test statistic is the sample average of the log
#' likelihood ratio, standardized by a consistent estimate of its standard
#' deviation.  If the null hypothesis is false, and one class of distributions is 
#' closer to the "truth", this test statistic goes to +-infinity with probability 1,
#' indicating the better-fitting class of distributions.  (See, in particular,
#' Theorem 5.1 on p. 318 of his paper.) 
#' 
#' The function returns a "one-sided" p-value, which is an upper limit
#' on getting that small a log likelihood ratio if the first distribution, \code{d1},
#'  is actually true, and a "two-sided" p-value, which is the probability of getting a log likelihood
#' ratio which deviates that much from zero in _either_ direction, if the two
#' distributions are actually equally good.  

#' @param d1 distribution objects
#' @param d2 distribution objects
#' @return A list giving total, mean and standard deviation of the log likelihood ratio points.
#' Also returned, is Vuong's test statistic (normalized pointwise log likelihood ratio), 
#' one-sided and two-sided p-values (based on asymptotical standard Gaussian distribution)
#' @note Code initially based on R code developed by Cosma Rohilla Shalizi (http://bactra.org/)
#' @references Vuong, Quang H. (1989): "Likelihood Ratio Tests for Model Selection and Non-Nested Hypotheses", Econometrica 57: 307--333.
#' @export
#' @examples
#' x = 3:10; xmin = 2
#' ##CTN PL
#' m1 = conpl$new(x)
#' m1$setXmin(xmin)
#' est1 = estimate_pars(m1)
#' m1$setPars(est1$pars)
#' 
#' ##Exponential
#' m2 = conexp$new(x)
#' m2$setXmin(xmin)
#' est2 = estimate_pars(m2)
#' m2$setPars(est2$pars)
#' 
#' #Vuong's test
#' compare_distributions(m1, m2)
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
  
  
  l = list(vuong_statistic = v, 
           p_one_sided = p1, p_two_sided=p2, 
           ratio = data.frame(x=q, ratio=ll_ratio_pts))
  class(l) = "compare_distributions"
  return(l)
}
