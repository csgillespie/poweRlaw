lnorm_ll = function(x, pars, xmin) {
  m_log = pars[1]; sd_log = pars[2]
  n = length(x)
  ll = sum(dlnorm(x, m_log, sd_log, log=TRUE))
  threshold_prob = plnorm(xmin, 
                          m_log, sd_log, 
                          log.p=TRUE, lower.tail=FALSE)
  L = ll - n*threshold_prob
  return(L)
}




# 
# x = rlnorm(100)
# lnorm.loglike.tail(x,0, 1, 0)
# 
# lnorm.fit.max = function(x, threshold = 0) {
#   # Use moment-based estimator on whole data as starting point
#   initial.fit = lnorm.fit.moments(x)
#   theta_0 = c(initial.fit$meanlog,initial.fit$sdlog)
#   x = x[x>=threshold]
#   negloglike <- function(theta) {
#     -lnorm.loglike.tail(x,theta[1],theta[2],threshold)
#   }
#   est = nlm(f=negloglike,p=theta_0)
#   fit = list(type="lnorm",meanlog=est$estimate[1],sdlog=est$estimate[2],
#               datapoints.over.threshold = length(x), loglike=-est$minimum)
#   return(fit)
# }
