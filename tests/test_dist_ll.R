test_that("Testing log-likelihood function", {
  
  ##Discrete Power-law
  discrete_data = readRDS("discrete_data.RData")
  m = displ$new(discrete_data)
  m$setPars(2.58); m$setXmin(2)
  expect_equal(dist_ll(m), -9155.62809, tol=1e-4)
  
  ##Discrete Log-normal
  x = c(1, 1)
  m = dislnorm$new(x)
  m$setPars(c(1, 1))
  
  l = (plnorm(1.5, 1, 1)-plnorm(0.5, 1, 1))/(1 - plnorm(0.5, 1, 1))
  ll1 = 2*log(l)
  expect_equal(dist_ll(m), ll1, tol=1e-4)
  
  x = c(1, 1, 3,4)
  m$setDat(x);  m$setXmin(2)
  ll3 = (plnorm(3.5, 1, 1)-plnorm(2.5, 1, 1))/(1 - plnorm(1.5, 1, 1))
  ll4 = (plnorm(4.5, 1, 1)-plnorm(3.5, 1, 1))/(1 - plnorm(1.5, 1, 1))
  ll = log(ll3) + log(ll4)
  expect_equal(dist_ll(m), ll, tol=1e-4)
  
  ##Discrete Poisson
  x = c(1, 1, 3, 4)
  m = dispois$new(x); m$setPars(2)
  ll = log(prod(dpois(x, 2)/(1-sum(dpois(0, 2)))))
  expect_equal(dist_ll(m), ll, tol=1e-4)
  
  m$setXmin(2)
  ll = log(prod(dpois(3:4, 2)/(1-sum(dpois(0:1, 2)))))
  expect_equal(dist_ll(m), ll, tol=1e-4)
  
  ##Discrete Exponential
  x = c(1, 1)
  m = disexp$new(x)
  m$setPars(1)
  l = (pexp(1.5, 1, 1)-pexp(0.5, 1, 1))/(1 - pexp(0.5, 1, 1))
  ll1 = 2*log(l)
  expect_equal(dist_ll(m), ll1, tol=1e-4)
  x = c(1, 1, 3,4)
  m$setDat(x);  m$setXmin(2)
  ll3 = (pexp(3.5, 1, 1)-pexp(2.5, 1, 1))/(1 - pexp(1.5, 1, 1))
  ll4 = (pexp(4.5, 1, 1)-pexp(3.5, 1, 1))/(1 - pexp(1.5, 1, 1))
  ll = log(ll3) + log(ll4)
  expect_equal(dist_ll(m), ll, tol=1e-4)
  
  
  #######################################
  #######################################
  #######################################  
  ##CTN Power-law
  ctn_data = readRDS("ctn_data.RData")
  m = conpl$new(ctn_data)
  m$setPars(2.53282); m$setXmin(1.43628)
  expect_equal(dist_ll(m), -9276.4, tolerance=0.00001);  
  
  ##Lognormal
  x = c(1, 1)
  m = conlnorm$new(x); m$setPars(c(1,1))
  
  ll1 = sum(log(dlnorm(x, 1, 1)/(1 - plnorm(1, 1, 1))))
  expect_equal(dist_ll(m), ll1, tol=1e-4)

  x = c(1, 1, 3, 4)
  m$setDat(x)
  ll2 = sum(log(dlnorm(3:4, 1, 1)/(1 - plnorm(1, 1, 1))))
  expect_equal(dist_ll(m), ll1+ll2, tol=1e-4)
  
  m$setXmin(2)
  ll3 = sum(log(dlnorm(3:4, 1, 1)/(1 - plnorm(2, 1, 1))))
  expect_equal(dist_ll(m), ll3, tol=1e-4)
  
  
  ##Exponential
  x = c(1, 1)
  m = conexp$new(x); m$setPars(1)
  
  ll1 = sum(log(dexp(x, 1)/(1 - pexp(1, 1))))
  expect_equal(dist_ll(m), ll1, tol=1e-4)
  
  x = c(1, 1, 3, 4)
  m$setDat(x)
  ll2 = sum(log(dexp(3:4, 1)/(1 - pexp(1, 1))))
  expect_equal(dist_ll(m), ll1+ll2, tol=1e-4)
  
  m$setXmin(2)
  ll3 = sum(log(dexp(3:4, 1)/(1 - pexp(2,1))))
  expect_equal(dist_ll(m), ll3, tol=1e-4)
  
  
  
}
)