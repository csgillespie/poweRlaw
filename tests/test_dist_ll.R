test_that("Testing log-likelihood function", {
  
  ##Discrete Power-law
  load("discrete_data.RData")
  m = displ$new(discrete_data)
  m$setPars(2.58); m$setXmin(2)
  expect_equal(dist_ll(m), -9155.62809)
  
  ##Discrete Log-normal
  x = c(1, 1)
  m = dislnorm$new(x)
  m$setPars(c(1, 1))
  
  l = (plnorm(1.5, 1, 1)-plnorm(0.5, 1, 1))/(1 - plnorm(0.5, 1, 1))
  ll1 = 2*log(l)
  expect_equal(dist_ll(m), ll1)
  
  x = c(1, 1, 3,4)
  m$setDat(x);  m$setXmin(2)
  ll3 = (plnorm(3.5, 1, 1)-plnorm(2.5, 1, 1))/(1 - plnorm(1.5, 1, 1))
  ll4 = (plnorm(4.5, 1, 1)-plnorm(3.5, 1, 1))/(1 - plnorm(1.5, 1, 1))
  ll = log(ll3) + log(ll4)
  expect_equal(dist_ll(m), ll)
  
  ##Discrete Poisson
  x = c(1, 1, 3, 4)
  m = dispois$new(x); m$setPars(2)
  ll = log(prod(dpois(x, 2)/(1-sum(dpois(0, 2)))))
  expect_equal(dist_ll(m), ll)
  
  m$setXmin(2)
  ll = log(prod(dpois(3:4, 2)/(1-sum(dpois(0:1, 2)))))
  expect_equal(dist_ll(m), ll)
  
  #######################################
  #######################################
  #######################################  
  ##CTN Power-law
  load("ctn_data.RData")
  m = conpl$new(ctn_data)
  m$setPars(2.53282); m$setXmin(1.43628)
  expect_equal(dist_ll(m), -9276.42, 1e-1);  
  
  ##Lognormal
  x = c(1, 1)
  m = conlnorm$new(x); m$setPars(c(1,1))
  
  ll1 = sum(log(dlnorm(x, 1, 1)/(1 - plnorm(1, 1, 1))))
  expect_equal(dist_ll(m), ll1)

  x = c(1, 1, 3, 4)
  m$setDat(x)
  ll2 = sum(log(dlnorm(3:4, 1, 1)/(1 - plnorm(1, 1, 1))))
  expect_equal(dist_ll(m), ll1+ll2)
  
  m$setXmin(2)
  ll3 = sum(log(dlnorm(3:4, 1, 1)/(1 - plnorm(2, 1, 1))))
  expect_equal(dist_ll(m), ll3)
  
}
)