test_that("Testing dist_cdf function", {
  
  ##Discrete power-law
  x = c(1, 1, 3)
  mt = displ$new(x); mt$setPars(2)
  expect_equal(dist_cdf(mt), 
               c(0.6079, 0.6079, 0.8275), 
               tol=1e-4)
  
  expect_equal(dist_all_cdf(mt), 
               c(0.6079,0.7599,0.8275), 
               tol=1e-4)
  
  expect_equal(dist_cdf(mt, q=1), 0.6079, tol=1e-4)
  
  mt$setXmin(2)
  expect_equal(dist_cdf(mt, 3), 0.5599, tol=1e-4)
  expect_equal(dist_cdf(mt, c(1, 3)), c(0, 0.5599), tol=1e-4)
  
  mt$setXmin(3)
  expect_equal(dist_cdf(mt), c(0, 0, 0.2813), tol=1e-3)
  
  ##Discrete log-normal
  x = c(1, 1, 3)
  mt = dislnorm$new(x); mt$setPars(c(1, 1))
  cdf1 = (plnorm(1.5, 1, 1)-plnorm(0.5, 1, 1))/(1 - plnorm(0.5, 1, 1))
  cdf2 = (plnorm(2.5, 1, 1)-plnorm(0.5, 1, 1))/(1 - plnorm(0.5, 1, 1))
  cdf3 = (plnorm(3.5, 1, 1)-plnorm(0.5, 1, 1))/(1 - plnorm(0.5, 1, 1))
  expect_equal(dist_cdf(mt), c(cdf1, cdf1, cdf3), tol=1e-4)
  expect_equal(dist_all_cdf(mt), c(cdf1, cdf2, cdf3), tol=1e-4)
  expect_equal(dist_cdf(mt, q=c(1, 3)), c(cdf1, cdf3), tol=1e-4)
  
  mt$setXmin(2)
  cdf3 = (plnorm(3.5, 1, 1)-plnorm(1.5, 1, 1))/(1 - plnorm(1.5, 1, 1))
  expect_equal(dist_cdf(mt), c(0, 0, cdf3), tol=1e-4)
  expect_equal(dist_cdf(mt, c(1, 3)), c(0, cdf3), tol=1e-4)
  
  mt$setXmin(3)
  cdf3 = cdf3 = (plnorm(3.5, 1, 1)-plnorm(2.5, 1, 1))/(1 - plnorm(2.5, 1, 1))
  expect_equal(dist_cdf(mt), c(0, 0, cdf3), tol=1e-4)
  
  ##Discrete Poisson
  x = c(1, 1, 3)
  mt = dispois$new(x); mt$setPars(1)
  cdf1 = dpois(1, 1)/(1 - dpois(0, 1))
  cdf2 = sum(dpois(1:2, 1))/(1 - dpois(0, 1))
  cdf3 = sum(dpois(1:3, 1))/(1 - dpois(0, 1))
  expect_equal(dist_cdf(mt), c(cdf1, cdf1, cdf3), tol=1e-4)
  expect_equal(dist_all_cdf(mt), c(cdf1, cdf2, cdf3), tol=1e-4)
  expect_equal(dist_cdf(mt, q=c(1, 3)), c(cdf1, cdf3), tol=1e-4)
  
  mt$setXmin(2)
  cdf3 = sum(dpois(2:3, 1)/(1 - sum(dpois(0:1, 1))))
  expect_equal(dist_cdf(mt), c(0, 0, cdf3), tol=1e-4)
  expect_equal(dist_cdf(mt, c(1, 3)), c(0, cdf3), tol=1e-4)
  
  mt$setXmin(3)
  cdf3 = sum(dpois(3, 1)/(1 - sum(dpois(0:2, 1))))
  expect_equal(dist_cdf(mt), c(0, 0, cdf3), tol=1e-4)
  
  ##Discrete Exp
  x = c(1, 1, 3)
  mt = disexp$new(x); mt$setPars(1)
  cdf1 = (pexp(1.5, 1, 1)-pexp(0.5, 1, 1))/(1 - pexp(0.5, 1, 1))
  cdf2 = (pexp(2.5, 1, 1)-pexp(0.5, 1, 1))/(1 - pexp(0.5, 1, 1))
  cdf3 = (pexp(3.5, 1, 1)-pexp(0.5, 1, 1))/(1 - pexp(0.5, 1, 1))
  expect_equal(dist_cdf(mt), c(cdf1, cdf1, cdf3), tol=1e-4)
  expect_equal(dist_all_cdf(mt), c(cdf1, cdf2, cdf3), tol=1e-4)
  expect_equal(dist_cdf(mt, q=c(1, 3)), c(cdf1, cdf3), tol=1e-4)
  
  mt$setXmin(2)
  cdf3 = (pexp(3.5, 1, 1)-pexp(1.5, 1, 1))/(1 - pexp(1.5, 1, 1))
  expect_equal(dist_cdf(mt), c(0, 0, cdf3), tol=1e-4)
  expect_equal(dist_cdf(mt, c(1, 3)), c(0, cdf3), tol=1e-4)
  
  mt$setXmin(3)
  cdf3 = cdf3 = (pexp(3.5, 1, 1)-pexp(2.5, 1, 1))/(1 - pexp(2.5, 1, 1))
  expect_equal(dist_cdf(mt), c(0, 0, cdf3), tol=1e-4)
  
  
  
  ################################
  #CTN Distributions
  ################################
  ##Power law
  x = c(2, 2, 4)
  mt = conpl$new(x); 
  mt$setXmin(1); mt$setPars(2)
  expect_equal(dist_cdf(mt), c(0.5, 0.5, 0.75), tol=1e-4)
  expect_equal(dist_all_cdf(mt), 
               c(0, 0.5, 2/3, 0.75), tol=1e-4)
  expect_equal(dist_cdf(mt, q=c(2, 4)), c(0.5, 0.75), tol=1e-4)
  
  mt$setXmin(2)
  expect_equal(dist_cdf(mt), c(0, 0, 0.5), tol=1e-4)
  mt$setXmin(3)
  expect_equal(dist_cdf(mt), c(0, 0, 0.25), tol=1e-4)
  expect_equal(dist_cdf(mt, c(1, 4)), c(0, 0.25), tol=1e-4)
  
  ##Log normal  
  x = c(2, 2, 4)
  mt = conlnorm$new(x); 
  mt$setXmin(1); mt$setPars(c(1, 1))
  cdf2 = (plnorm(2, 1, 1)-plnorm(1, 1, 1))/(1 - plnorm(1, 1, 1))
  cdf3 = (plnorm(3, 1, 1)-plnorm(1, 1, 1))/(1 - plnorm(1, 1, 1))
  cdf4 = (plnorm(4, 1, 1)-plnorm(1, 1, 1))/(1 - plnorm(1, 1, 1))
  expect_equal(dist_cdf(mt), c(cdf2, cdf2, cdf4), tol=1e-4)
  expect_equal(dist_all_cdf(mt), c(0, cdf2, cdf3, cdf4), tol=1e-4)
  expect_equal(dist_cdf(mt, q=c(2, 4)), c(cdf2, cdf4), tol=1e-4)

  cdf4 = (plnorm(4, 1, 1)-plnorm(2, 1, 1))/(1 - plnorm(2, 1, 1))
  mt$setXmin(2)
  expect_equal(dist_cdf(mt), c(0, 0, cdf4), tol=1e-4)
  expect_equal(dist_cdf(mt, c(1, 4)), c(0, cdf4), tol=1e-4)
  
  mt$setXmin(4)
  expect_equal(dist_cdf(mt), c(0,0,0), tol=1e-8)
  
  ##Exponential
  x = c(2, 2, 4)
  mt = conexp$new(x); 
  mt$setXmin(1); mt$setPars(c(1))
  cdf2 = (pexp(2, 1)-pexp(1, 1))/(1 - pexp(1, 1))
  cdf3 = (pexp(3, 1)-pexp(1, 1))/(1 - pexp(1, 1))
  cdf4 = (pexp(4, 1)-pexp(1, 1))/(1 - pexp(1, 1))
  expect_equal(dist_cdf(mt), c(cdf2, cdf2, cdf4), tol=1e-4)
  expect_equal(dist_all_cdf(mt), c(0, cdf2, cdf3, cdf4), tol=1e-4)
  expect_equal(dist_cdf(mt, q=c(2, 4)), c(cdf2, cdf4), tol=1e-4)
  
  cdf4 = (pexp(4, 1)-pexp(2, 1))/(1 - pexp(2, 1))
  mt$setXmin(2)
  expect_equal(dist_cdf(mt), c(0, 0, cdf4), tol=1e-4)
  expect_equal(dist_cdf(mt, c(0, 4)), c(0, cdf4), tol=1e-4)
  
  mt$setXmin(4)
  expect_equal(dist_cdf(mt), c(0,0, 0), tol=1e-4)
  
}
)
