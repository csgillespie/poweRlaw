# The data files discrete_data.txt and continuous_data.txt are the 
#original example files from Aaron Clausets own implementation of 
#the power law fitting method. Fitting discrete_data.txt should 
#result in alpha = 2.58, xmin = 2 and a log-likelihood of -9155.63.

#I got the data from https://github.com/ntamas/plfit/
test_that("Discrete Clauset test", {
  load("discrete_data.RData")
  m = displ$new(discrete_data)
  
  ##Log-likelihood test
  m$setPars(2.58); m$setXmin(2)
  expect_equal(dist_ll(m), -9155.62809)
    
  ## Estimate xmin test
  est = estimate_xmin(m, pars=seq(2, 3, 0.01))
  expect_equal(est$pars, 2.58, tol=1e-1)
  expect_equal(est$xmin, 2)
  m$setXmin(est)
  expect_equal(dist_ll(m), -9155.62809)
})
