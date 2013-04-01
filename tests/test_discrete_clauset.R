test_that("Discrete Clauset test", {
  load("discrete_data.RData")
  m = displ$new(discrete_data)
  
  ##Log-likelihood test
  m$setPars(2.58); m$setXmin(2)
  expect_equal(dist_ll(m), -9155.62809)
  
  ## MLE test
  m$setPars(3)  
  expect_equal(estimate_pars(m), 2.437, tol=1e-4)
  
  ## Estimate xmin test
  est = estimate_xmin(m, pars=seq(2, 3, 0.01))
  expect_equal(est$pars, 2.58, tol=1e-1)
  expect_equal(est$xmin, 2)
  
})
