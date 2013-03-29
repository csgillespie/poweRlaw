test_that("Test CTN data fitting", {
  load("ctn_data.RData")
  m = conpl$new(ctn_data)
  m$setPars(2.53282)
  m$setXmin(1.43628)
  
  
  expect_equal(dist_ll(m), -9276.42, 1e-1);  
  
  ## MLE test
  m$setPars(2)
  m$setXmin(1.43628)
  expect_equal(estimate_pars(m), 2.53282, tol=1e-4);
  
  
  
  ## Estimate xmin
  est = estimate_xmin(m)
  expect_equal(est$pars, 2.53282, tol=1e-4)
  expect_equal(est$xmin, 1.43628, tol=1e-4)
  
  
  
})