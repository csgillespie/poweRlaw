#Fitting continuous_data.txt should yield 
#alpha=2.53282, xmin=1.43628 and a 
#log-likelihood of -9276.42.
test_that("CTN Clauset test", 
{
  load("ctn_data.RData")
  m = conpl$new(ctn_data)
  m$setPars(2.53282); m$setXmin(1.43628)
  expect_equal(dist_ll(m), -9276.42, 1e-1);  
  
  ## Estimate xmin
  m = conpl$new(ctn_data)
  est = estimate_xmin(m)
  expect_equal(est$pars, 2.53282, tol=1e-4)
  expect_equal(est$xmin, 1.43628, tol=1e-4)
}
)

