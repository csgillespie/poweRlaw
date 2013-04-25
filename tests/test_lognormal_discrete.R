##Tests for discrete power-laws
##Small data, but specifically chosen data sets
test_that("Testing discrete log-normal", {
  x = c(1, 1, 2, 4, 5, 5)
  m = dislnorm$new(x)
  expect_equal(m$getXmin(), 1)
  mle = estimate_pars(m)
  
  expect_equal(mle, c(0.8386, 0.7521), tol=1e-4)
  
  ##Check Setting pars
  m$setPars(c(1, 1))
  expect_equal(m$getPars(), c(1, 1))
  
  ##Check ll
  expect_equal(dist_ll(m), -12.1, tol=1e-3)
  
  ##Check setting xmin
  set.seed(1)
  x = round(rlnorm(10, 2, 1))
  m = dislnorm$new(x)
  est = estimate_xmin(m)
  m$setXmin(est)
  expect_equal(m$getXmin(), 5)
  expect_equal(m$getPars(), c(2.3779,0.6482), tol=1e-5)
  
  ##Check updating data set
  y = c(2, 4, 5, 5)
  m$setDat(y); xmin = 2
  m$getXmin()
  
  ##Check setting xmin  and data_cdf
  ##Set xmin to place where there are no data points
  y = c(4, 5, 5)
  m = dislnorm$new(y)
  xmin = 3
  expect_equal(dist_data_cdf(m), c(1/3, 1))
  
  ##Check Copying
  ##Testing the mle should also test for the internal
  ##Structure
  x = 1:10
  m = dislnorm$new(x)
  m$setXmin(2); m$setPars(c(1, 2))
  m_new = m$copy()
  expect_equal(m$getXmin(), 2)
  expect_equal(m$getPars(), c(1, 2))
  expect_equal(estimate_pars(m_new), estimate_pars(m))
  expect_equal(estimate_xmin(m_new), estimate_xmin(m))
  
}
)
