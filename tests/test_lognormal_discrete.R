##Tests for discrete power-laws
##Small data, but specifically chosen data sets
test_that("Testing discrete log-normal", {
  x = c(1, 1, 2, 4, 5, 5)
  m = dislnorm$new(x)
  ##Set xmin = 1
  xmin = 1
  
  mle = estimate_pars(m)
  
  expect_equal(mle, c(0.8386, 0.7521), tol=1e-4)
#   dist_ll(m)
#   m$setXmin(xmin)
#   expect_equal(mle, exact)
#   
#   m$setPars(mle)
#   expect_equal(dist_ll(m), -13.38, 1e-3)
#   expect_equal(dist_data_cdf(m), c(1/3, 1/2, 4/6, 1))
#   
#   ##Check updating data set
#   y = c(2, 4, 5, 5)
#   m$setDat(y); xmin = 2
#   m$getXmin()
#   mle = estimate_pars(m)
#   exact = 1 + length(y)*(sum(log(y/(xmin-1/2)))^(-1))
#   expect_equal(mle, exact)
#   
#   ##Check setting xmin (floating comparison)
#   m = displ$new(x)
#   xmin = 2
#   m$setXmin(xmin)
#   mle = estimate_pars(m)
#   expect_equal(mle, exact)
#   expect_equal(dist_data_cdf(m), c(1/4, 1/2, 1))
#   
#   ##Check setting xmin  and data_cdf
#   ##Set xmin to place where there are no data points
#   y = c(4, 5, 5)
#   m = displ$new(x)
#   xmin = 3
#   m$setXmin(xmin)
#   mle = estimate_pars(m)
#   exact = 1 + length(y)*(sum(log(y/(xmin-1/2)))^(-1))
#   expect_equal(mle, exact)
#   expect_equal(dist_data_cdf(m), c(1/3, 1))
#   
#   ##Check Copying
#   ##Testing the mle should also test for the internal
#   ##Structure
#   m = displ$new(x)
#   m$setXmin(2); m$setPars(3)
#   m_new = m$copy()
#   expect_equal(m$getXmin(), 2)
#   expect_equal(m$getPars(), 3)
#   expect_equal(estimate_pars(m_new), estimate_pars(m))
#   expect_equal(estimate_xmin(m_new), estimate_xmin(m))
#   
}
)
