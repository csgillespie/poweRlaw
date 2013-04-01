test_that("Test discrete power-law", {
  x = c(1, 1, 2, 4, 5, 5)
  m = displ$new(x)
  ##Set xmin = 1
  xmin = 1
 
  mle = estimate_pars(m)
  exact = 1 + length(x)*(sum(log(x/(xmin-1/2)))^(-1))
  expect_equal(mle, exact)
  m$setXmin(xmin)
  expect_equal(mle, exact)
  
  m$setPars(mle)
  expect_equal(dist_ll(m), -13.38, 1e-3)
  expect_equal(dist_data_cdf(m), c(1/3, 1/2, 4/6, 1))

  ##Check updating data set
  y = c(2, 4, 5, 5)
  m$setDat(y); xmin = 2
  m$getXmin()
  mle = estimate_pars(m)
  exact = 1 + length(y)*(sum(log(y/(xmin-1/2)))^(-1))
  expect_equal(mle, exact)
  
  ##Check setting xmin (floating comparison)
  m = displ$new(x)
  xmin = 2
  m$setXmin(xmin)
  mle = estimate_pars(m)
  expect_equal(mle, exact)
  expect_equal(dist_data_cdf(m), c(1/4, 1/2, 1))
  
  ##Check setting xmin (where there are no data points)
  y = c(4, 5, 5)
  m = displ$new(x)
  xmin = 3
  m$setXmin(xmin)
  mle = estimate_pars(m)
  exact = 1 + length(y)*(sum(log(y/(xmin-1/2)))^(-1))
  expect_equal(mle, exact)
  expect_equal(dist_data_cdf(m), c(1/3, 1))
  
}
)
