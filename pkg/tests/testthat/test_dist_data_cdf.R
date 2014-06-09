test_that("Testing dist_data_cdf function", {
  
  x = c(1, 1, 2, 4, 5, 5)
  mt = displ$new(x)
  expect_equal(dist_data_cdf(mt), c(1/3, 1/2, 4/6, 1), tol=1e-4)
  mt$setXmin(2)
  expect_equal(dist_data_cdf(mt), c(1/4, 1/2, 1), tol=1e-4)
  
  ##Discrete log normal
  x = c(1, 1, 2, 4, 5, 5)
  mt = dislnorm$new(x)
  expect_equal(dist_data_cdf(mt), c(1/3, 1/2, 4/6, 1), tol=1e-4)
  mt$setXmin(2)
  expect_equal(dist_data_cdf(mt), c(1/4, 1/2, 1), tol=1e-4)
  
  ##Discrete Poisson
  x = c(1, 1, 2, 4, 5, 5)
  mt = dispois$new(x)
  expect_equal(dist_data_cdf(mt), c(1/3, 1/2, 4/6, 1), tol=1e-4)
  mt$setXmin(2)
  expect_equal(dist_data_cdf(mt), c(1/4, 1/2, 1), tol=1e-4)
  
  
  ##I don't think this is correct
  ##But it's what clausett does
  ##Need to check
  ##CTN power-law
  x = c(1, 1, 2, 4, 5)
  mt = conpl$new(x)
  expect_equal(dist_data_cdf(mt), 0:4/5, tol=1e-4)
  mt$setXmin(2)
  expect_equal(dist_data_cdf(mt), 0:2/3, tol=1e-4)
  
  ##CTN power-law
  x = c(1, 1, 2, 4, 5)
  mt = conpl$new(x)
  expect_equal(dist_data_cdf(mt), 0:4/5, tol=1e-4)
  mt$setXmin(2)
  expect_equal(dist_data_cdf(mt), 0:2/3, tol=1e-4)
  
  ##CTN lognormal
  x = c(1, 1, 2, 4, 5)
  mt = conlnorm$new(x)
  expect_equal(dist_data_cdf(mt), 0:4/5, tol=1e-4)
  mt$setXmin(2)
  expect_equal(dist_data_cdf(mt), 0:2/3, tol=1e-4)
  
  
  ##CTN Exp
  x = c(1, 1, 2, 4, 5)
  mt = conexp$new(x)
  expect_equal(dist_data_cdf(mt), 0:4/5, tol=1e-4)
  mt$setXmin(2)
  expect_equal(dist_data_cdf(mt), 0:2/3, tol=1e-4)
  
  
  
}
)