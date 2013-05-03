test_that("Testing discrete poisson", {
  set.seed(1)
  x = rpois(1000, 10)
  m = dispois$new(x)
  mle = estimate_pars(m)
  expect_equal(mle$pars, 9.775, tol=1e-4)
  
  
  ##Check updating data set
  x = 1000:1004
  m$setDat(x)
  expect_equal(m$getDat(), x)
  
  ##Check Setting pars
  m$setPars(1002)
  expect_equal(m$getPars(), 1002)
  
  ##Check ll
  m$setXmin(1)
  expect_equal(dist_ll(m), sum(dpois(x, 1002, log=TRUE)))
  
}
)
