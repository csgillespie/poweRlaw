test_that("Test CTN data fitting", {
  x = c(1, 1, 2, 2, 3)
  m = conpl$new(x)
  ##Set xmin = 1
  xmin = 1
  mle = estimate_pars(m)
  exact = 1 + length(x)*(sum(log(x/xmin))^(-1))
  expect_equal(mle, exact)
  m$setXmin(xmin)
  expect_equal(mle, exact)
  
  
})