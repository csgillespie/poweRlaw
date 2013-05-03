test_that("Test CTN data fitting", {
  x = c(1, 1, 2, 2, 3, 4, 4)
  m = conpl$new(x)
  ##Set xmin = 1
  ##Check mle estimate
  xmin = 1
  mle = estimate_pars(m)
  m$setPars(mle)
  exact = 1 + length(x)*(sum(log(x/xmin))^(-1))
  expect_equal(m$getPars(), exact)
  
  ##Check setting data
  y = 10:12
  m$setDat(y)
  expect_equal(m$getDat(), y)
  
  
  ##Check Copying
  ##Testing the mle should also test for the internal
  ##Structure
  m = conpl$new(x)
  m$setXmin(2); m$setPars(3)
  m_new = m$copy()
  expect_equal(m$getXmin(), 2)
  expect_equal(m$getPars(), 3)
  expect_equal(estimate_pars(m_new), estimate_pars(m))
  expect_equal(estimate_xmin(m_new), estimate_xmin(m))
})