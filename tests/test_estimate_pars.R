test_that("Testing estimate_pars function", {

  ##Discrete Power-law
  load("discrete_data.RData")
  mt = displ$new(discrete_data)
  mt$setXmin(2)
  est = estimate_pars(mt, pars=seq(2, 3, 0.01))
  expect_equal(est$pars, 2.58, tol=1e-1)
  est = estimate_pars(mt)
  expect_equal(est$pars, 2.437, tol=1e-1)
  
  ##CTN Power-law
  load("ctn_data.RData")
  mt = conpl$new(ctn_data)
  mt$setXmin(1.43628)
  
  est = estimate_pars(mt)
  expect_equal(est$pars, 2.514, tol=1e-3)
  
  est = estimate_pars(mt, pars=seq(2.4, 2.6, 0.0001))
  expect_equal(est$pars, 2.514, tol=1e-3)
  
  est$pars
  
  est$pars
  
  
  expect_equal(est$pars, 2.53282, tol=1e-4)
  expect_equal(est$xmin, 1.43628, tol=1e-4)


}
)