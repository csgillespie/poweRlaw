test_that("Testing estimate_pars function", {
  skip_on_cran()  
  
  ## FIX:For some reason set.seed() gives different values when run 
  ## via R CMD check
  skip_if_not(interactive())
  message("Hi")
  ##Discrete Power-law
  discrete_data = readRDS("discrete_data.RData")
  mt = displ$new(discrete_data)
  mt$setXmin(2)
  est = estimate_pars(mt, pars=seq(2, 3, 0.01))
  expect_equal(est$pars, 2.58, tol=1e-3)
  est = estimate_pars(mt)
  expect_equal(est$pars, 2.583, tol=1e-3)
  
  ##Discrete Poisson
  x = rep(2, 10000)
  mt = dispois$new(x)
  mt$setXmin(0)
  est = estimate_pars(mt)
  expect_equal(est$pars, 2, tol=1e-3)
  est = estimate_pars(mt, pars=seq(1.5, 2.5, 0.01))
  expect_equal(est$pars, 2, tol=1e-3)
  #1.026 1.964
  ##Discrete Log normal
  set.seed(1)
  z = dislnorm$new()
  z$setPars(c(1, 2))
  z$setXmin(2)
  x = dist_rand(z, 1000)
  mt = dislnorm$new(x)
  mt$setXmin(2)
  est = estimate_pars(mt)
  expect_equal(est$pars, c(0.979, 2.055), tol=1e-2)
  
  m_pars = expand.grid(seq(0.5, 1.5, 0.05), seq(1.5, 2.5, 0.05))
  est = estimate_pars(mt, m_pars)
  expect_equal(est$pars, c(1.0, 2.05), tol=1e-3)
  
  ##CTN Power-law
  ctn_data = readRDS("ctn_data.RData")
  mt = conpl$new(ctn_data)
  mt$setXmin(1.43628)
  
  est = estimate_pars(mt)
  expect_equal(est$pars, 2.533, tol=1e-3)
  
  est = estimate_pars(mt, pars=seq(2.3, 2.7, 0.0001))
  expect_equal(est$pars, 2.53282, tol=1e-3)
  
  
  ##Log normal
  set.seed(1)
  x = rlnorm(1000, 1, 2)
  mt = conlnorm$new(x)
  mt$setXmin(0)
  est = estimate_pars(mt)
  expect_equal(est$pars, c(0.98, 2.07), tol=1e-2)
  m_pars = expand.grid(seq(0.85, 1.25, 0.01), seq(1.85, 2.25, 0.01))
  est = estimate_pars(mt, m_pars)
  expect_equal(est$pars, c(0.98, 2.07), tol=1e-3)
  
  ##Exponential
  x = rep(2, 10000)
  mt = conexp$new(x)
  mt$setXmin(0)
  est = estimate_pars(mt)
  expect_equal(est$pars, 1/2, tol=1e-5)
  est = estimate_pars(mt, pars=seq(0.1, 1, 0.01))
  expect_equal(est$pars, 1/2, tol=1e-3)
  
  
}
)