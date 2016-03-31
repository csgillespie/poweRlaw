test_that("Testing estimate_xmin accuracy", {
  skip_on_cran()  
  ##Discrete Power-law
  discrete_data = readRDS("discrete_data.RData")
  mt = displ$new(discrete_data)
  est = estimate_xmin(mt, pars=seq(2, 3, 0.01))
  expect_equal(est$pars, 2.58, tol=1e-1)
  expect_equal(est$xmin, 2, tol=1e-3)
  
  ##Poisson
  set.seed(1)
  x = rpois(10000, 10)
  x = x[x >= 10]
  x = c(x, sample(1:9, 10000-length(x), replace=TRUE))
  
  mt = dispois$new(x)
  est = estimate_xmin(mt)
  expect_equal(est$pars, 9.948, tol=1e-4)
  expect_equal(est$xmin, 13, tol=1e-3)
  
  ##Discrete Log-normal
  set.seed(1)
  x = ceiling(rlnorm(10000, 3, 1))
  x = x[x >= 10]
  x = c(x, sample(1:9, 10000-length(x), replace=TRUE))
  
  mt = dislnorm$new(x)
  est = estimate_xmin(mt)
  expect_equal(est$pars, c(2.981, 1.012), tol=1e-3)
  expect_equal(est$xmin, 10, tol=1e-3)
  
  
  ##CTN Power-law
  ##Takes a while
  if(interactive()) {
    ctn_data = readRDS("ctn_data.RData")
    mt = conpl$new(ctn_data)
    est = estimate_xmin(mt)
    expect_equal(est$pars, 2.53255, tol=1e-3)
    expect_equal(est$xmin, 1.43628, tol=1e-3)
  }
  ##Log-normal
  set.seed(1)
  x = rlnorm(10000, 3, 1)
  x = x[x >= 10]
  x = c(x, runif(10000-length(x), 0, 10))
  
  mt = conlnorm$new(x)
  est = estimate_xmin(mt, xmins=1:50)
  expect_equal(est$pars, c(2.966, 1.022), tol=1e-4)
  expect_equal(est$xmin, 10, tol=1e-3)
  
  
  ##Exponential
  set.seed(1)
  x = rexp(10000, 0.01)
  x = x[x >= 10]
  x = c(x, runif(10000-length(x), 0, 10))
  
  mt = conexp$new(x)
  est = estimate_xmin(mt, xmins=1:50)
  
  expect_equal(est$pars, 0.01003, tol=1e-3)
  expect_equal(est$xmin, 4, tol=1e-3)
 
  #########################################
  ## Edge cases
  x = c(2, 2,2)
  mt = displ$new(x)
  est = estimate_xmin(mt)
  expect_true(is.infinite(est$gof))
  expect_true(is.na(est$pars))

  ## Empty object
  mt = displ$new()
  est = estimate_xmin(mt)
  expect_true(is.infinite(est$gof))
  expect_true(is.na(est$pars))

}
)

test_that("Testing estimate_xmin distance measures", {
  skip_on_cran() 
  discrete_data = 1:10
  mt = displ$new(discrete_data)
  est = estimate_xmin(mt, pars=seq(2, 3, 0.01), distance="ks")
  expect_equal(est$pars, 3, tol=1e-1)
  expect_equal(est$xmin, 5, tol=1e-3)

  est = estimate_xmin(mt, pars=seq(2, 3, 0.01), distance="reweight")
  expect_equal(est$pars, 2.57, tol=1e-1)
  expect_equal(est$xmin, 4, tol=1e-3)
  
}
)

