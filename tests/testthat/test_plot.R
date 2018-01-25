test_that("Test plotting", {
  skip_on_cran()  
  x = 1:4
  ##Discrete power-law
  mt = displ$new(x); mt$setPars(3); mt$setXmin(2)
  
  d_exact = data.frame(x=1:4, y=4:1/4)
  expect_equal(plot(mt, draw=FALSE), d_exact, tol=1e-4)
  d2 = lines(mt, draw=FALSE)
  expect_equal(d2$x, 2:4, tol=1e-4)
  y = c(0.7500, 0.2860, 0.1485)
  expect_equal(d2$y, y, tol=1e-4)

  ##Poisson
  m = dispois$new(x); m$setPars(1); m$setXmin(2)
  
  d_exact = data.frame(x=1:4, y=4:1/4)
  expect_equal(plot(m, draw=FALSE), d_exact, tol=1e-4)
  d2 = lines(m, draw=FALSE)
  y = c(0.75000,0.17735,0.03418)
  expect_equal(d2$x, 2:4, tol=1e-4)
  expect_equal(d2$y, y, tol=1e-5)
  
  ##Discrete Log normal
  m = dislnorm$new(x); m$setPars(c(1, 1)); m$setXmin(2)
  
  d_exact = data.frame(x=1:4, y=4:1/4)
  expect_equal(plot(m, draw=FALSE), d_exact, tol=1e-4)
  d2 = lines(m, draw=FALSE)
  y = c(0.75, 0.5628, 0.4318)
  expect_equal(d2$x, 2:4, tol=1e-4)
  expect_equal(d2$y, y, tol=1e-4)
  
  ##CTN power-law
  m = conpl$new(x); m$setPars(3); m$setXmin(2)
  
  d_exact = data.frame(x=1:4, y=4:1/4)
  expect_equal(plot(m, draw=FALSE), d_exact, tol=1e-4)
  d2 = lines(m, draw=FALSE, length.out=4)
  expect_equal(d2$x, c(2.000, 2.520, 3.175, 4.000), tol=1e-4)
  y = c(0.7500, 0.4725, 0.2976, 0.1875)
  expect_equal(d2$y, y, tol=1e-4)
  
  ##Log normal
  m = conlnorm$new(x); m$setPars(c(1, 1)); m$setXmin(2)
  
  d_exact = data.frame(x=1:4, y=4:1/4)
  expect_equal(plot(m, draw=FALSE), d_exact, tol=1e-4)
  d2 = lines(m, draw=FALSE, length.out=4)
  expect_equal(d2$x, c(2.000, 2.520, 3.175, 4.000), tol=1e-4)
  y = c(0.7500, 0.6408, 0.5298, 0.4226)
  expect_equal(d2$y, y, tol=1e-4)
  
  
  
  ##Exponential
  m = conexp$new(x); m$setPars(0.5); m$setXmin(2)
  
  d_exact = data.frame(x=1:4, y=4:1/4)
  expect_equal(plot(m, draw=FALSE), d_exact, tol=1e-4)
  d2 = lines(m, draw=FALSE, length.out=4)
  expect_equal(d2$x, c(2.000, 2.520, 3.175, 4.000), tol=1e-4)
  y = c(0.7500, 0.5783, 0.4168,0.2759)
  expect_equal(d2$y, y, tol=1e-4)
  
  
}
          )