test_that("Testing Vuong's", {

  data(moby)
  xmin = 2
  ##CTN PL vs Exp
  m1 = conpl$new(moby); m1$setXmin(xmin)
  m1$setPars(2.078)
    
  m2 = conexp$new(moby)
  m2$setXmin(xmin)
  m2$setPars(0.03643)
  
  v = compare_distributions(m1, m2)
  expect_equal(dim(v$ratio), c(9694, 2), tol=1e-4)
  expect_equal(v$test_statistic, 20.3, tol=1e-3)
  expect_equal(v$p_one_sided, 1, tol=1e-4)
  expect_equal(v$p_two_sided, 0, tol=1e-4)
  
  
  ##CTN PL vs lognormal
  m3 = conlnorm(moby)
  m3$setXmin(xmin)
  m3$setPars(c(-6000, 75))
  v = compare_distributions(m1, m3)
  expect_equal(v$test_statistic, 0.6737, tol=1e-4)
  expect_equal(v$p_one_sided, 0.7498, tol=1e-4)
  expect_equal(v$p_two_sided, 0.5005, tol=1e-4)
  

  ##Test discrete
  xmin = 2
  m1 = displ$new(moby)
  m1$setXmin(xmin)
  m1$setPars(2)
  
  m2 = dislnorm$new(moby)  
  m2$setXmin(xmin)
  m2$setPars(c(-9, 4))
  v = compare_distributions(m1, m2)
  expect_equal(sum(v$ratio$ratio), -64.85, tol=1e0)
  
}
)