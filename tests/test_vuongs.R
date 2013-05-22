test_that("Testing Vuong's", {

  data(moby)
  xmin = 2
  ##CTN PL vs Exp
  m1 = conpl$new(moby); m1$setXmin(xmin)
  m1$setPars(2.078)
    
  m2 = conexp$new(moby)
  m2$setXmin(xmin)
  m2$setPars(0.03643)
  
  v = vuong(m1, m2)
  expect_equal(v$total_ll_ratio, 14044, tol=1e-5)
  expect_equal(v$mean_ll_ratio, 1.449, tol=1e-3)
  expect_equal(v$sd_ll_ratio, 7.025, tol=1e-4)
  expect_equal(v$vuong_statistic, 20.3, tol=1e-3)
  expect_equal(v$p_one_sided, 1)
  expect_equal(v$p_two_sided, 0)
  
  
  ##CTN PL vs lognormal
  m3 = conlnorm(moby)
  m3$setXmin(xmin)
  m3$setPars(c(-6000, 75))
  v = vuong(m1, m3)
  expect_equal(v$total_ll_ratio, 0.7841, tol=1e-4)
  expect_equal(v$mean_ll_ratio, 8.088e-5, tol=1e-4)
  expect_equal(v$vuong_statistic, 0.6737, tol=1e-4)
  expect_equal(v$p_one_sided, 0.7498, tol=1e-4)
  expect_equal(v$p_two_sided, 0.5005, tol=1e-4)
  

  ##Test discrete
  m1 = displ$new(moby)
  m1$setXmin(xmin)
  m1$setPars(2)
  
  m2 = dislnorm$new(moby)  
  m2$setXmin(xmin)
  m2$setPars(c(-9, 4))
  v = vuong(m1, m2)
  expect_equal(v$total_ll_ratio, 2175, tol=1e0)
  
}
)