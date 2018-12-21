test_that("Testing checks", {
  x = c(-1, 2)
  expect_error(check_discrete_data(x))
  expect_error(check_ctn_data(x))
  x = c(1, 1.5)
  expect_error(check_discrete_data(x))
  
  x = 1:4
  expect_null(check_discrete_data(x))
  expect_null(check_ctn_data(x))
}
)