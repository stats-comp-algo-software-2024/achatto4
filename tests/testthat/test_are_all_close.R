



#' Tests
test_that("same vectors pass the test", {
  expect_true(are_all_close(c(1:10), c(1:10), abs_tol = 1e-6, rel_tol = 1e-6))
})

#' We expect a false test and that is what we get (abs_tol violated)
test_that("abs_tol_violated", {
  expect_false(are_all_close(
    c(1:10),
    c(1:10) + 10 ^ -6,
    abs_tol = 1e-6,
    rel_tol = 1e-3
  ))
})

#' We expect a false test and that is what we get (rel_tol violated)
test_that("rel_tol_violated", {
  expect_false(are_all_close(
    c(1:10),
    c(1:10) + 10 ^ -6,
    abs_tol = 1e-6,
    rel_tol = 1e-3
  ))
})
