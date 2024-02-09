
test_that("are_all_close", {

  temp_C <- are_all_close(c(1:10), c(1:10), abs_tol = 1e-6, rel_tol = 1e-6);

  # Test that the result is the correct value
  expect_equal( temp_C, TRUE );
})

test_that("are_all_close_reltol", {

  temp_C <- are_all_close(c(1:10), c(1:10)+rnorm(10,0,1e-10), abs_tol = 1e-6, rel_tol = 1e-6);

  # Test that the result is the correct value
  expect_equal( temp_C, TRUE );
})

test_that("are_all_close_abstol", {

  temp_C <- are_all_close(c(1:10), c(1:10), abs_tol = 1e-6, rel_tol = 1e-6);

  # Test that the result is the correct value
  expect_equal( temp_C, TRUE );
})


test_that("linalg and optim least-sq coincide", {
  n_obs <- 32; n_pred <- 4
  data <- simulate_data(n_obs, n_pred, model = 'linear', seed = 1918)
  design <- data$design; outcome <- data$outcome
  via_linalg_out <- hiper_glm(design, outcome, model = 'linear')
  via_bfgs_out <- hiper_glm(
    design, outcome, model = 'linear', option = list(mle_solver = 'BFGS')
  )
  expect_true(are_all_close(
    coef(via_linalg_out), coef(via_bfgs_out), abs_tol = 1e-2, rel_tol = 1e-2
  ))
})










