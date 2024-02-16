

test_that("linalg and optim least-sq coincide", {
  n_obs <- 32
  n_pred <- 4
  data <- simulate_data(n_obs, n_pred, model = "linear", seed = 200)
  design <- data$design
  outcome <- data$outcome
  via_linalg_out <- hiper_glm(design, outcome, model = "linear")
  via_bfgs_out <- hiper_glm(design,
                            outcome,
                            model = "linear",
                            option = list(mle_solver = "BFGS"))
  expect_true(are_all_close(
    coef(via_linalg_out),
    coef(via_bfgs_out),
    abs_tol = 1e-2,
    rel_tol = 1e-2
  ))
})

test_that("Gradient formula works!", {
  n_obs <- 100
  n_pred <- 10
  data <-
    simulate_data(n_obs, n_pred, model = "linear", seed = 1000)
  design1 <- data$design
  outcome1 <- data$outcome
  beta1 <- data$coef_true

  a1 <-
    neg_gradient(beta =  beta1,
                 design = design1,
                 outcome = outcome1)
  a2 <-
    approx_grad(
      function(x)
        neg_log_likelihood(
          x,
          design = design1,
          outcome = outcome1,
          noise_var = 1
        ),
      x = beta1,
      dx = .Machine$double.eps ^ (1 / 3)
    )

  expect_true(are_all_close(a1, a2, abs_tol = 1e-4, rel_tol = 1e-4))
})
