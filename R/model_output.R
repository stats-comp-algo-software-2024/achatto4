#' @export
coef.hiper_glm <- function(object, ...) {
  if (!inherits(object, "hiper_glm")) {
    stop("Object is not of class 'hiper_glm'")
  }
  return(object$coefficients)
}

#' @export
vcov.hiper_glm <- function(object, ...) {
  warning("The 'vcov.hiper_glm' function is yet to be implemented.")
}

#' @export
print.hiper_glm <- function(x, ...) {
  warning("The 'print.hiper_glm' function is yet to be implemented.")
}


are_all_close <- function(v, w, abs_tol = 1e-6, rel_tol = 1e-6) {
  abs_diff <- abs(v - w)
  are_all_within_atol <- all(abs_diff < abs_tol)
  are_all_within_rtol <- all(abs_diff < rel_tol * pmax(abs(v), abs(w)))#should be min
  return(are_all_within_atol && are_all_within_rtol)
}

simulate_data <- function(
    n_obs, n_pred, model = "linear", intercept = NULL,
    coef_true = NULL, design = NULL, seed = NULL, signal_to_noise = 5
) {
  if (!is.null(seed)) {
    set.seed(seed)
  }
  if (is.null(coef_true)) {
    coef_true <- rnorm(n_pred, sd = 1 / sqrt(n_pred))
  }
  if (is.null(design)) {
    design <- matrix(rnorm(n_obs * n_pred), nrow = n_obs, ncol = n_pred)
  }
  if (!is.null(intercept)) {
    if (!is.numeric(intercept)) {
      stop("The intercept argument must be numeric.")
    }
    coef_true <- c(intercept, coef_true)
    design <- cbind(rep(1, n_obs), design)
  }
  expected_mean <- as.vector(design %*% coef_true)
  noise_magnitude <- sqrt(var(expected_mean) / signal_to_noise^2)
  noise <- noise_magnitude * rnorm(n_obs)
  outcome <- expected_mean + noise
  return(list(design = design, outcome = outcome, coef_true = coef_true))
}

log_likelihood <- function(beta, design, outcome , noise_var = 1) {
  X <- as.matrix(design)
  y <- as.vector(outcome)
  mu <- X %*% beta
  n <- length(y)
  0.5 * (n * log(2 * pi) + n * log(1) + sum((y - mu)^2)/noise_var)
}

gradient <- function(beta, design, outcome, noise_var = 1) {
  X <- as.matrix(design)
  y <- as.vector(outcome)
  mu <- X %*% beta
  grad <- -t(X) %*% (y - mu)/noise_var
  return(grad)
}

# Run check for gradient

approx_grad <- function(func, x, dx = .Machine$double.eps^(1/3)) {
  d <- length(x)
  numerical_grad <- numeric(d)
  for (i in 1:d) {
    x_plus <- x
    x_minus <- x
    x_plus[i] <- x_plus[i] + dx
    x_minus[i] <- x_minus[i] - dx
    numerical_grad[i] <- (func(x_plus) - func(x_minus)) / (2 * dx)
  }
  return(numerical_grad)
}

####################ROUGH

#
#
# n_obs <- 32; n_pred <- 4
# data <- simulate_data(n_obs, n_pred, model = 'linear', seed = 1000)
# design1 <- data$design2; outcome1 <- data$outcome2; beta1 <- data$coef_true2
#
# log_likelihood <- function(beta= beta1, design= design1 , outcome= outcome1, noise_var = 1) {
#   X <- as.matrix(design)
#   y <- as.vector(outcome)
#   mu <- X %*% beta
#   n <- length(y)
#   0.5 * (n * log(2 * pi * noise_var) + sum((y - mu)^2) / noise_var)
# }
#
# gradient <- function(beta= beta1, design= design1 , outcome= outcome1, noise_var = 1) {
#   X <- as.matrix(design)
#   y <- as.vector(outcome)
#   mu <- X %*% beta
#   grad <- -t(X) %*% (y - mu) / noise_var
#   return(grad)
# }
#
# approx_grad(function(x) log_likelihood(x, design= design1 , outcome= outcome1, noise_var = 1), x=beta1, dx = .Machine$double.eps^(1/3))
#
#
# approx_grad <- function(func, x, dx = .Machine$double.eps^(1/3)) {
#   d <- length(x)
#   numerical_grad <- numeric(d)
#   for (i in 1:d) {
#     x_plus <- x
#     x_minus <- x
#     x_plus[i] <- x_plus[i] + dx
#     x_minus[i] <- x_minus[i] - dx
#     numerical_grad[i] <- (func(x_plus) - func(x_minus)) / (2 * dx)
#   }
#   return(numerical_grad)
# }
#
#
# a1=gradient()
# a2= approx_grad(function(x) log_likelihood(x, design= design1 , outcome= outcome1, noise_var = 1), x=beta1, dx = .Machine$double.eps^(1/3))
# are_all_close(a1,a2,abs_tol = 1e-6, rel_tol = 1e-6)
#
# library(stats)
#
# fn=function(beta)log_likelihood(beta, design= design1 , outcome= outcome1, noise_var = 1)
# grr = function(beta)gradient(beta, design= design1 , outcome= outcome1, noise_var = 1)
# opt=optim(beta1, fn, grr)
#
# calculate_mle_ls <- function(design= design1 , outcome= outcome1) {
#   X <- as.matrix(design)
#   y <- as.vector(outcome)
#   beta_hat <- solve(t(X) %*% X) %*% t(X) %*% y
#   return(beta_hat)
# }
#
# expect_true(are_all_close(
#   calculate_mle_ls(), opt$par, abs_tol = 1e-2, rel_tol = 1e-2
# ))





#library(testthat)
#library(hiperglm)
#test_check("hiperglm")



