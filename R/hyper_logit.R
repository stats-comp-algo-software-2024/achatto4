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




#' High-Performance GLM
#'
#' Fits a high-performance generalized linear model.
#'
#'
#' @param design A design matrix.
#' @param outcome The outcome variable.
#' @param model The type of model. Currently supports 'linear'.
#' @param option A list of options. Currently supports 'mle_solver' with values 'LS' or 'BFGS'.
#' @return An object of class 'hiper_glm'.
#' @export
hiper_glm <- function(design, outcome, model = 'linear', option = list()) {
  if (model == 'linear') {
    if (!is.null(option) && 'mle_solver' %in% names(option) && option$mle_solver == 'BFGS') {
      # BFGS optimization

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



      opt <- optim(par = rep(0, ncol(design)), fn = log_likelihood, gr = gradient,
                   design = design, outcome = outcome)

      output <- list(coefficients = opt$par, model = model)
      class(output) <- "hiper_glm"
    } else {
      # LS optimization
      beta_hat <- solve(t(design) %*% design) %*% t(design) %*% outcome
      output <- list(coefficients = beta_hat, model = model)
      class(output) <- "hiper_glm"
    }
  } else {
    stop("Model type not supported. Only 'linear' model is currently supported.")
  }

  n_obs <- 100
  n_pred <- 10
  data <- simulate_data(n_obs, n_pred, model = 'linear', seed = 1000)
  design1 <- data$design
  outcome1 <- data$outcome
  beta1 <- data$coef_true

  a1 <- gradient(beta =  beta1, design = design1, outcome = outcome1)
  a2 <- approx_grad(function(x) log_likelihood(x, design = design1, outcome = outcome1, noise_var = 1), x = beta1, dx = .Machine$double.eps^(1/3))

  validation_result <- are_all_close(a1, a2, abs_tol = 1e-6, rel_tol = 1e-6)

  if (validation_result) {
    print("Gradient function has been validated.")
  } else {
    print("Gradient function validation failed. Use results with a pinch of salt!")
  }


  return(output)
}


# Example usage:
n_obs <- 32; n_pred <- 4
data <- simulate_data(n_obs, n_pred, model = 'linear', seed = 1918)
design <- data$design; outcome <- data$outcome
via_linalg_out <- hiper_glm(design, outcome, model = 'linear')
via_bfgs_out <- hiper_glm(
  design, outcome, model = 'linear', option = list(mle_solver = 'BFGS')
)
via_linalg_out
via_bfgs_out

