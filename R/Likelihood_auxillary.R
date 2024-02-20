

#' we define the function neg_log_likelihood which is the negative of the log-likelihood.
neg_log_likelihood <-
  function(beta, design = design, outcome = outcome , noise_var = 1) {
    mu <- design %*% beta
    n <- length(outcome)
    0.5 * (sum((outcome - mu) ^ 2) / noise_var)
  }

#' we define the function neg_gradient which is the negative of the gradient.
neg_gradient <- function(beta, design = design, outcome = outcome , noise_var = 1) {
  mu <- design %*% beta
  grad <- -t(design) %*% (outcome - mu) / noise_var
  return(grad)
}

# Function to calculate the log-likelihood under the logistic model
log_likelihood_logistic <- function(beta, design, outcome) {
  eta <- design %*% beta
  log_likelihood <- sum(outcome[[1]] * eta - log(1 + exp(eta)))
  return(-log_likelihood)  # Negative because optimizers typically minimize
}

# Function to calculate the gradient of the log-likelihood under the logistic model
gradient_log_likelihood_logistic <- function(beta, design, outcome) {
  eta <- design %*% beta
  p <- exp(eta) / (1 + exp(eta))
  gradient <- t(design) %*% (outcome[[1]] - p)
  return(gradient)  # Negative because optimizers typically minimize
}


# Function to calculate the Hessian of the negative log-likelihood under the logistic model
hessian_log_likelihood_logistic <- function(beta, design) {
  eta <- design %*% beta
  p <- exp(eta) / (1 + exp(eta))
  W <- diag(c(p) * c(1 - p))  # Diagonal matrix of weights
  Hessian <- -t(design) %*% W %*% design
  return(Hessian)
}

# Function to perform Newton's method for logistic regression
newton_logistic <- function(design, outcome, start_values = NULL, max_iter = 10000) {
  if (is.null(start_values)) {
    start_values <- rep(0, ncol(design))
  }
  beta <- start_values
  iter=1
  for (iter in 1:max_iter) {
    gradient <- gradient_log_likelihood_logistic(beta, design, outcome)
    hessian <- hessian_log_likelihood_logistic(beta, design)
    update <- solve(hessian, gradient)
    beta <- beta - update
  }
  return(beta)
}


