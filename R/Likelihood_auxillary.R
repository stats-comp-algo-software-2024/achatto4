


#' we define the function neg_log_likelihood which is the negative of the log-likelihood.
neg_log_likelihood <-
  function(beta,
           design = design,
           outcome = outcome ,
           noise_var = 1) {
    mu <- design %*% beta
    n <- length(outcome)
    0.5 * (sum((outcome - mu) ^ 2) / noise_var)
  }

#' we define the function neg_gradient which is the negative of the gradient.
neg_gradient <-
  function(beta,
           design = design,
           outcome = outcome ,
           noise_var = 1) {
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
gradient_log_likelihood_logistic <-
  function(beta, design, outcome) {
    eta <- design %*% beta
    p <- exp(eta) / (1 + exp(eta))
    gradient <- t(design) %*% (outcome[[1]] - p)
    return(-gradient)  # Negative because optimizers typically minimize
  }


# Function to calculate the Hessian of the negative log-likelihood under the logistic model
hessian_log_likelihood_logistic <- function(beta, design) {
  eta <- design %*% beta
  p <- exp(eta) / (1 + exp(eta))
  W <- diag(c(p) * c(1 - p))  # Diagonal matrix of weights
  Hessian <- -t(design) %*% W %*% design
  return(Hessian)
}

#' Function to perform Newton's method for logistic regression
newton_logistic <-
  function(design,
           outcome,
           start_values = NULL,
           max_iter = 10000) {
    if (is.null(start_values)) {
      start_values <- rep(0, ncol(design))
    }
    beta <- start_values
    iter <- 1
    diff <- 1
    while (iter < max_iter & diff > 10 ^ -8) {
      gradient <- gradient_log_likelihood_logistic(beta, design, outcome)
      hessian <- hessian_log_likelihood_logistic(beta, design)
      update <- solve(hessian, gradient)
      init_log <-
        log_likelihood_logistic(beta = beta,
                                design = design,
                                outcome = outcome)
      beta <- beta + update
      fin_log <-
        log_likelihood_logistic(beta = beta,
                                design = design,
                                outcome = outcome)
      diff <- abs(init_log - fin_log)
      iter <- iter + 1
      if (iter == max_iter) {
        print("Newton did not converge within max number of iterations.")
      }
    }
    return(beta)
  }

#' Function to find MLE via Pseudo inverse
find_mle_pseudoinverse <- function(design, outcome) {
  beta_hat <- solve(t(design) %*% design, t(design) %*% outcome)
  return(beta_hat)
}

#' Function to find MLE via BFGS
find_mle_bfgs <- function(design, outcome) {
  opt <- optim(
    par = rep(0, ncol(design)),
    fn = neg_log_likelihood,
    gr = neg_gradient,
    design = design,
    outcome = outcome
  )
  return(opt$par)
}

#' Function to find MLE via BFGS for Logistic Model
find_mle_bfgs_log <- function(design, outcome) {
  opt <- optim(
    par = rep(0, ncol(design)),
    fn = log_likelihood_logistic,
    gr = gradient_log_likelihood_logistic,
    design = design,
    outcome = outcome
  )
  return(opt$par)
}

#' Function to find MLE via Newton's algorithm for Logistic Model
newton_algo <- function(design, outcome) {
  opt1 <- newton_logistic(
    design = design,
    outcome = outcome,
    start_values = NULL,
    max_iter = 10000
  )
  return(opt1)
}
