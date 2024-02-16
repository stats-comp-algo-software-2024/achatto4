

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
