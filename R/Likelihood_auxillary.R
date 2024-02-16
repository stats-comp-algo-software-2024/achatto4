
# we define the function neg_log_likelihood which is the negative of the log-likelihood.

neg_log_likelihood <-
  function(beta, design, outcome , noise_var = 1) {
    X <- as.matrix(design)
    y <- as.vector(outcome)
    mu <- X %*% beta
    n <- length(y)
    0.5 * (sum((y - mu) ^ 2) / noise_var)
  }

# we define the function neg_gradient which is the negative of the gradient.
neg_gradient <- function(beta, design, outcome, noise_var = 1) {
  X <- as.matrix(design)
  y <- as.vector(outcome)
  mu <- X %*% beta
  grad <- -t(X) %*% (y - mu) / noise_var
  return(grad)
}
