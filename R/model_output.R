#' @export
coef.hiper_glm <- function(object, ...) {
  # Your implementation of coef method
  # For example, returning coefficients from the object
  return(coef(object))
}

#' @export
vcov.hiper_glm <- function(object, ...) {
  # Your implementation of vcov method
  # For example, returning the variance-covariance matrix from the object
  return(vcov(object))
}

#' @export
print.hiper_glm <- function(x, ...) {
  # Your implementation of print method
  # For example, printing relevant information about the model
  cat("High-Performance GLM Model\n")
  cat("Call: ", deparse(x$call), "\n")
  cat("Coefficients:\n")
  print(coef(x))
}
