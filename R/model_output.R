#' @export
coef.hiper_glm <- function(object, ...) {
  # Returning coefficients from the object(can be changed accordingly)
  return(coef(object))
}

#' @export
vcov.hiper_glm <- function(object, ...) {
  # Returning var-cov matrix from the object(can be changed accordingly)
  return(vcov(object))
}

#' @export
print.hiper_glm <- function(x, ...) {
  # For example, printing relevant information about the model
  cat("High-Performance GLM Model\n")
  cat("Call: ", deparse(x$call), "\n")
  cat("Coefficients:\n")
  print(coef(x))
}
