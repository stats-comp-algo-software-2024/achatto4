#' @export
coef.hiper_glm <- function(object, ...) {
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
