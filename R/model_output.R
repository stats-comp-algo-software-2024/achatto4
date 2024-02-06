#' @export
coef.hiper_glm <- function(object, ...) {
  warning("The 'coef.hiper_glm' function is yet to be implemented.")
}

#' @export
vcov.hiper_glm <- function(object, ...) {
  warning("The 'vcov.hiper_glm' function is yet to be implemented.")
}

#' @export
print.hiper_glm <- function(x, ...) {
  warning("The 'print.hiper_glm' function is yet to be implemented.")
}

#stats::optim
usethis::use_testthat()

test_check("hiperglm")

#actual test- helper.R, test-mle-finder.R

