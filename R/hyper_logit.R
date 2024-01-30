#' High-Performance GLM
#'
#' Fits a high-performance generalized linear model.
#'
#' @param design A design matrix.
#' @param outcome The outcome variable.
#' @return An object of class 'hiper_glm'.
#' @export
hiper_glm <- function(design, outcome) {
  # Your implementation of high-performance GLM here
  # For example, using glm function for simplicity
  model <- glm(outcome ~ design, family = "binomial")
  class(model) <- "hiper_glm"
  return(model)
}
