


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
#' @examples n_obs <- 10; n_pred <- 5
#' data <- simulate_data(n_obs, n_pred, model = "linear", seed = 1)
#' design <- data$design; outcome <- data$outcome
#' hiper_glm(design, outcome, model = "linear")
#' hiper_glm(design, outcome, model = "linear", option = list(mle_solver = "BFGS"))
#'

hiper_glm <-
  function(design,
           outcome,
           model = "linear",
           option = list()) {
    if (model == "linear") {
      if (!is.null(option) && "mle_solver" %in% names(option)) {
        if (option$mle_solver == "BFGS") {
          simulate_data <- function(n_obs,
                                    n_pred,
                                    model = "linear",
                                    intercept = NULL,
                                    coef_true = NULL,
                                    design = NULL,
                                    seed = NULL,
                                    signal_to_noise = 5) {
            if (!is.null(seed)) {
              set.seed(seed)
            }
            if (is.null(coef_true)) {
              coef_true <- rnorm(n_pred, sd = 1 / sqrt(n_pred))
            }
            if (is.null(design)) {
              design <- matrix(rnorm(n_obs * n_pred),
                               nrow = n_obs,
                               ncol = n_pred)
            }
            if (!is.null(intercept)) {
              if (!is.numeric(intercept)) {
                stop("The intercept argument must be numeric.")
              }
              coef_true <- c(intercept, coef_true)
              design <- cbind(rep(1, n_obs), design)
            }
            expected_mean <- as.vector(design %*% coef_true)
            noise_magnitude <-
              sqrt(var(expected_mean) / signal_to_noise ^ 2)
            noise <- noise_magnitude * rnorm(n_obs)
            outcome <- expected_mean + noise
            return(list(
              design = design,
              outcome = outcome,
              coef_true = coef_true
            ))
          }

          opt <-
            optim(
              par = rep(0, ncol(design)),
              fn = neg_log_likelihood,
              gr = neg_gradient,
              design = design,
              outcome = outcome
            )

          output <- list(coefficients = opt$par, model = model)
          class(output) <- "hiper_glm"
        } else {
          stop("Option not correct. Only 'BFGS' option is supported for 'mle_solver'.")
        }
      } else {
        beta_hat <- solve(t(design) %*% design, t(design) %*% outcome)
        output <- list(coefficients = beta_hat, model = model)
        class(output) <- "hiper_glm"
      }
    } else {
      stop("Model type not supported. Only 'linear' model is currently supported.")
    }

    return(output)
  }
