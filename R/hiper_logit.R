


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
          simulate_data(
            n_obs,
            n_pred,
            model = "linear",
            intercept = NULL,
            coef_true = NULL,
            design = NULL,
            seed = NULL
          )

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
    } else if (model == "logit"){
      if (!is.null(option) && "mle_solver" %in% names(option)) {
        if (option$mle_solver == "BFGS") {
          simulate_data(
            n_obs,
            n_pred,
            model = "logit",
            intercept = NULL,
            coef_true = NULL,
            design = NULL,
            seed = NULL,
            option = list(n_trial = 2)
          )

          opt <-
            optim(
              par = rep(0, ncol(design)),
              fn = log_likelihood_logistic,
              gr = gradient_log_likelihood_logistic,
              design = design,
              outcome = outcome
            )

          output <- list(coefficients = opt$par, model = model)
          class(output) <- "hiper_glm"
        } else {
          stop("Option not correct. Only 'BFGS' option is supported for 'mle_solver'.")
        }
      } else {
        opt1<- newton_logistic(design = design,
                               outcome = outcome, start_values = NULL, max_iter = 10000)
        output <- list(coefficients = opt1, model = model)
        class(output) <- "hiper_glm"
      }

    }

    return(output)
  }

