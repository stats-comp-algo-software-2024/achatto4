




#' High-Performance GLM
#'
#' Fits a high-performance generalized linear model.
#'
#'
#' @param design A design matrix.
#' @param outcome The outcome variable.
#' @param model The type of model. Currently supports 'linear'.
#' @param option A list of options. Currently supports 'mle_solver' with values 'LS' and 'Logit' in 'BFGS' and 'Newton'.
#' @return An object of class 'hiper_glm'.
#' @examples n_obs <- 10; n_pred <- 5
#' data <- simulate_data(n_obs, n_pred, model = "linear", seed = 1)
#' design <- data$design; outcome <- data$outcome
#' hiper_glm(design, outcome, model = "linear")
#' hiper_glm(design, outcome, model = "linear", option = list(mle_solver = "BFGS"))
#' hiper_glm(design, outcome, model = 'logit', option = list(mle_solver = 'BFGS'))
#' hiper_glm(design, outcome, model = 'logit')

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

          output <-
            list(coefficients = find_mle_bfgs(design, outcome),
                 model = model)

        } else {
          stop("Option not correct. Only 'BFGS' option is supported for 'mle_solver'.")
        }
      } else {
        output <- list(coefficients = find_mle_pseudoinverse(design, outcome), model = model)

      }
    } else if (model == "logit") {
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


          output <-
            list(coefficients = find_mle_bfgs_log(design, outcome),
                 model = model)

        } else {
          stop("Option not correct. Only 'BFGS' option is supported for 'mle_solver'.")
        }
      } else {
        output <-
          list(coefficients = newton_algo(design, outcome),
               model = model)

      }

    } else{
      stop("Only 'Linear' and 'Logit' models are available.")
    }
    class(output) <- "hiper_glm"
    return(output)
  }
