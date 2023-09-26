library(here)
library(R6)
library(tidyverse)
library(estimatr)

Lm <- R6::R6Class("Lm",
  public = list(
    data = NULL,
    initialize = function(data, covariate, se, fe = NULL) {
      ctrl <- levels(data$treat)[1]

      rhs <- list(
        unctrl = "treat",
        ctrl = c("treat", covariate)
      )

      if (!is.null(fe)) {
        rhs$ctrl <- append(
          rhs$ctrl,
          sapply(fe, function(x) paste0("factor(", x, ")"))
        )
      }

      private$model <- lapply(rhs, function(m) reformulate(m, "value"))
      private$ctrl_arm <- levels(data$treat)[1]
      self$data <- data
      private$se_type <- se

      cat("Options for linear regression\n")
      cat("- Control arm:", private$ctrl_arm, "\n")
      cat("- Clustered standard error: FALSE\n")
      cat("  - Standard error type:", private$se_type, "\n")
      cat("Regression models\n")
      for (i in 1:length(private$model)) print(private$model[[i]])
    },
    fit_all = function() {
      est <- self$data %>%
        group_by(outcome) %>%
        nest() %>%
        mutate(
          fit1 = private$call_lm(data, private$model$unctrl, private$se_type),
          fit2 = private$call_lm(data, private$model$ctrl, private$se_type),
          avg = map_chr(
            data,
            ~ with(
              subset(., treat == private$ctrl_arm),
              sprintf("%1.4f", mean(value))
            )
          )
        ) %>%
        pivot_longer(
          fit1:fit2,
          names_prefix = "fit",
          names_to = "model",
          values_to = "fit"
        ) %>%
        select(-data)

      LmAll$new(est)
    }
  ),
  private = list(
    ctrl_arm = "",
    model = list(),
    se_type = "",
    call_lm = function(data, model, se) {
      map(data, ~ lm_robust(model, data = ., se_type = se))
    }
  )
)

LmAll <- R6::R6Class("LmAll",
  public = list(
    initialize = function(est) private$est <- est,
    get_est = function() private$est
  ),
  private = list(
    est = NULL
  )
)