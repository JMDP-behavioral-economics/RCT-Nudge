library(here)
library(R6)

Lm <- R6::R6Class("Lm",
  public = list(
    data = NULL,
    initialize = function(data, covariate, se, cluster = NULL, fe = NULL) {
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

      private$mod <- lapply(rhs, function(m) reformulate(m, value))
      private$ctrl_arm <- levels(data$treat)[1]
      self$data <- data

      cat("Options for linear regression\n")
      cat("- Control arm:", private$ctrl_arm, "\n")
      cat("- Clustered standard error: FALSE\n")
      cat("  - Standard error type:", private$se_type "\n")
      car("Regression models\n")
      for (i in 1:length(private$model)) print(private$model[[i]])
    }
  ),
  private = list(
    ctrl_arm = "",
    model = list(),
    se_type = ""
  )
)