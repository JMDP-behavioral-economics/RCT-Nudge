library(here)
library(R6)

Logit <- R6::R6Class("Logit",
  public = list(
    data = NULL,
    initialize = function(data, covariate, fe) {
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

      cat("\n")
      cat("Options for binary regression (Logit)\n")
      cat("- Control arm:", private$ctrl_arm, "\n")
      cat("Regression models\n")
      for (i in 1:length(private$model)) print(private$model[[i]])
      cat("\n")
    },
    fit_all = function() {
      est <- self$data %>%
        group_by(outcome) %>%
        nest() %>%
        mutate(
          fit1 = map(data, ~ glm(private$model$unctrl, data = ., family = binomial())),
          fit2 = map(data, ~ glm(private$model$ctrl, data = ., family = binomial()))
        ) %>%
        ungroup() %>%
        pivot_longer(
          fit1:fit2,
          names_prefix = "fit",
          names_to = "model",
          values_to = "fit"
        ) %>%
        select(-data) %>%
        rename(covs = model) %>%
        mutate(covs = if_else(covs == "2", "X", ""))

      LogitAll$new(est)
    }
  ),
  private = list(
    ctrl_arm = "",
    model = list()
  )
)

LogitAll <- R6::R6Class("LogitAll",
  public = list(),
  private = list()
)