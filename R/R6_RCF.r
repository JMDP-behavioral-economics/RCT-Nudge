library(here)
library(R6)
library(grf)

RCF <- R6::R6Class("RCF",
  public = list(
    initialize = function(Y, D, X) {
      rcf <- multi_arm_causal_forest(X, Y, D)
      tau <- predict(rcf, X)$predictions[, , 1]

      lab <- str_remove(colnames(tau), paste(" -", levels(D)[1]))
      lab <- paste0("effect_", lab)
      colnames(tau) <- lab

      private$X <- X
      private$tau <- tau
      private$rcf <- rcf
    }
  ),
  private = list(
    X = NULL,
    tau = NULL,
    rcf = NULL
  )
)