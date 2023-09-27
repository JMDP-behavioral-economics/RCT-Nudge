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
    },
    subset_boxplot = function() {
      dt <- cbind(private$tau, private$X) %>%
        data.frame() %>%
        pivot_longer(
          all_of(colnames(private$tau)),
          names_to = c(".value", "treat"),
          names_sep = "_"
        ) %>%
        mutate(
          male = factor(male, labels = c("Females", "Males")),
          treat = factor(treat)
        )
      
      dt %>%
        ggplot(aes(x = age, y = effect)) +
        geom_hline(aes(yintercept = 0), linetype = 2) +
        geom_boxplot(aes(group = age)) +
        stat_smooth(se = FALSE, color = "blue") +
        facet_grid(treat ~ male) +
        labs(x = "Age", y = "Predicted treatment effect") +
        my_theme_classic(strip_hjust = 0.5)
    }
  ),
  private = list(
    X = NULL,
    tau = NULL,
    rcf = NULL
  )
)