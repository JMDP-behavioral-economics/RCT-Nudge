library(here)
library(R6)
library(grf)
library(rlang)

RCF <- R6::R6Class("RCF",
  public = list(
    initialize = function(Y, D, X) {
      rcf <- multi_arm_causal_forest(X, Y, D)
      tau <- predict(rcf, X)$predictions[, , 1]

      ctrl <- levels(D)[1]
      lab <- str_remove(colnames(tau), paste(" -", ctrl))
      lab <- paste0("effect_", lab)
      colnames(tau) <- lab

      private$X <- X
      private$tau <- tau
      private$rcf <- rcf
      private$ctrl_arm <- ctrl
    },
    get_rcf = function() private$rcf,
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
    },
    cate = function(...) {
      cond <- private$list_subset(...)
      pattern <- private$bool_pattern(length(cond))
      combine_cond <- private$combine_subset(cond, pattern)

      cate <- seq(length(combine_cond)) %>%
        map(function(i) {
          average_treatment_effect(a, subset = combine_cond[[i]]) %>%
            mutate(
              i = i,
              z = abs(estimate / std.err),
              p = 2 * pnorm(z, lower.tail = FALSE),
              treat = str_remove(contrast, " - A")
            ) %>%
            select(-contrast, -outcome)
        }) %>%
        map(function(tbl) {
          col_label <- colnames(pattern)
          for (i in col_label) {
            tbl[, col_label] <- pattern[tbl$i, col_label, drop = TRUE]
          }
          tbl
        }) %>%
        reduce(bind_rows) %>%
        select(i, starts_with("cond"), treat, everything())
      
      RCFCate$new(cate)
    }
  ),
  private = list(
    X = NULL,
    tau = NULL,
    rcf = NULL,
    ctrl_arm = NULL,
    list_subset = function(...) {
      cond <- enquos(...)
      sub <- list()
      for (i in 1:length(cond)) {
        bool <- eval_tidy(cond[[i]], data.frame(private$X))
        sub <- append(sub, list(bool))
      }
      sub
    },
    bool_pattern = function(length_cond) {
      pattern <- data.frame(cond1 = c(TRUE, FALSE))
      for (i in seq(length_cond - 1)) {
        col_lab <- paste0("cond", i + 1)
        pattern[, col_lab] <- c(TRUE, FALSE)
      }
      tibble(expand.grid(pattern))
    },
    combine_subset = function(list_subset, bool_pattern) {
      lapply(
        seq(nrow(bool_pattern)),
        function(i) {
          bool <- rep(TRUE, nrow(private$X))
          for (j in seq(length(list_subset))) {
            bool <- bool * c(list_subset[[j]] == bool_pattern[i, j, drop = TRUE])
          }
          as.logical(bool)
        }
      )
    }
  )
)

RCFCate <- R6::R6Class("RCFCate",
  public = list(
    initialize = function(cate) private$cate <- cate
  ),
  private = list(
    cate = NULL
  )
)