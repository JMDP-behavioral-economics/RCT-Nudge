library(here)
library(R6)
library(tidyverse)

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
  public = list(
    initialize = function(est) private$est <- est,
    get_est = function() private$est,
    print_msummary = function() private$reg_tab,
    msummary = function(title = "") {
      add_tab <- data.frame(rbind(c("Covariates", private$est$covs)))

      attr(add_tab, "position") <- 7

      private$reg_tab <- private$est %>%
        pull(fit) %>%
        modelsummary(
          title = title,
          estimate = "{or}",
          statistic = "[{lower.or}, {upper.or}]",
          coef_map = c(
            "treatB" = "Treatment B",
            "treatC" = "Treatment C",
            "treatD" = "Treatment D"
          ),
          stars = c("***" = .01, "**" = .05, "*" = .1),
          gof_omit = "R2|AIC|BIC|RMSE|Std|FE|se_type",
          align = paste(c("l", rep("c", nrow(private$est))), collapse = ""),
          add_rows = add_tab,
          fmt = fmt_sprintf("%.3f")
        )
      
      invisible(self)
    }
  ),
  private = list(
    est = NULL,
    reg_tab = NULL,
    label_structure = function(est) {
      label <- c(" ", as.character(est$outcome))
      intention_label <- str_detect(label, "intention")

      if (any(intention_label)) {
        label2 <- ifelse(intention_label, "Intention", " ")
        rle2 <- rle(label2)
      } else {
        label2 <- rle2 <- NULL
      }

      label1 <- str_remove(label, " intention")
      rle1 <- rle(label1)

      return(list(rle1 = rle1, rle2 = rle2))
    }
  )
)