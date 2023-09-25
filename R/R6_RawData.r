library(here)
library(R6)
library(tidyverse)
library(rlang)
library(estimatr)
source(here("R/R6_RCT.r"))

RawData <- R6::R6Class("RawData",
  public = list(
    data = NULL,
    initialize = function(path, treat_vars, treat_levels, treat_labels = NULL) {
      use <- read_csv(here(data_root, "shaped.csv"), locale = locale(encoding = "cp932"))
      use <- use %>%
        mutate(
          age_less30 = if_else(age < 30, 1, 0),
          age_demean = age - mean(use$age)
        )
      
      if (is.null(treat_labels)) treat_labels <- treat_levels

      use[, treat_vars] <- factor(
        use[, treat_vars, drop = TRUE],
        levels = treat_levels,
        labels = treat_labels
      )

      private$treat <- treat_vars
      self$data <- use
    },
    add_cond_study_sample = function(condition) {
      private$filter_cond <- append(private$filter_cond, condition)
      invisible(private$filter_cond)
    },
    balance_attrition = function(cluster = NULL, se = "stata") {
      x <- self$data[, private$treat, drop = TRUE]
      args <- list(se_type = se)
      if (!is.null(cluster)) {
        g <- self$data[, cluster, drop = TRUE]
        args <- append(args, list(cluster = g))
      }

      reg <- private$filter_cond %>%
        map(function(cond) {
          y <- !eval_tidy(parse_expr(cond), self$data)
          args <- append(args, list(formula = y ~ x))
          do.call(lm_robust, args)
        })
      
      ftest <- reg %>%
        map(function(r) {
          f <- summary(r)$fstatistic[1]
          numdf <- summary(r)$fstatistic[2]
          dendf <- summary(r)$fstatistic[3]
          p <- pf(f, numdf, dendf, lower.tail = FALSE)

          sprintf("F-test, p-value = $%1.3f$", p)
        })
      
      names(ftest) <- private$filter_cond
      ftest
    },
    RCT = function() {
      use <- self$data
      cond <- private$filter_cond

      if (length(cond) > 0) {
        for (i in 1:length(cond)) {
          boolean <- eval_tidy(parse_expr(cond[[i]]), use)
          use <- use[boolean, , drop = FALSE]
        }
      }

      use <- use %>%
        rename(treat = private$treat)

      RCT$new(use)
    }
  ),
  private = list(
    treat = "",
    filter_cond = list()
  )
)