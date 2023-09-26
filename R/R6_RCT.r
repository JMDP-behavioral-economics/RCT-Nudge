library(here)
library(R6)
library(tidyverse)
source(here("R/R6_BalanceTest.r"))
source(here("R/R6_Lm.r"))

RCT <- R6Class("RCT", 
  public = list(
    data = NULL,
    initialize = function(data) self$data <- data,
    add_intervention = function(assignment) {
      new_intervention <- assignment # must be list
      private$intervention <- append(private$intervention, new_intervention)
      print(private$intervention)
    },
    add_covariate = function(add) {
      private$covariate <- append(private$covariate, add)
      private$covariate <- private$covariate[!duplicated(private$covariate)]
      print(private$covariate)
    },
    add_fixed_effect = function(add) {
      private$fe <- append(private$fe, add)
      private$fe <- private$fe[!duplicated(private$fe)]
      print(private$fe)
    },
    add_outcome = function(add) {
      private$outcome <- append(private$endpoint, add) #add = list(colname = label)
      print(private$outcome)
    },
    se_cluster = function(g) {
      private$cluster <- g
      print(g)
    },
    set_default_se = function(se) {
      private$se_type <- se
      print(private$se_type)
    },
    reset_setup = function() {
      private$intervention <- list()
      private$covariate <- c()
      private$outcome <- list()
      private$se_type <- ""
      private$cluster <- NULL
    },
    summary_experiment = function(cluster, se) {
      if (missing(cluster)) cluster <- private$cluster
      if (missing(se)) se <- private$se_type
      if (se == "") stop("Specify se_type by set_default_se()")

      BalanceTest$new(
        self$data,
        private$covariate,
        private$intervention,
        se,
        cluster
      )
    },
    lm = function(outcome_id, se) {
      if (missing(se)) se <- private$se_type
      if (se == "") stop("Specify se_type by set_default_se()")

      exclude <- self$data %>%
        select(id, starts_with("exg_stop")) %>%
        rename(exg_stop_positive = exg_stop_intention) %>%
        mutate(exg_stop_negative = exg_stop_positive) %>%
        pivot_longer(
          -id,
          names_to = "outcome", values_to = "exclude",
          names_prefix = "exg_stop_"
        )
      
      use <- self$data %>%
        select(
          reply,
          positive,
          negative,
          test,
          candidate,
          consent,
          donate,
          everything(),
          -starts_with("exg_stop")
        ) %>%
        pivot_longer(reply:donate, names_to = "outcome") %>%
        dplyr::left_join(exclude, by = c("id", "outcome")) %>%
        dplyr::filter(exclude == 0) %>%
        mutate(outcome = factor(
          outcome,
          levels = names(private$outcome),
          labels = unname(unlist(private$outcome))
        ))
      
      if (!missing(outcome_id)) {
        keep <- unname(unlist(private$outcome))[outcome_id]
        use <- subset(use, outcome %in% keep)
      }

      Lm$new(use, private$covariate, se, private$fe)
    }
  ),
  private = list(
    intervention = list(),
    covariate = c(),
    fe = c(),
    outcome = list(),
    se_type = "",
    cluster = NULL
  )
)