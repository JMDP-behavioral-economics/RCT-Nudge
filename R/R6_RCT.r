library(here)
library(R6)
library(tidyverse)
source(here("R/R6_BalanceTest.r"))
source(here("R/R6_Lm.r"))
source(here("R/R6_Logit.r"))

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
    set_default_cluster = function(g) {
      private$cluster <- g
      print(g)
    },
    set_default_se_type = function(se) {
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
      if (se == "") stop("Specify se_type by set_default_se_type()")

      BalanceTest$new(
        self$data,
        private$covariate,
        private$intervention,
        se,
        cluster
      )
    },
    lm = function(outcome_id, se, cluster) {
      if (missing(se)) se <- private$se_type
      if (se == "") stop("Specify se_type by set_default_se_type()")

      use <- private$create_analysis_data()
      if (!missing(outcome_id)) use <- private$subset_by_outcome(use, outcome_id)

      if (missing(cluster)) cluster <- private$cluster
      if (is.null(cluster)) {
        Lm$new(use, private$covariate, se, private$fe)
      } else {
        LmCluster$new(use, private$covariate, se, cluster, private$fe)
      }
    },
    logit = function(outcome_id) {
      use <- private$create_analysis_data()
      if (!missing(outcome_id)) use <- private$subset_by_outcome(use, outcome_id)
      Logit$new(use, private$covariate, private$fe)
    }
  ),
  private = list(
    intervention = list(),
    covariate = c(),
    fe = NULL,
    outcome = list(),
    se_type = "",
    cluster = NULL,
    create_analysis_data = function() {
      exclude <- self$data %>%
        select(id, starts_with("exg_stop")) %>%
        rename(exg_stop_positive = exg_stop_intention) %>%
        mutate(exg_stop_negative = exg_stop_positive) %>%
        pivot_longer(
          -id,
          names_to = "outcome", values_to = "exclude",
          names_prefix = "exg_stop_"
        )

      self$data %>%
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
    },
    subset_by_outcome = function(data, outcome_id) {
      keep <- unname(unlist(private$outcome))[outcome_id]
      subset(data, outcome %in% keep)
    }
  )
)