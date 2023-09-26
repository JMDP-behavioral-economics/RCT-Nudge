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