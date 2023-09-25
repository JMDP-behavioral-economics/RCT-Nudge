library(here)
library(R6)
library(tidyverse)
source(here("R/R6_BalanceTest.r"))

RCT <- R6Class("RCT", 
  public = list(
    data = NULL,
    initialize = function(data) self$data <- data,
    add_intervention = function(label, assignment) {
      new_intervention <- assignment # must be list
      names(new_intervention) <- label
      private$intervention <- append(private$intervention, new_intervention)
      print(private$intervention)
    },
    add_covariate = function(add) {
      private$covariate <- append(private$covariate, add)
      private$covariate <- private$covariate[!duplicated(private$covariate)]
      print(private$covariate)
    },
    add_outcome = function(add) {
      private$outcome <- append(private$endpoint, add) #add = list(colname = label)
      print(private$outcome)
    },
    reset_setup = function() {
      private$intervention <- list()
      private$covariate <- c()
      private$outcome <- list()
    },
    summary_experiment = function(cluster = NULL, se = "stata") {
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
    outcome = list()
  )
)