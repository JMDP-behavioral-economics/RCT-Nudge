library(R6)
library(tidyverse)

StartAnalysis <- R6Class("StartAnalysis", list(
  data = NULL,
  label = list(
    reply = "Reply",
    positive = "Positive intention",
    negative = "Negative intention",
    test = "CT",
    candidate = "Candidate",
    consent = "Consent",
    donate = "Donation"
  ),
  list_intervention = list(),
  initialize = function(data) {
    self$data <- data %>%
      mutate(
        treat = factor(treat, levels = LETTERS[1:4]),
        age_less30 = if_else(age < 30, 1, 0),
        age_demean = age - mean(data$age)
      )
  },
  print = function() View(self$data),
  intervention = function(label, assignment) {
    self$list_intervention <- append(self$list_intervention, assignment)
    i <- length(self$list_intervention)
    names(self$list_intervention)[i] <- label
  },
  summary_experiment = function() {
    panelA <- reduce(list_intervention, bind_rows),
    panelA <- bind_cols(term = names(list_intervention), panelA)
  }
))
