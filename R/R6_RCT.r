library(here)
library(R6)
library(tidyverse)
source(here("R/R6_BalanceTest.r"))
source(here("R/R6_Lm.r"))
source(here("R/R6_Logit.r"))
source(here("R/R6_RCF.r"))
source(here("R/R6_Flow.r"))
source(here("R/R6_MultipleHypotheses.r"))

RCT <- R6Class("RCT",
  public = list(
    data = NULL,
    initialize = function(data) self$data <- data,
    add_intervention = function(assignment) {
      new_intervention <- assignment # must be list
      private$intervention <- append(private$intervention, new_intervention)
      print(private$intervention)
      invisible(self)
    },
    add_outcome = function(add) {
      private$outcome <- append(private$endpoint, add) #add = list(colname = label)
      print(private$outcome)
      invisible(self)
    },
    set_default_cluster = function(g) {
      private$cluster <- g
      print(g)
      invisible(self)
    },
    set_default_se_type = function(se) {
      private$se_type <- se
      print(private$se_type)
      invisible(self)
    },
    reset_setup = function() {
      private$intervention <- list()
      private$outcome <- list()
      private$se_type <- ""
      private$cluster <- NULL
      invisible(self)
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
    smd_balance = function() {
      SmdBalanceTest$new(
        self$data,
        private$covariate
      )
    },
    lm_effect = function( outcome,
                          sample_drop = TRUE,
                          demean_covariate = TRUE,
                          se,
                          cluster,
                          hide_message = TRUE)
    {
      if (missing(se)) se <- private$se_type
      if (se == "") stop("Specify se_type by set_default_se_type()")

      use <- private$create_analysis_data(sample_drop)
      if (!missing(outcome)) {
        outcome_id <- which(names(private$outcome) %in% outcome)
        use <- private$subset_by_outcome(use, outcome_id)
      }

      if (missing(cluster)) cluster <- private$cluster

      Lm$new(use, demean_covariate, se, cluster, hide_message)
    },
    logit_effect = function(outcome,
                            sample_drop = TRUE,
                            demean_covariate = TRUE,
                            cluster,
                            hide_message = TRUE)
    {
      use <- private$create_analysis_data(sample_drop)
      if (!missing(outcome)) {
        outcome_id <- which(names(private$outcome) == outcome)
        use <- private$subset_by_outcome(use, outcome_id)
      }

      if (missing(cluster)) cluster <- private$cluster

      Logit$new(use, demean_covariate, cluster, hide_message)
    },
    rcf = function(outcome, sample_drop = TRUE) {
      if (length(private$covariate) == 0) stop("Specify covariate by add_covariate()")
      if (length(outcome) > 1) stop("Specify only one outcome")

      dt <- private$create_analysis_data(sample_drop)
      outcome_id <- which(names(private$outcome) == outcome)
      use <- private$subset_by_outcome(dt, outcome_id)

      model <- reformulate(c("treat", private$covariate), "value")
      mat <- model.frame(model, data = use)

      Y <- mat[, 1, drop = TRUE]
      D <- mat[, 2, drop = TRUE]
      X <- as.matrix(mat[, private$covariate])

      RCF$new(Y, D, X, private$covariate)
    },
    flow = function(outcome,
                    sample_drop = TRUE,
                    se,
                    cluster)
    {
      if (missing(se)) se <- private$se_type
      if (se == "") stop("Specify se_type by set_default_se_type()")
      if (length(outcome) > 1) stop("Specify only one outcome")

      dt <- private$create_analysis_data(sample_drop)
      outcome_id <- which(names(private$outcome) == outcome)
      use <- private$subset_by_outcome(dt, outcome_id)

      if (missing(cluster)) cluster <- private$cluster
      Flow$new(use, se, cluster)
    },
    lm_decompose_effect = function( outcome,
                                    demean_covariate = TRUE,
                                    se,
                                    cluster,
                                    hide_message = TRUE)
    {
      if (missing(se)) se <- private$se_type
      if (se == "") stop("Specify se_type by set_default_se_type()")

      previous_endpoint <- names(private$outcome)[which(names(private$outcome) == outcome) - 1]

      dt <- self$data
      dt$exg_stop <- dt[, paste0("exg_stop_", outcome), drop = TRUE]
      dt$endpoint <- dt[, outcome, drop = TRUE]
      dt$before_endpoint <- dt[, previous_endpoint, drop = TRUE]
      # dt$exg_attr <- ifelse(dt$before_endpoint == 1 & dt$endpoint == 0 & dt$exg_stop == 1, 0, 1)
      dt$end_attr <- ifelse(dt$before_endpoint == 1 & dt$endpoint == 0 & dt$exg_stop == 0, 0, 1)

      use <- dt %>%
        select(-exg_stop, -endpoint) %>%
        pivot_longer(before_endpoint:end_attr, names_to = "outcome") %>%
        mutate(
          outcome = factor(
            outcome,
            levels = c("before_endpoint", "end_attr"),
            # levels = c("before_endpoint", "end_attr", "exg_attr"),
            labels = c(
              private$outcome[[previous_endpoint]],
              "No endogenous dropout"
              # "No exogenous dropout"
            )
          )
        )

      if (missing(cluster)) cluster <- private$cluster

      Lm$new(use, demean_covariate, se, cluster, hide_message)
    },
    multiple_hypotheses_adjust = function(outcome,
                                          group_by_gender = FALSE,
                                          group_by_gender_age = FALSE,
                                          age_cut = 30)
    {
      MultipleHypothesis$new(
        self$data,
        outcome,
        group_by_gender,
        group_by_gender_age,
        age_cut
      )
    }
  ),
  private = list(
    intervention = list(),
    outcome = list(),
    se_type = "",
    cluster = NULL,
    create_analysis_data = function(drop) {
      exclude <- self$data %>%
        select(id, starts_with("exg_stop")) %>%
        rename(exg_stop_positive = exg_stop_intention) %>%
        mutate(exg_stop_negative = exg_stop_positive) %>%
        pivot_longer(
          -id,
          names_to = "outcome", values_to = "exclude",
          names_prefix = "exg_stop_"
        )

      data <- self$data %>%
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
        mutate(outcome = factor(
          outcome,
          levels = names(private$outcome),
          labels = unname(unlist(private$outcome))
        ))

      if (drop) {
        subset(data, exclude == 0)
      } else {
        data
      }

    },
    subset_by_outcome = function(data, outcome_id) {
      keep <- unname(unlist(private$outcome))[outcome_id]
      subset(data, outcome %in% keep)
    }
  )
)