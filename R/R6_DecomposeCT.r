library(R6)
source("R/misc.r")

DecomposeLm <- R6::R6Class("DecomposeLm",
  public = list(
    initialize = function(data, list_outcome, endpoint, se) {
      private$list_outcome <- list_outcome
      focus_stage <- c(names(list_outcome)[which(names(list_outcome) == endpoint) - 1], endpoint)

      dt <- data
      dt$exg_stop <- dt[, paste0("exg_stop_", focus_stage[2]), drop = TRUE]
      dt$endpoint <- dt[, focus_stage[2], drop = TRUE]
      dt$before_endpoint <- dt[, focus_stage[1], drop = TRUE]
      dt$exg_attr <- ifelse(dt$before_endpoint == 1 & dt$endpoint == 0 & dt$exg_stop == 1, 1, 0)
      dt$end_attr <- ifelse(dt$before_endpoint == 1 & dt$endpoint == 0 & dt$exg_stop == 0, 1, 0)

      private$data <- dt %>%
        pivot_longer(before_endpoint:end_attr, names_to = "outcome")

      private$focus_stage <- focus_stage
      private$se <- se
    },
    get_data = function() private$data,
    lm = function(scale = 1) {
      invisible(self)
    }
  ),
  private = list(
    data = NULL,
    list_outcome = NULL,
    focus_stage = NULL,
    se = NULL
  )
)