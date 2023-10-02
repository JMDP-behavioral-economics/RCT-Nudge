library(R6)

Flow <- R6::R6Class("Flow",
  public = list(
    data = NULL,
    initialize = function(reply_data) self$data <- reply_data
  ),
  private = list()
)