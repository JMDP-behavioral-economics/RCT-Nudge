tidy_custom.lm_robust <- function(x, ...) broom::tidy(x)


tidy_custom.glm <- function(x, ...) {
  tbl <- tidy(x) %>%
    mutate(
      or = exp(estimate),
      lower.or = exp(estimate - 1.96 * std.error),
      upper.or = exp(estimate + 1.96 * std.error)
    )

  tbl
}