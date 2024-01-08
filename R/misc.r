# //NOTE template of flextable output
library(flextable)
library(officer)

ft_theme <- function(ft) {
  border_remove(ft) %>%
    hline_bottom(part = "body", border = fp_border()) %>%
    hline_bottom(part = "header", border = fp_border()) %>%
    hline_top(part = "header", border = fp_border()) %>%
    hline_top(part = "footer", border = fp_border())
}

# //NOTE template of ggplot2
library(tidyverse)

my_theme_classic <- function(size = 15, strip_hjust = 0) {
    theme_classic(base_size = size) +
    theme(
      strip.background = element_blank(),
      strip.text = element_text(size = size + 1, hjust = strip_hjust)
    )
}

# //NOTE Tidy custom for glm class
tidy_custom.glm <- function(x, ...) {
  tbl <- broom::tidy(x) %>%
    mutate(
      or = exp(estimate),
      lower.or = exp(estimate - 1.96 * std.error),
      upper.or = exp(estimate + 1.96 * std.error)
    )

  tbl
}

# * Standard errors
se <- function(x, na.rm = FALSE) {
  if (na.rm) x <- na.omit(x)
  sqrt(var(x) / length(x))
}