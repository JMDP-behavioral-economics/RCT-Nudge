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

my_theme_classic <- function(size = 15) {
    theme_classic(base_size = size) +
    theme(
      strip.background = element_blank(),
      strip.text = element_text(size = size + 1, hjust = 0)
    )
}