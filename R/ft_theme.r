library(tidyverse)
library(flextable)
library(officer)

ft_theme <- function(ft) {
  border_remove(ft) %>%
    hline_bottom(part = "body", border = fp_border()) %>%
    hline_bottom(part = "header", border = fp_border()) %>%
    hline_top(part = "header", border = fp_border()) %>%
    hline_top(part = "footer", border = fp_border())
}