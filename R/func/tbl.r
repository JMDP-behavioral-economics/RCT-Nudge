tbl <- function(x) UseMethod("tbl")

tbl.summary_experiment <- function(x) {
  x %>%
    as_grouped_data("type") %>%
    as_flextable(hide_grouplabel = TRUE) %>%
    set_header_labels(vars = "", p = "F-test, p-value") %>%
    add_header_row(values = c("", "Experimental Arms", ""), colwidths = c(1, 4, 1)) %>%
    add_footer_lines(paste(
      "Notes: Balance test regresses a covariate on treatment dummies",
      "and test a null hypothesis that all coefficients are zero.",
      "We use the clustered standard error with the CR2 adjustment for inference",
      "(cluster unit is an experimental week)."
    )) %>%
    align(j = -1, align = "center", part = "all") %>%
    padding(j = 1:6, padding.top = 5, padding.bottom = 5, part = "all") %>%
    padding(2:4, padding.left = 10) %>%
    padding(6, padding.left = 10) %>%
    padding(8:13, padding.left = 10) %>%
    width(j = 1, 2) %>%
    fontsize(size = 9, part = "all") %>%
    ft_theme()
}

tbl.lm_all_stock <- function(x) {
  x %>%
    add_header_row(
      values = c("", "Reply", "Positive", "Negative"),
      colwidths = c(1, 2, 2, 2)
    ) %>%
    add_header_row(
      values = c("", "Intention"),
      colwidths = c(3, 4)
    ) %>%
    align(j = -1, align = "center", part = "all") %>%
    add_footer_lines(paste(
      "Notes: * p < 0.1, ** p < 0.05, *** p < 0.01.",
      "The clustered standard errors with the CR2 adjustment",
      "are reported in parenetheses (cluster unit is an experimental week).",
      "Covariates are gender, squared polynomial of (demeaned) age,",
      "number of past coordinations,",
      "number of hospitals per 10 square kilometers,",
      "number of hospitals with PBSC collection per 10 square kilometers,",
      "number of hospitals with BM collection per 10 square kilometers,",
      "month dummies, and week dummies."
    )) %>%
    width(j = 1, 1) %>%
    fontsize(size = 9, part = "all") %>%
    ft_theme()
}