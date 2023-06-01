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

tbl.lm_all_coordination <- function(x) {
  x %>%
    add_header_row(
      values = c("", "CT", "Candidate", "Consent", "Donation"),
      colwidths = c(1, 2, 2, 2, 2)
    ) %>%
    align(j = -1, align = "center", part = "all") %>%
    add_footer_lines(paste(
      "Notes: * p < 0.1, ** p < 0.05, *** p < 0.01.",
      "The clustered standard errors with the CR2 adjustment",
      "are reported in parenetheses (cluster unit is experimental weeks).",
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

tbl.cf_cate <- function(x) {
  x %>%
    as_grouped_data("age") %>%
    as_flextable(hide_grouplabel = TRUE) %>%
    set_header_labels(
      contrast = "",
      estimate_Females = "Estimate",
      std.err_Females = "S.E.",
      p_Females = "P-value",
      estimate_Males = "Estimate",
      std.err_Males = "S.E.",
      p_Males = "P-value"
    ) %>%
    add_header_row(values = c("", "Females", "Males"), colwidths = c(1, 3, 3)) %>%
    align(j = -1, align = "center", part = "all") %>%
    colformat_double(j = -1, digits = 3) %>%
    padding(j = 1:7, padding.top = 5, padding.bottom = 5, part = "all") %>%
    padding(2:4, padding.left = 10) %>%
    padding(6:8, padding.left = 10) %>%
    add_footer_lines(paste(
      "Notes: See Athey and Wager (2019) for",
      "estimation method of conditional average treatment effect (CATE).",
      "Since these estimates are asymptotically normal,",
      "we calculate z-score under the null hypothesis that CATE is zero,",
      "and obtain p-value."
    )) %>%
    fontsize(size = 9, part = "all") %>%
    ft_theme()
}

tbl.cf_corr_effect <- function(x) {
  tbl_tau_corr <- x %>%
    setNames(paste0("(", seq_len(length(.)), ")")) %>%
    modelsummary(
      coef_map = c(
        "(Intercept)" = "Constant",
        "I(B + C)" = "B + C",
        "B" = "B",
        "C" = "C"
      ),
      statistic = c("({std.error})", "conf.int"),
      stars = c("***" = .01, "**" = .05, "*" = .1),
      fmt = 4,
      gof_omit = "R2|AIC|BIC|Log|Std|FE|se_type|RMSE"
    )

  tbl_tau_corr %>%
    add_header_row(
      values = c("", rep(c("Age<30", "30\u2264Age", "Age<30", "30\u2264Age"), 2))
    ) %>%
    add_header_row(
      values = c("", rep(c("Females", "Males"), 2)),
      colwidths = c(1, 2, 2, 2, 2)
    ) %>%
    add_header_row(
      values = c("", "Treatment effect D"),
      colwidths = c(1, 8)
    ) %>%
    align(j = -1, align = "center", part = "all") %>%
    add_footer_lines(paste(
      "Notes: * p < 0.1, ** p < 0.05, *** p < 0.01.",
      "Robust standard errors are reported in parentheses.",
      "Square brackets show 95 percent confidential intervals."
    )) %>%
    width(j = 1, 1) %>%
    fontsize(size = 9, part = "all") %>%
    ft_theme()
}

tbl.logit_all_stock <- function(obj) {
  obj %>%
    flextable::add_header_row(
      values = c("", "Reply", "Positive", "Negative"),
      colwidths = c(1, 2, 2, 2)
    ) %>%
    flextable::add_header_row(
      values = c("", "Intention"),
      colwidths = c(3, 4)
    ) %>%
    add_footer_lines(paste(
      "Notes: We show odds ratios and associated 95 percent confidential intervals.",
      "Covariates are gender, squared polynomial of (demeaned) age,",
      "number of past coordinations,",
      "number of hospitals per 10 square kilometers,",
      "number of hospitals with PBSC collection per 10 square kilometers,",
      "number of hospitals with BM collection per 10 square kilometers,",
      "month dummies, and week dummies."
    )) %>%
    align(j = -1, align = "center", part = "all") %>%
    width(j = 1, 1) %>%
    fontsize(size = 9, part = "all") %>%
    ft_theme()
}

tbl.logit_all_coordination <- function(obj) {
  obj %>%
    flextable::add_header_row(
      values = c("", "CT", "Candidate", "Consent", "Donation"),
      colwidths = c(1, 2, 2, 2, 2)
    ) %>%
    align(j = -1, align = "center", part = "all") %>%
    width(j = 1, 1) %>%
    add_footer_lines(paste(
      "Notes: We show odds ratios and associated 95 percent confidential intervals.",
      "Covariates are gender, squared polynomial of (demeaned) age,",
      "number of past coordinations,",
      "number of hospitals per 10 square kilometers,",
      "number of hospitals with PBSC collection per 10 square kilometers,",
      "number of hospitals with BM collection per 10 square kilometers,",
      "month dummies, and week dummies."
    )) %>%
    fontsize(size = 9, part = "all") %>%
    ft_theme()
}

tbl.wildbs_subset_stock <- function(obj) {
  obj$table %>%
    align(j = -1, align = "center", part = "all") %>%
    add_header_row(
      values = c("", "Reply", "Positive", "Negative"),
      colwidths = c(1, 2, 2, 2)
    ) %>%
    add_header_row(
      values = c("", "Intention"),
      colwidths = c(3, 4)
    ) %>%
    add_footer_lines(paste(
      "Notes: * p < 0.1, ** p < 0.05, *** p < 0.01.",
      "The clustered standard errors with the CR2 adjustment",
      "are reported in parenetheses (cluster unit is experimental weeks).",
      "Covariates are number of past coordinations,",
      "number of hospitals per 10 square kilometers,",
      "number of hospitals with PBSC collection per 10 square kilometers,",
      "number of hospitals with BM collection per 10 square kilometers,",
      "month dummies, and week dummies.",
      "Bootstrap p-value is based on the wild cluster bootstrap-t with a null hypothesis imposed,",
      "which is proposed by Cameron et al. (2008)."
    )) %>%
    width(j = 1, 1) %>%
    padding(10:(10 + (obj$check - 1)), padding.left = 10) %>%
    fontsize(size = 9, part = "all") %>%
    ft_theme()
}

tbl.wildbs_subset_coordination <- function(obj) {
  obj$table %>%
    align(j = -1, align = "center", part = "all") %>%
    add_header_row(
      values = c("", "CT", "Candidate", "Consent", "Donation"),
      colwidths = c(1, 2, 2, 2, 2)
    ) %>%
    width(j = 1, 1) %>%
    add_footer_lines(paste(
      "Notes: * p < 0.1, ** p < 0.05, *** p < 0.01.",
      "The clustered standard errors with the CR2 adjustment",
      "are reported in parenetheses (cluster unit is experimental weeks).",
      "Covariates are number of past coordinations,",
      "number of hospitals per 10 square kilometers,",
      "number of hospitals with PBSC collection per 10 square kilometers,",
      "number of hospitals with BM collection per 10 square kilometers,",
      "month dummies, and week dummies.",
      "Bootstrap p-value is based on the wild cluster bootstrap-t with a null hypothesis imposed,",
      "which is proposed by Cameron et al. (2008)."
    )) %>%
    padding(10:(10 + (obj$check - 1)), padding.left = 10) %>%
    fontsize(size = 9, part = "all") %>%
    ft_theme()
}
