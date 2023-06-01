summary_experiment <- function( data,
                                cluster = params$is_cluster,
                                se_type = params$se_type)
{
  summary_stat <- data %>%
    select(
      male,
      age,
      coordinate,
      hospital_per_area,
      PB_per_area,
      BM_per_area,
      treat
    ) %>%
    group_by(treat) %>%
    summarize_all(mean) %>%
    mutate_at(vars(-treat), list(~ sprintf("%1.3f", .))) %>%
    pivot_longer(-treat, names_to = "vars", values_to = "mean") %>%
    pivot_wider(names_from = "treat", values_from = "mean")

  balance_p <- data %>%
    select(
      male,
      age,
      coordinate,
      hospital_per_area,
      PB_per_area,
      BM_per_area,
      treat,
      RCTweek
    ) %>%
    pivot_longer(male:BM_per_area, values_to = "value", names_to = "vars") %>%
    group_by(vars) %>%
    do(est = lm_robust(
      value ~ treat,
      clusters = if (cluster) .$RCTweek,
      se_type = se_type,
      data = .
    )) %>%
    summarize(
      vars = vars,
      f = summary(est)$fstatistic[1],
      numdf = summary(est)$fstatistic[2],
      dendf = summary(est)$fstatistic[3],
      p = sprintf("%1.3f", pf(f, numdf, dendf, lower.tail = FALSE))
    ) %>%
    select(vars, p)

  balance_test <- summary_stat %>%
    dplyr::left_join(balance_p, by = "vars") %>%
    mutate(type = "C. Balance Test") %>%
    mutate(vars = recode(
      vars,
      "male" = "Male (=1)",
      "age" = "Age",
      "coordinate" = "Number of past coordinations",
      "hospital_per_area" = "Number of listed hospitals",
      "PB_per_area" = "Number of hospitals listed with PBSC collection",
      "BM_per_area" = "Number of hospitals listed with BM collection"
    ))

  size <- with(data, sprintf("%1d", table(treat))) %>%
    {
      tribble(
        ~vars, ~A, ~B, ~C, ~D, ~p, ~type,
        "Standard notification", "X", "X", "X", "X", "", "A. Interventions",
        "Probability message", "", "X", "", "X", "", "A. Interventions",
        "Patients message", "", "", "X", "X", "", "A. Interventions",
        "N", .[1], .[2], .[3], .[4], "", "B. Sample Size"
      )
    }

  tbl_experiment_summary <- bind_rows(size, balance_test)

  return(tbl_experiment_summary)
}

tbl_summary_experiment <- function(obj) {
  obj %>%
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