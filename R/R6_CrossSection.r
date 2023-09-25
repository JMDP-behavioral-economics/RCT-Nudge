library(R6)

CrossSection <- R6Class("CrossSection", list(
  data = NULL,
  label = NULL,
  initialize = function(rawdt) {
    self$data <- rawdt %>%
      dplyr::filter(ongoing == 0) %>%
      mutate(
        treat = factor(treat, levels = LETTERS[1:4]),
        plan_two_methods = if_else(plan_method == "BM/PB", 1, 0),
        age_less30 = if_else(age < 30, 1, 0),
        age_demean = age - mean(rawdt$age)
      )

    self$label <- list(
      reply = "Reply",
      positive = "Positive intention",
      negative = "Negative intention",
      test = "CT",
      candidate = "Candidate",
      consent = "Consent",
      donate = "Donation"
    )
  },
  print = function(...) {
    View(self$data)
  },
  balance = function(is_cluster = params$is_cluster, se = params$se) {
    BalanceTest$new(self$data, is_cluster, se)
  },
  stock = function() {
    StockData$new(self$data, self$label)
  }
))

BalanceTest <- R6Class("BalanceTest", list(
  table = NULL,
  initialize = function(data, is_cluster, se) {
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
        clusters = if (is_cluster) .$RCTweek,
        se_type = se,
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
          "Early Coordination message", "", "", "X", "X", "", "A. Interventions",
          "N", .[1], .[2], .[3], .[4], "", "B. Sample Size"
        )
      }

    self$table <- bind_rows(size, balance_test)
  },
  flextable = function() {
    self$table %>%
      as_grouped_data("type") %>%
      as_flextable(hide_grouplabel = TRUE) %>%
      set_header_labels(vars = "", p = "F-test, p-value") %>%
      add_header_row(values = c("", "Experimental Arms", ""), colwidths = c(1, 4, 1)) %>%
      add_footer_lines(paste(
        "Notes: Balance test regresses a covariate on treatment dummies",
        "and test a null hypothesis that all coefficients are zero.",
        "We use the robust standard errors for statistical inference."
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
))