#' ---
#' title: Subsample Analysis of Effect on Reply and Intentions
#' ---
#'
#+ load-packages, include = FALSE
library(here)
source(here("R", "_library.r"))
source(here("R", "_outcome_labels.r"))

#+ load-data, include=FALSE
root <- "D:/JMDPフィールド実験"

schedule <- read_csv(here(root, "RCT-schedule.csv"))

rawdt <- read_csv(
  here(root, "shaped.csv"),
  locale = locale(encoding = "cp932")
)

use <- rawdt %>%
  mutate(treat = factor(treat, levels = LETTERS[1:4])) %>%
  dplyr::filter(ongoing == 0 & prefecture != "海外") %>%
  dplyr::filter(exg_stop_reply == 0) %>%
  rename(positive = intention) %>%
  mutate(negative = reply * (1 - positive)) %>%
  select(reply, positive, negative, everything()) %>%
  pivot_longer(reply:negative, "outcome") %>%
  mutate(outcome = factor(
    outcome,
    levels = unlist(names(outcome_label)[1:3]),
    labels = unlist(outcome_label[1:3])
  ))

#+ reg-reply-stock-subsample, include=FALSE
mod <- value ~ treat + coordinate +
  factor(prefecture) + factor(month) + factor(week)
  
est_stock <- use %>%
  mutate(age_less30 = if_else(age < 30, 1, 0)) %>%
  group_by(outcome, male, age_less30) %>%
  nest() %>%
  mutate(est = map(data, ~ lm_robust(
    mod,
    cluster = RCTweek,
    se_type = "stata",
    data = .x
  ))) %>%
  mutate(
    fit = map(est, tidy),
    fit = map(fit, ~ subset(.x, str_detect(term, "treat"))),
    fit = map(fit, ~ dplyr::select(.x, -outcome)),
    N = map_chr(est, ~ paste0("N=", nobs(.x)))
  ) %>%
  select(-data, -est) %>%
  unnest(cols = fit) %>%
  mutate(
    pos = paste0(male, age_less30),
    pos = factor(
      pos,
      levels = c("01", "00", "11", "10"),
      labels = c(
        "Female\u00d7\nAge\u226430",
        "Female\u00d7\n30<Age",
        "Male\u00d7\nAge\u226430",
        "Male\u00d7\n30<Age"
      )
    ),
    term = str_replace(term, "treat", ""),
    term = factor(term, LETTERS[2:4])
  )

#+ reg-plot-reply-stock-subsample
plot_list <- unique(est_stock$outcome) %>%
  purrr::map(function(x) {
    subset(est_stock, outcome == x) %>%
      ggplot(aes(x = pos, y = estimate)) +
      geom_hline(aes(yintercept = 0), linetype = 2) +
      geom_point(
        aes(color = term, shape = term),
        size = 3, position = position_dodge(0.5)
      ) +
      geom_errorbar(
        aes(ymin = conf.low, ymax = conf.high, color = term),
        position = position_dodge(0.5),
        width = 0
      ) +
      geom_text(
        aes(y = -0.125, label = N),
        data = ungroup(est_stock) %>%
          select(male, age_less30, N, pos) %>%
          distinct(),
        color = "black"
      ) +
      scale_y_continuous(
        breaks = seq(-0.2, 0.2, by = 0.05), limits = c(-0.17, 0.17)
      ) +
      labs(
        title = paste("Outcome:", x),
        x = "Subset",
        y = "Estimated Effects (95%CI)",
        color = "Treatment", shape = "Treatment"
      ) +
      simplegg()
  })

wrap_plots(plot_list, ncol = 2) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")
