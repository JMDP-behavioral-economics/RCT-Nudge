#' ---
#' title: Subsample Analysis of Effect on Flow Variables
#' ---
#'
#+ load-packages, include = FALSE
library(here)
source(here("R", "_library.r"))
source(here("R", "_outcome_labels.r"))

#+ load-data, include=FALSE
root <- "D:/JMDPフィールド実験"

rawdt <- read_csv(
  here(root, "shaped.csv"),
  locale = locale(encoding = "cp932")
)

use <- rawdt %>%
  mutate(treat = factor(treat, levels = LETTERS[1:4])) %>%
  dplyr::filter(ongoing == 0 & prefecture != "海外") %>%
  dplyr::filter(exg_stop_reply == 0) %>%
  rename(positive = intention) %>%
  mutate(
    negative = reply * (1 - positive),
    age_less30 = if_else(age < 30, 1, 0),
  ) %>%
  select(reply, positive, negative, everything()) %>%
  pivot_longer(reply:negative, "outcome") %>%
  mutate(outcome = factor(
    outcome,
    levels = unlist(names(outcome_label)[1:3]),
    labels = unlist(outcome_label[1:3])
  )) %>%
  mutate(
    days_reply = if_else(is.na(days_reply), 10000, days_reply),
    days4 = if_else(days_reply <= 4, value, 0),
    days7 = if_else(days_reply <= 7, value, 0),
    days10 = if_else(days_reply <= 10, value, 0),
    days14 = if_else(days_reply <= 14, value, 0),
    days21 = if_else(days_reply <= 21, value, 0),
    days28 = if_else(days_reply <= 28, value, 0)
  ) %>%
  select(-value) %>%
  pivot_longer(days4:days28, "within", "days") %>%
  mutate(within = as.numeric(within))

#+ reg-reply-flow-subsample, include=FALSE
est_flow <- use %>%
  group_by(outcome, within, male, age_less30) %>%
  nest() %>%
  mutate(fit = map(data, ~ lm_robust(
    update(mod, . ~ . - male - age_demean),
    cluster = RCTweek,
    se_type = "stata",
    data = .x
  ))) %>%
  mutate(
    tidy = map(fit, tidy),
    tidy = map(tidy, ~ subset(.x, str_detect(term, "treat"))),
    tidy = map(tidy, ~ dplyr::select(.x, -outcome))
  ) %>%
  dplyr::select(-data, -fit) %>%
  unnest(cols = tidy) %>%
  mutate(
    term = str_replace(term, "treat", ""),
    term = factor(term, LETTERS[2:4])
  )

#+ reg-plot-reply-flow-female-less30
plot_list <- unique(est_flow$outcome) %>%
  purrr::map(function(x) {
    subset(est_flow, outcome == x & male == 0 & age_less30 == 1) %>%
      ggplot(aes(x = within, y = estimate, color = term, shape = term)) +
      geom_hline(aes(yintercept = 0), linetype = 2) +
      geom_point(size = 3, position = position_dodge(2)) +
      geom_errorbar(
        aes(ymin = conf.low, ymax = conf.high),
        position = position_dodge(2),
        width = 0
      ) +
      scale_x_continuous(breaks = unique(est_flow$within)) +
      scale_y_continuous(
        breaks = seq(-0.2, 0.2, by = 0.05), limits = c(-0.2, 0.15)
      ) +
      labs(
        title = paste("Outcome:", x),
        x = "Days after sending notification",
        y = "Estimated Effects (95%CI)",
        color = "Treatment", shape = "Treatment"
      ) +
      simplegg()
  })

wrap_plots(plot_list, ncol = 2) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

#+ reg-plot-reply-flow-female-over30
plot_list <- unique(est_flow$outcome) %>%
  purrr::map(function(x) {
    subset(est_flow, outcome == x & male == 0 & age_less30 == 0) %>%
      ggplot(aes(x = within, y = estimate, color = term, shape = term)) +
      geom_hline(aes(yintercept = 0), linetype = 2) +
      geom_point(size = 3, position = position_dodge(2)) +
      geom_errorbar(
        aes(ymin = conf.low, ymax = conf.high),
        position = position_dodge(2),
        width = 0
      ) +
      scale_x_continuous(breaks = unique(est_flow$within)) +
      scale_y_continuous(
        breaks = seq(-0.2, 0.2, by = 0.05), limits = c(-0.15, 0.1)
      ) +
      labs(
        title = paste("Outcome:", x),
        x = "Days after sending notification",
        y = "Estimated Effects (95%CI)",
        color = "Treatment", shape = "Treatment"
      ) +
      simplegg()
  })

wrap_plots(plot_list, ncol = 2) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

#+ reg-plot-reply-flow-male-less30
plot_list <- unique(est_flow$outcome) %>%
  purrr::map(function(x) {
    subset(est_flow, outcome == x & male == 1 & age_less30 == 1) %>%
      ggplot(aes(x = within, y = estimate, color = term, shape = term)) +
      geom_hline(aes(yintercept = 0), linetype = 2) +
      geom_point(size = 3, position = position_dodge(2)) +
      geom_errorbar(
        aes(ymin = conf.low, ymax = conf.high),
        position = position_dodge(2),
        width = 0
      ) +
      scale_x_continuous(breaks = unique(est_flow$within)) +
      scale_y_continuous(
        breaks = seq(-0.2, 0.2, by = 0.05), limits = c(-0.1, 0.2)
      ) +
      labs(
        title = paste("Outcome:", x),
        x = "Days after sending notification",
        y = "Estimated Effects (95%CI)",
        color = "Treatment", shape = "Treatment"
      ) +
      simplegg()
  })

wrap_plots(plot_list, ncol = 2) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

#+ reg-plot-reply-flow-male-over30
plot_list <- unique(est_flow$outcome) %>%
  purrr::map(function(x) {
    subset(est_flow, outcome == x & male == 1 & age_less30 == 0) %>%
      ggplot(aes(x = within, y = estimate, color = term, shape = term)) +
      geom_hline(aes(yintercept = 0), linetype = 2) +
      geom_point(size = 3, position = position_dodge(2)) +
      geom_errorbar(
        aes(ymin = conf.low, ymax = conf.high),
        position = position_dodge(2),
        width = 0
      ) +
      scale_x_continuous(breaks = unique(est_flow$within)) +
      scale_y_continuous(
        breaks = seq(-0.2, 0.2, by = 0.05), limits = c(-0.15, 0.1)
      ) +
      labs(
        title = paste("Outcome:", x),
        x = "Days after sending notification",
        y = "Estimated Effects (95%CI)",
        color = "Treatment", shape = "Treatment"
      ) +
      simplegg()
  })

wrap_plots(plot_list, ncol = 2) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")
