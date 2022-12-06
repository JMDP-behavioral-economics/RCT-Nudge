#' ---
#' title: Subsample Analysis of Effect on Coordination Process
#' ---
#'
#+ load-packages, include=FALSE
library(here)
source(here("R", "_library.r"))
source(here("R", "_outcome_labels.r"))

#+ load-data, include=FALSE
root <- "D:/JMDPフィールド実験"

rawdt <- read_csv(
  here(root, "shaped.csv"),
  locale = locale(encoding = "cp932")
)

exclude <- rawdt %>%
  select(id, starts_with("exg_stop")) %>%
  pivot_longer(
    -id,
    names_to = "outcome", values_to = "exclude",
    names_prefix = "exg_stop_"
  )

use <- rawdt %>%
  dplyr::filter(ongoing == 0 & prefecture != "海外") %>%
  mutate(treat = factor(treat, levels = LETTERS[1:4])) %>%
  select(test, candidate, consent, donate, everything()) %>%
  pivot_longer(test:donate, names_to = "outcome") %>%
  dplyr::left_join(exclude, by = c("id", "outcome")) %>%
  mutate(
    age_less30 = if_else(age < 30, 1, 0),
    outcome = factor(
      outcome,
      levels = unlist(names(outcome_label)[4:7]),
      labels = unlist(outcome_label[4:7])
    )
  )

#+ reg-process-subsample, include=FALSE
model <- value ~ treat + coordinate +
  factor(prefecture) + factor(month) + factor(week)

est_process <- use %>%
  dplyr::filter(exclude == 0) %>%
  group_by(outcome, male, age_less30) %>%
  nest() %>%
  mutate(fit = map(data, ~ lm_robust(
    model,
    cluster = RCTweek,
    se_type = "stata",
    data = .x
  ))) %>%
  mutate(
    tidy = map(fit, tidy),
    tidy = map(tidy, ~ subset(.x, str_detect(term, "treat"))),
    tidy = map(tidy, ~ dplyr::select(.x, -outcome)),
    N = map_chr(fit, ~ paste0("N=", nobs(.x)))
  ) %>%
  select(-data, -fit) %>%
  unnest(cols = tidy) %>%
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
    )
  )

#+ reg-plot-process-subsample, fig.width=10, fig.height=7
est_process %>%
  ggplot(aes(x = pos, y = estimate)) +
  geom_hline(aes(yintercept = 0), linetype = 2) +
  geom_point(
    aes(shape = term, color = term),
    size = 3,
    position = position_dodge(0.5)
  ) +
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high, color = term),
    position = position_dodge(0.5), width = 0
  ) +
  geom_text(
    aes(y = -0.075, label = N, group = outcome),
    data = ungroup(est_process) %>%
      select(outcome, male, age_less30, N, pos) %>%
      distinct(),
    color = "black"
  ) +
  facet_wrap(~outcome, ncol = 2) +
  scale_y_continuous(
    limits = c(-0.1, 0.2),
    breaks = seq(-0.2, 0.2, by = 0.05)
  ) +
  labs(
    x = "Subset",
    y = "Estimated Effects (95%CI)",
    shape = "Treatments", color = "Treatments"
  ) +
  simplegg()
