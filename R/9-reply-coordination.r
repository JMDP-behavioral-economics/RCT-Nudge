#' ---
#' title: 返信日数とコーディネーション過程の関係
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
  dplyr::filter(reply == 1) %>%
  dplyr::filter(ongoing == 0 & prefecture != "海外") %>%
  mutate(treat = factor(treat, levels = LETTERS[1:4])) %>%
  select(test, candidate, consent, donate, everything()) %>%
  pivot_longer(test:donate, names_to = "outcome") %>%
  dplyr::left_join(exclude, by = c("id", "outcome")) %>%
  mutate(
    age_demean = age - mean(rawdt$age),
    within = case_when(
      days_reply <= 7 ~ "1 week",
      days_reply <= 14 ~ "2 weeks",
      TRUE ~ "more than 2 weeks"
    ),
    outcome = factor(
      outcome,
      levels = unlist(names(outcome_label)[4:7]),
      labels = unlist(outcome_label[4:7])
    )
  )

#+ reg-coordination-speed, include=FALSE
model <- value ~ treat + age_demean + male + coordinate + within +
  factor(prefecture) + factor(month) + factor(week)

est_process <- use %>%
  group_by(outcome) %>%
  do(fit = lm_robust(
    model,
    cluster = RCTweek,
    se_type = "stata",
    data = subset(., exclude == 0)
  ))

est_process_cross <- use %>%
  group_by(outcome) %>%
  do(fit = lm_robust(
    update(model, . ~ . + treat:within),
    cluster = RCTweek,
    se_type = "stata",
    data = subset(., exclude == 0)
  ))

#+ reg-tab-coordination-speed
est_process %>%
  pull(fit) %>%
  setNames(paste0("(", seq_len(length(.)), ")")) %>%
  modelsummary(
    stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
    coef_map = c(
      "treatB" = "B",
      "treatC" = "C",
      "treatD" = "D",
      "within2 weeks" = "between 1 and 2 week",
      "withinmore than 2 weeks" = "more than 2 weeks"
    ),
    gof_omit = "R2|AIC|BIC|Log|Std|FE|se_type"
  ) %>%
  kableExtra::kable_styling() %>%
  kableExtra::group_rows("Treatment Effect", 1, 6) %>%
  kableExtra::group_rows("Reply Speed (Reference: within 1 week)", 7, 10) %>%
  kableExtra::add_header_above(c(" ", as.character(est_process$outcome)))

#+ reg-tab-coordination-speed2
est_process_cross %>%
  pull(fit) %>%
  setNames(paste0("(", seq_len(length(.)), ")")) %>%
  modelsummary(
    stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
    coef_map = c(
      "within2 weeks" = "between 1 and 2 week",
      "withinmore than 2 weeks" = "more than 2 weeks",
      "treatB" = "B",
      "treatB:within2 weeks" = "B \u00d7 between 1 and 2 week",
      "treatB:withinmore than 2 weeks" = "B \u00d7 more than 2 week",
      "treatC" = "C",
      "treatC:within2 weeks" = "C \u00d7 between 1 and 2 week",
      "treatC:withinmore than 2 weeks" = "C \u00d7 more than 2 week",
      "treatD" = "D",
      "treatD:within2 weeks" = "D \u00d7 between 1 and 2 week",
      "treatD:withinmore than 2 weeks" = "D \u00d7 more than 2 week"
    ),
    gof_omit = "R2|AIC|BIC|Log|Std|FE|se_type"
  ) %>%
  kableExtra::kable_styling() %>%
  kableExtra::group_rows("Reply Speed (Reference: within 1 week)", 1, 4) %>%
  kableExtra::group_rows("Heterogenous Effect of Message B", 5, 10) %>%
  kableExtra::group_rows("Heterogenous Effect of Message C", 11, 16) %>%
  kableExtra::group_rows("Heterogenous Effect of Message D", 17, 22) %>%
  kableExtra::add_header_above(c(" ", as.character(est_process$outcome)))
