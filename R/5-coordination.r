#' ---
#' title: Effect on Coordination Process
#' ---
#'
#+ load-packages, include=FALSE
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
    age_demean = age - mean(rawdt$age),
    outcome = factor(
      outcome,
      levels = unlist(names(outcome_label)[4:7]),
      labels = unlist(outcome_label[4:7])
    )
  )

#+ reg-coordination, include=FALSE
model <- value ~ treat + age_demean + male + coordinate +
  factor(prefecture) + factor(month) + factor(week)

est_process <- use %>%
  group_by(outcome) %>%
  do(fit = lm_robust(
    model,
    cluster = RCTweek,
    se_type = "stata",
    data = subset(., exclude == 0)
  ))

#+ reg-fisher-process, include=FALSE
plan(multisession)
seed <- 120511L
draw <- 1000

fisher <- 1:draw %>%
  furrr::future_map(function(x) {
    sim <- tibble(
      RCTweek = schedule$RCTweek,
      simulate = schedule[sample(nrow(schedule)), ]$treat
    )

    stats <- use %>%
      left_join(sim, by = "RCTweek") %>%
      group_by(outcome) %>%
      nest() %>%
      mutate(
        est = map(data, ~ lm_robust(
          update(model, . ~ . - treat + simulate),
          se_type = "HC0",
          data = .x
        )),
        coef = map(est, coef),
        vcov = map(est, ~ diag(vcov(.x))),
        stat = map2(
          coef, vcov,
          ~ c(.x^2 * .y^(-1))[c("simulateB", "simulateC", "simulateD")]
        )
      ) %>%
      select(outcome, stat) %>%
      unnest(cols = stat) %>%
      mutate(
        draw = x,
        treat = str_replace(attr(stat, "names"), "simulate", "")
      ) %>%
      ungroup()
  }, .options = furrr_options(seed = seed))

observed <- est_process %>%
  group_by(outcome) %>%
  mutate(
    coef = map(fit, coef),
    vcov = map(fit, ~ diag(vcov(.x))),
    true = map2(
      coef, vcov,
      ~ c(.x^2 * .y^(-1))[c("treatB", "treatC", "treatD")]
    )
  ) %>%
  select(outcome, true) %>%
  unnest(cols = true) %>%
  mutate(treat = str_replace(attr(true, "names"), "treat", ""))

set.seed(seed)
fisher_p <- fisher %>%
  reduce(bind_rows) %>%
  left_join(observed, by = c("outcome", "treat")) %>%
  mutate(
    greater = if_else(stat > true, 1, 0),
    equal = if_else(stat == true, 1, 0)
  ) %>%
  group_by(outcome, treat) %>%
  summarize(
    greater = sum(greater),
    equal = sum(equal)
  ) %>%
  mutate(
    p = greater / (draw + 1) +
      runif(1) * (equal + 1) / (draw + 1)
  ) %>%
  select(-greater, -equal) %>%
  pivot_wider(names_from = "outcome", values_from = "p") %>%
  rename(terms = treat)

plan(sequential)

#+ reg-tab-process
ctrl_avg <- use %>%
  dplyr::filter(exclude == 0) %>%
  dplyr::filter(treat == "A") %>%
  group_by(outcome) %>%
  summarize(mean = mean(value)) %>%
  pivot_wider(names_from = outcome, values_from = mean) %>%
  bind_cols(tibble(terms = "Control Avg."), .)

add_table <- bind_rows(ctrl_avg, fisher_p)
attr(add_table, "position") <- 7:10

est_process %>%
  pull(fit) %>%
  setNames(paste0("(", seq_len(length(.)), ")")) %>%
  modelsummary(
    stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
    coef_map = c(
      "treatB" = "B",
      "treatC" = "C",
      "treatD" = "D"
    ),
    gof_omit = "R2|AIC|BIC|Log|Std|FE|se_type",
    add_rows = add_table
  ) %>%
  kableExtra::kable_styling(font_size = 8) %>%
  kableExtra::add_header_above(
    c(" ", as.character(est_process$outcome))
  ) %>%
  kableExtra::pack_rows(
    "Randomization-based test, p-value",
    8, 10,
    bold = FALSE, italic = TRUE
  ) %>%
  kableExtra::column_spec(2:5, width = "5em")
