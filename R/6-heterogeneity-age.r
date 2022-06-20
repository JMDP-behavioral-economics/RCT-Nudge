#+ include = FALSE
library(here)
source(here("R", "_library.r"))

#+ include = FALSE
root <- "D:/JMDPフィールド実験"

rawdt <- read_csv(
  here(root, "shaped.csv"),
  locale = locale(encoding = "cp932")
)

use <- rawdt %>%
  mutate(
    treat = factor(treat, levels = LETTERS[1:4]),
    prefecture = factor(prefecture, c("東京都", unique(rawdt$prefecture)[-11])),
    age = age - median(rawdt$age)
  )

#+ include = FALSE
out_lev <- c(
  "reply", "intention", "test",
  "candidate", "consent", "donate"
)

out_lab <- c(
  "Reply to notification",
  "Intention",
  "Confirmatory typing",
  "Candidate",
  "Final consent",
  "Donation"
)

#' サンプルを年齢順に4等分して、各サブサンプルで固定効果モデルを推定した。
#'
#+
est <- use %>%
  select(
    reply,
    intention,
    test,
    candidate,
    consent,
    donate,
    treat,
    month,
    week,
    prefecture,
    male,
    age
  ) %>%
  dplyr::filter(prefecture != "海外") %>%
  mutate(prefecture = droplevels(prefecture)) %>%
  mutate(age_group = ntile(age, 4)) %>%
  group_by(age_group) %>%
  mutate(
    min_age = min(age),
    max_age = max(age)
  ) %>%
  ungroup() %>%
  pivot_longer(reply:donate, names_to = "outcome", values_to = "value") %>%
  mutate(outcome = factor(outcome, out_lev, out_lab)) %>%
  group_by(outcome, age_group) %>%
  do(
    est = feols(
      value ~ treat + male + prefecture | month + week,
      cluster = ~ week,
      data = .
    ),
    min = unique(.$min_age),
    max = unique(.$max_age)
  )

testBC <- lapply(est$est, function(x) lincom_fixest(
  matrix(c(1, -1, 0, rep(0, 47)), ncol = 1), x
)) %>% as_vector() %>% sprintf("%1.3f", .)

testBD <- lapply(est$est, function(x) lincom_fixest(
  matrix(c(1, 0, -1, rep(0, 47)), ncol = 1), x
)) %>% as_vector() %>% sprintf("%1.3f", .)

testCD <- lapply(est$est, function(x) lincom_fixest(
  matrix(c(0, 1, -1, rep(0, 47)), ncol = 1), x
)) %>% as_vector() %>% sprintf("%1.3f", .)

#+
plotdt <- est %>%
  summarize(
    outcome = outcome,
    age = age_group,
    coef_B = coef(est)["treatB"],
    coef_C = coef(est)["treatC"],
    coef_D = coef(est)["treatD"],
    se_B = est$coeftable["treatB", 2],
    se_C = est$coeftable["treatC", 2],
    se_D = est$coeftable["treatD", 2],
    lwr.coef_B = coef_B - se_B * abs(qt(0.025, df = df)),
    lwr.coef_C = coef_C - se_C * abs(qt(0.025, df = df)),
    lwr.coef_D = coef_D - se_D * abs(qt(0.025, df = df)),
    upr.coef_B = coef_B + se_B * abs(qt(0.025, df = df)),
    upr.coef_C = coef_C + se_C * abs(qt(0.025, df = df)),
    upr.coef_D = coef_D + se_D * abs(qt(0.025, df = df))
  ) %>%
  tidyr::pivot_longer(
    coef_B:upr.coef_D,
    names_to = c("stats", "treat"),
    names_pattern = "(.*)_(.)"
  ) %>%
  tidyr::pivot_wider(names_from = stats, values_from = value) %>%
  mutate(
    treat = factor(treat, levels = LETTERS[2:4])
  )

scale_x_dt <- est %>%
  select(age_group, min, max) %>%
  distinct() %>%
  group_by(age_group) %>%
  summarize(
    min = min[[1]] + median(rawdt$age),
    max = max[[1]] + median(rawdt$age),
    label = paste0("[", min, ", ", max, "]")
  )

#' 図\@ref(fig:hetero-age-1-3step)にドナー候補者選定前のアウトカムに対するトリートメントの効果をプロットした。
#'
#' - 39～46歳のグループについて、介入B群は意向と確認検査の実施に正の効果を持つ
#'
#+ hetero-age-1-3step, fig.cap = "Heterogenous Effect by Age on Outcomes before Donor Candidate Selection"
plotdt %>%
  dplyr::filter(outcome %in% out_lab[1:3]) %>%
  mutate(age = factor(age, levels = 1:4, labels = scale_x_dt$label)) %>%
  ggplot(aes(x = treat, y = coef)) +
  geom_hline(aes(yintercept = 0), linetype = 2) +
  geom_pointrange(aes(ymin = lwr.coef, ymax = upr.coef)) +
  # scale_x_continuous(labels = scale_x_dt$label) +
  facet_grid(age ~ outcome) +
  labs(x = "Treatment", y = "Estimated treatment effect (95%CI)") +
  simplegg()

#' 図\@ref(fig:hetero-age-4-6step)にドナー候補者選定後のアウトカムに対するトリートメントの効果をプロットした。
#'
#' - どの年齢層でも、介入群はアウトカムに影響を与えない
#'
#+ hetero-age-4-6step, fig.cap = "Heterogenous Effect by Age on Outcomes after Donor Candidate Selection"
plotdt %>%
  dplyr::filter(!(outcome %in% out_lab[1:3])) %>%
  mutate(age = factor(age, levels = 1:4, labels = scale_x_dt$label)) %>%
  ggplot(aes(x = treat, y = coef)) +
  geom_hline(aes(yintercept = 0), linetype = 2) +
  geom_pointrange(aes(ymin = lwr.coef, ymax = upr.coef)) +
  # scale_x_continuous(labels = scale_x_dt$label) +
  facet_grid(age ~ outcome) +
  labs(x = "Treatment", y = "Estimated treatment effect (95%CI)") +
  simplegg()
