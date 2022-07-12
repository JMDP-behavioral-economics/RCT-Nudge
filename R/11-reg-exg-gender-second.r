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
    age = (age - median(rawdt$age)) / 100
  ) %>%
  dplyr::filter(coordinate > 1)

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

#' 性別によってサブサンプルを構築して、固定効果モデルを推定した。
#'
#+
main <- use %>%
  dplyr::filter(prefecture != "海外") %>%
  mutate(prefecture = droplevels(prefecture)) %>%
  select(
    id,
    treat,
    month,
    week,
    prefecture,
    male,
    age,
    reply,
    intention:donate
  ) %>%
  pivot_longer(reply:donate, names_to = "outcome")

exclude <- use %>%
  select(id, starts_with("exg_stop")) %>%
  pivot_longer(
    -id, names_to = "outcome", values_to = "exclude",
    names_prefix = "exg_stop_"
  )

estdt <- main %>%
  dplyr::left_join(exclude, by = c("id", "outcome"))

#+
est <- estdt %>%
  mutate(outcome = factor(outcome, out_lev, out_lab)) %>%
  group_by(outcome, male) %>%
  do(est = feols(
    value ~ treat + poly(age, 2, raw = TRUE) + prefecture |
      month + week,
    cluster = ~ week,
    data = subset(., exclude == 0)
  ))

testBC <- lapply(est$est, function(x) lincom_fixest(
  matrix(c(1, -1, 0, rep(0, 48)), ncol = 1), x
)) %>% as_vector() %>% sprintf("%1.3f", .)

testBD <- lapply(est$est, function(x) lincom_fixest(
  matrix(c(1, 0, -1, rep(0, 48)), ncol = 1), x
)) %>% as_vector() %>% sprintf("%1.3f", .)

testCD <- lapply(est$est, function(x) lincom_fixest(
  matrix(c(0, 1, -1, rep(0, 48)), ncol = 1), x
)) %>% as_vector() %>% sprintf("%1.3f", .)

#+ eval = FALSE
est %>%
  dplyr::filter(male == 1) %>%
  pull(est, name = outcome) %>%
  modelsummary(
    title = "Regression Results among Males",
    stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
    coef_map = c(
      "treatB" = "Treatment B",
      "treatC" = "Treatment C",
      "treatD" = "Treatment D",
      "poly(age, 2, raw = TRUE)1" = "Age",
      "poly(age, 2, raw = TRUE)2" = "Squared age"
    ),
    gof_omit = "R2 Adj.|R2 Within|R2 Pseudo|AIC|BIC|Log|Std|FE",
    add_rows = tribble(
      ~terms, ~"(1)", ~"(2)", ~"(3)", ~"(4)", ~"(5)", ~"(6)",
      "Prefecture dummies",
      "X", "X", "X", "X", "X", "X",
      "Week and month fixed effect", "X", "X", "X", "X", "X", "X"
    ) %>%
    rbind(c("B = C", testBC)) %>%
    rbind(c("B = D", testBD)) %>%
    rbind(c("C = D", testCD))
  ) %>%
  kableExtra::kable_styling() %>%
  kableExtra::group_rows(
    "F-tests, p-value", 15, 17, bold = FALSE, italic = TRUE
  )

#+ eval = FALSE
est %>%
  dplyr::filter(male == 0) %>%
  pull(est, name = outcome) %>%
  modelsummary(
    title = "Regression Results among Females",
    stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
    coef_map = c(
      "treatB" = "Treatment B",
      "treatC" = "Treatment C",
      "treatD" = "Treatment D",
      "poly(age, 2, raw = TRUE)1" = "Age",
      "poly(age, 2, raw = TRUE)2" = "Squared age"
    ),
    gof_omit = "R2 Adj.|R2 Within|R2 Pseudo|AIC|BIC|Log|Std|FE",
    add_rows = tribble(
      ~terms, ~"(1)", ~"(2)", ~"(3)", ~"(4)", ~"(5)", ~"(6)",
      "Prefecture dummies",
      "X", "X", "X", "X", "X", "X",
      "Week and month fixed effect", "X", "X", "X", "X", "X", "X"
    ) %>%
    rbind(c("B = C", testBC)) %>%
    rbind(c("B = D", testBD)) %>%
    rbind(c("C = D", testCD))
  ) %>%
  kableExtra::kable_styling() %>%
  kableExtra::group_rows(
    "F-tests, p-value", 15, 17, bold = FALSE, italic = TRUE
  )

#+
df <- length(unique(use$week)) - 1

plotdt <- est %>%
  summarize(
    outcome = outcome,
    male = male,
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
    male = factor(male, levels = c(1, 0), labels = c("Males", "Females")),
    treat = factor(treat, levels = LETTERS[2:4])
  )

#'
#' 図\@ref(fig:hetero-gender-1-3step)にドナー候補者選定前のアウトカムに対するトリートメントの効果をプロットした。
#'
#' - 男性について、介入B群は確認検査の実施に正の効果を持ち、介入D群は返信に正の効果を持っている
#' - 女性について、介入D群は確認検査の実施に正の効果を持っている
#' 
#+ hetero-gender-1-3step, fig.cap = "Heterogenous Effect by Gender on Outcomes before Donor Candidate Selection"
plotdt %>%
  dplyr::filter(outcome %in% out_lab[1:3]) %>%
  ggplot(aes(x = treat, y = coef)) +
  geom_hline(aes(yintercept = 0), linetype = 2) +
  geom_pointrange(
    aes(ymin = lwr.coef, ymax = upr.coef),
    size = 1
  ) +
  facet_grid(male ~ outcome) +
  labs(
    x = "Treatment",
    y = "Estimated treatment effect (95%CI)"
  ) +
  simplegg()

#'
#' 図\@ref(fig:hetero-gender-4-6step)にドナー候補者選定後のアウトカムに対するトリートメントの効果をプロットした。
#'
#' - 男性について、介入C群は最終同意に負の効果を持っている
#'
#+ hetero-gender-4-6step, fig.cap = "Heterogenous Effect by Gender on Outcomes after Donor Candidate Selection"
plotdt %>%
  dplyr::filter(!(outcome %in% out_lab[1:3])) %>%
  ggplot(aes(x = treat, y = coef)) +
  geom_hline(aes(yintercept = 0), linetype = 2) +
  geom_pointrange(
    aes(ymin = lwr.coef, ymax = upr.coef),
    size = 1
  ) +
  facet_grid(male ~ outcome) +
  labs(
    x = "Treatment",
    y = "Estimated treatment effect (95%CI)"
  ) +
  simplegg()
