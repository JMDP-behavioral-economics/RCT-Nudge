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
  "Reply to invitation",
  "Intention",
  "Confirmatory typing",
  "Candidate",
  "Final consent",
  "Donation"
)

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
  pivot_longer(reply:donate, names_to = "outcome", values_to = "value") %>%
  mutate(outcome = factor(outcome, out_lev, out_lab)) %>%
  group_by(outcome) %>%
  do(est = feols(
    value ~ treat + age + male + prefecture | month + week,
    cluster = ~ week,
    data = .
  ))

testBC <- lapply(est$est, function(x) lincom_fixest(
  matrix(c(1, -1, 0, rep(0, 49)), ncol = 1), x
)) %>% as_vector() %>% sprintf("%1.3f", .)

testBD <- lapply(est$est, function(x) lincom_fixest(
  matrix(c(1, 0, -1, rep(0, 49)), ncol = 1), x
)) %>% as_vector() %>% sprintf("%1.3f", .)

testCD <- lapply(est$est, function(x) lincom_fixest(
  matrix(c(0, 1, -1, rep(0, 49)), ncol = 1), x
)) %>% as_vector() %>% sprintf("%1.3f", .)

#+
show_est <- est$est
names(show_est) <- paste(
  est$outcome, "\n", paste0("(", seq(length(show_est)), ")")
)

modelsummary(
  show_est,
  stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  coef_map = c(
    "treatB" = "Treatment B",
    "treatC" = "Treatment C",
    "treatD" = "Treatment D"
  ),
  gof_omit = "R2 Adj.|R2 Within|R2 Pseudo|AIC|BIC|Log|Std|FE",
  add_rows = tribble(
    ~terms, ~"(1)", ~"(2)", ~"(3)", ~"(4)", ~"(5)", ~"(6)",
    "Male dummy, age, prefecture dummies", "X", "X", "X", "X", "X", "X",
    "Week and month fixed effect", "X", "X", "X", "X", "X", "X"
  ) %>%
  rbind(c("B = C", testBC)) %>%
  rbind(c("B = D", testBD)) %>%
  rbind(c("C = D", testCD))
) %>%
kableExtra::kable_styling() %>%
kableExtra::group_rows("F-tests, p-value", 11, 13, bold = FALSE, italic = TRUE)
