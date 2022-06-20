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

#'
#' $m$月の第$w$週に適合通知を受け取った個人$i$について、
#' 以下の固定効果モデルを考える。
#' $$
#'   Y_{imw} =
#'   \beta_1 \cdot \text{B}_{mw} + \beta_2 \cdot \text{C}_{mw}
#'   + \beta_3 \cdot \text{D}_{mw}
#'   + X'_i \gamma + \lambda_m + \theta_w + u_{imw},
#' $$
#' ここで$X_i$は個人$i$の属性に関する共変量で、
#' 性別・年齢・居住する都道府県・コーディネーション回数であり、
#' $\lambda_m$と$\theta_w$は週・月の固定効果である。
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
    age,
    coordinate
  ) %>%
  pivot_longer(reply:donate, names_to = "outcome", values_to = "value") %>%
  mutate(outcome = factor(outcome, out_lev, out_lab)) %>%
  group_by(outcome) %>%
  do(est = feols(
    value ~ treat + age + male + coordinate + prefecture | month + week,
    cluster = ~ week,
    data = .
  ))

testBC <- lapply(est$est, function(x) lincom_fixest(
  matrix(c(1, -1, 0, rep(0, 50)), ncol = 1), x
)) %>% as_vector() %>% sprintf("%1.3f", .)

testBD <- lapply(est$est, function(x) lincom_fixest(
  matrix(c(1, 0, -1, rep(0, 50)), ncol = 1), x
)) %>% as_vector() %>% sprintf("%1.3f", .)

testCD <- lapply(est$est, function(x) lincom_fixest(
  matrix(c(0, 1, -1, rep(0, 50)), ncol = 1), x
)) %>% as_vector() %>% sprintf("%1.3f", .)

#'
#' 表\@ref(tab:reg-main)は固定効果モデルの推定結果である。
#'
#' - 新しく得られた結果
#'   - 週・月の固定効果と個人の属性を考慮すると、介入群Dの返信率はコントロールよりも8%ポイント高く、
#'   これは統計的に5%水準で有意である。
#' - t検定で統計的に10%以内で有意である差の棄却率が増加する
#'   - 意向比率について、介入B群とコントロール群の差は統計的に非有意であるが、
#'   介入B群と介入C群の差（ $\hat{\beta}_1 - \hat{\beta}_2 =$ 2.4%ポイント）は統計的に10%水準で有意である。
#'   - 確認検査の実施率について、介入B群とコントロール群の差（3.2%ポイント）は統計的に10%水準で有意であるが、
#'   介入D群とコントロール群の差は統計的に非有意である。
#' - 個人属性に関するミスマッチ
#'   - 男性は返信や意向を示す可能性が低いが、第一候補者として選ばれる可能性が高い
#'   - 高齢であればあるほど、返信や意向を示す可能性が高いが、第一候補者として選ばれる可能性が低い
#'
#+ reg-main
est %>%
  pull(est, name = outcome) %>%
  modelsummary(
    title = "Estimation of Fixed-Effect Models",
    stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
    coef_map = c(
      "treatB" = "Treatment B",
      "treatC" = "Treatment C",
      "treatD" = "Treatment D",
      "male" = "Male",
      "age" = "Age"
    ),
    gof_omit = "R2 Adj.|R2 Within|R2 Pseudo|AIC|BIC|Log|Std|FE",
    add_rows = tribble(
      ~terms, ~"(1)", ~"(2)", ~"(3)", ~"(4)", ~"(5)", ~"(6)",
      "Number of coordinations, prefecture dummies",
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
  ) %>%
  kableExtra::column_spec(2:7, "7em")

#' 回帰分析の結果はt検定で一貫性に欠けるものだが、
#' 全体的に*One chance in million*の介入を施した介入B群とD群が
#' 適合通知の返信・意向・確認検査の実施に正の影響を与えているかもしれないが、
#' 第一候補者に選ばれた後のプロセスに影響を持たない。