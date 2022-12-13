#' ---
#' title: About Field Experiment
#' subtitle: balance test
#' ---
#'
#+ load-packages, include = FALSE
library(here)
source(here("R", "_library.r"))
source(here("R", "_outcome_labels.r"))

#+ load-data, include = FALSE
root <- "D:/JMDPフィールド実験"

rawdt <- read_csv(
  here(root, "shaped.csv"),
  locale = locale(encoding = "cp932")
)

use <- rawdt %>%
  dplyr::filter(ongoing == 0 & prefecture != "海外") %>%
  mutate(treat = factor(treat, levels = LETTERS[1:4]))

#+ balance-test
balance_test <- use %>%
  select(male, age, coordinate, toka, treat, RCTweek) %>%
  pivot_longer(male:toka, values_to = "value", names_to = "vars") %>%
  group_by(vars) %>%
  do(est = lm_robust(
    value ~ treat,
    clusters = RCTweek,
    se_type = "stata",
    data = .
  )) %>%
  summarize(
    vars = vars,
    f = summary(est)$fstatistic[1],
    numdf = summary(est)$fstatistic[2],
    dendf = summary(est)$fstatistic[3],
    "p-value" = pf(f, numdf, dendf, lower.tail = FALSE)
  )

#+ overview-experiment
size <- with(use, sprintf("%1d", table(treat))) %>%
  {
    tribble(
      ~terms, ~A, ~B, ~C, ~D, ~"p-value",
      "通常の適合通知", "X", "X", "X", "X", "",
      "確率メッセージ", "", "X", "", "X", "",
      "移植患者情報", "", "", "X", "X", "",
      "サンプルサイズ", .[1], .[2], .[3], .[4], ""
    )
  }

attr(size, "position") <- seq(nrow(size))

note <- paste(
  "Note: p値はバランステストの結果である。",
  "バランステストは実験群を共変量に線形回帰したF検定を用いている。"
)

datasummary(
  (`年齢` = age) +
  (`コーディネーション回数` = coordinate) +
  (`男性` = male) +
  (`東京・大阪・神奈川・愛知` = toka) ~ mean * treat,
  data = use,
  title = "Overview of Field Experiment",
  add_rows = size,
  add_columns = balance_test[, 5],
  align = "lccccc"
) %>%
kableExtra::kable_styling() %>%
add_header_above(c(" " = 1, "実験群" = 4, " " = 1)) %>%
group_rows("A. 介入", 1, 3) %>%
group_rows("B. サンプルサイズ", 4, 4) %>%
group_rows("C. 共変量", 5, 8)
