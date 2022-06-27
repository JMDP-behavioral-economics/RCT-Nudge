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
    treat = factor(treat, levels = LETTERS[1:4])
  )

schedule <- read_csv(here(root, "RCT-schedule.csv"))

#'
#' <!---
#' フィールド実験の対象者は骨髄バンクドナー確定後に「適合通知」を受け取るドナー候補者である。
#' ドナー候補者確定後、骨髄バンクは対象者に幹細胞提供を依頼する「適合通知」および
#' それを郵送した旨を伝えるSNSメーセージを送付する。
#' 我々は行動科学の知見に基づいたメッセージを適合通知に加える介入を実施する。
#' 現行の適合通知（*Basic notification*）は以下の通りである。
#'
#' > この度、あなたと骨髄バンクの登録患者さんのHLA型（白血球の型）が一致し、
#' ドナー候補者のおひとりに選ばれました。
#' 今後、ご提供に向け詳しい検査や面談を希望されるかをお伺いしたく連絡させていただきました。
#' 同封の資料をよくお読みいただき、コーディネートが可能かどうか検討の上、
#' この案内が届いてから７日以内に返信用紙ほかをご返送ください。
#' 返送後、コーディネートを進めさせていただく場合は、
#' 担当者よりご相談のお電話を差し上げますのでよろしくお願い申し上げます。
#'
#' 介入は以上の文章に以下の2種類のメッセージを加える。
#'
#' - *One chance in million*: １人の登録患者さんとHLA型が一致するドナー登録者は数百〜数万人に1人です。
#' ドナー候補者が複数みつかる場合もありますが、多くはないこともご理解頂ければ幸いです。
#' - *Transplant recipient*: 骨髄バンクを介して移植ができる患者さんは現在約6割にとどまっています。
#' 骨髄等を提供するドナーが早く見つかれば、その比率を高めることができます。
#'
#' 現行の適合通知にメッセージを加えないコントロール群（A）に加えて、
#' 我々は2つの介入を用いて3つの実験群を作成した。
#'
#' 1. B: One chance in million
#' 1. C: Transplant recipient
#' 1. D: One chance in million + Transplant recipient
#'
#' 我々はフィールド実験を2021年9月～2022年2月で実施した。
#' 表\@ref(tab:assignment-schedule)に示されているように、我々は週単位で実験群を割り当てる。
#' このとき、月の固定効果と週の固定効果を取り除くために、
#' 実験群が月単位・各月の週単位でバランスされるように設計した。
#' --->
#'
#' ## 割り当てスケジュール
#'
#+ assignment-schedule
treat_value <- function(x) {
  x <- as.character(x)
  switch(x,
    "1" = "A",
    "2" = "B",
    "2" = "B",
    "3" = "C",
    "4" = "D"
  )
}

schedule %>%
  mutate(
    my = paste0(month, "/", year - 2000),
    my = factor(
      my,
      levels = c("9/21", "10/21", "11/21", "12/21", "1/22", "2/22")
    ),
    week = factor(week, levels = 1:4),
    treat = case_when(
      treat == "A" ~ 1,
      treat == "B" ~ 2,
      treat == "C" ~ 3,
      treat == "D" ~ 4
    )
  ) %>%
  datasummary(
    week ~ treat * treat_value * my,
    data = .,
    title = "Assignment Schedule",
    align = "ccccccc"
  ) %>%
  kableExtra::kable_styling() %>%
  kableExtra::add_header_above(c(
    " " = 1, "Month/Year" = 6
  )) %>%
  kableExtra::as_image(cliprect = c(40, 300, 400, 250))

#' 
#' <!---
#' 表\@ref(tab:overview-field-exp)にフィールド実験の概要を示した。
#'
#' - 実験期間中に合計11,154件の適合通知を送付し、
#' 実験群A・B・C・Dのサンプルサイズは
#' それぞれ2,559件、3,075件、2,754件、2,766件である
#' - 性別の比率・年齢は実験群間でバランスされているが、
#' コーディネーション回数はバランスされていない
#' ---->
#'
#' ## フィールド実験概要
#'
#+ overview-field-exp
balance_test <- use %>%
  select(male, age, coordinate, treat, week) %>%
  pivot_longer(male:coordinate, values_to = "value", names_to = "vars") %>%
  group_by(vars) %>%
  do(est = lm_robust(
    value ~ treat,
    clusters = week,
    se_type = "CR0",
    data = .
  )) %>%
  summarize(
    vars = vars,
    f = summary(est)$fstatistic[1],
    numdf = summary(est)$fstatistic[2],
    dendf = summary(est)$fstatistic[3],
    "p-value" = pf(f, numdf, dendf, lower.tail = FALSE)
  )

size <- with(use, sprintf("%1d", table(treat))) %>%
  {
    tribble(
      ~terms, ~A, ~B, ~C, ~D, ~"p-value",
      "Basic notification", "X", "X", "X", "X", "",
      "One chance in million", "", "X", "", "X", "",
      "Transplant recipient", "", "", "X", "X", "",
      "Number of observations", .[1], .[2], .[3], .[4], ""
    )
  }

attr(size, "position") <- 1:nrow(size)

note <- paste(
  "Note: p値はバランステストの結果である。",
  "バランステストは実験群を共変量に線形回帰したF検定を用いている。"
)

datasummary(
  (`Age` = age) +
  (`Number of coordinations` = coordinate) +
  (`1 = Male` = male) ~ mean * treat,
  data = use,
  title = "Overview of Field Experiment",
  add_rows = size,
  add_columns = balance_test[, 5],
  align = "lccccc"
) %>%
kableExtra::kable_styling() %>%
add_header_above(c(" " = 1, "Experimental Arms" = 4, " " = 1)) %>%
group_rows("A. Intervention", 1, 3) %>%
group_rows("B. Sample Size", 4, 4) %>%
group_rows("C. Covariate", 5, 7) %>%
kableExtra::as_image(cliprect = c(40, 250, 500, 410))
