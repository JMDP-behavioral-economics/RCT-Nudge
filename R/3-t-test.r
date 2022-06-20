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

#'
#' アウトカム変数は提供に至るまでのプロセスであり、具体的に以下の6つの工程がある。
#'
#' - Reply to notification: 適合通知に返信したならば1を取る二値変数
#' - Intention: 提供を希望するという意向を示して返信したならば1を取る二値変数
#' - Confirmatory typing: 確認検査を実施したならば1を取る二値変数
#' - Candidate: 第一候補者に選定されたならば1を取る二値変数
#' - Final consent: 最終同意をしたならば1を取る二値変数
#' - Donation: 採取をしたならば1を取る二値変数
#' 
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

#+
stat <- use %>%
  group_by(treat) %>%
  summarize_at(
    vars(reply, intention, test, candidate, consent, donate),
    list(
      mean =~mean(.),
      se =~se(.)
    )
  ) %>%
  pivot_longer(
    -treat,
    names_to = c("outcome", "stat"),
    names_pattern = "(.*)_(.*)"
  ) %>%
  pivot_wider(names_from = "stat", values_from = "value") %>%
  mutate(
    lwr.mean = mean - se,
    upr.mean = mean + se,
    outcome = factor(outcome, out_lev, out_lab)
  )

#+
ttest_info <- expand.grid(
  outcome = c(
    "reply", "intention", "test",
    "candidate", "consent", "donate"
  ),
  group1 = LETTERS[1:4],
  group2 = LETTERS[1:4],
  stringsAsFactors = FALSE) %>%
  dplyr::filter(group1 < group2) %>%
  arrange(outcome) %>%
  mutate(id = seq(n()))

ttest <- ttest_info %>%
  group_by(id) %>%
  do(test = t.test(
    use[use$treat == .$group1, .$outcome],
    use[use$treat == .$group2, .$outcome]
  )) %>%
  summarize(
    id = id,
    p = test$p.value
  )

show_ttest_info <- ttest_info %>%
  dplyr::left_join(ttest, by = "id") %>%
  dplyr::filter(p <= 0.1) %>%
  mutate_at(vars(group1, group2), list(~factor(., levels = LETTERS[1:4]))) %>%
  mutate(outcome = factor(outcome, out_lev, out_lab)) %>%
  group_by(id) %>%
  do(data.frame(
    outcome = .$outcome,
    group1 = .$group1,
    group2 = .$group2,
    p = .$p,
    y_g1 = subset(stat, outcome == .$outcome & treat == .$group1)$upr.mean,
    y_g2 = subset(stat, outcome == .$outcome & treat == .$group2)$upr.mean
  )) %>%
  ungroup() %>%
  mutate(
    y = if_else(y_g1 > y_g2, y_g1, y_g2) + 0.02,
    y = if_else(id == 21, y + 0.02, y),
    y = if_else(id == 34, y + 0.06, y),
    sign = case_when(
      p < 0.01 ~ "***",
      p < 0.05 ~ "**",
      p < 0.1 ~ "*",
      TRUE ~ "",
    )
  )

#'
#' 図\@ref(fig:ttest-1-3step)に第一候補者に選定される前のプロセスについての記述統計とt検定の結果を示した。
#'
#' - 介入は適合通知の返信率に影響を与えていない
#' - トリートメントB群は他の群と比較して意向を高めた可能性がある。
#'   - トリートメントB群の意向比率は57.5%である一方、
#'   コントロール群（A群）とトリートメントC群の意向比率はそれぞれ55.3%と53.9%である。
#'   B群とA群の差（2.2%ポイント）は統計的に10%有意であるが、
#'   B群とC群の差（2.6%ポイント）は統計的に5%水準で有意である。
#' - コントロールと比較して、B群とD群は確認検査の実施まで至った人の確率を高めている可能性がある。
#'   - B群とD群の確認検査の実施率はそれぞれ25.9%と25.5%である一方で、
#'   コントロール群の確認検査の実施率は22.9%である。
#'   B群とコントロール群の差（3%ポイント）、
#'   およびD群とコントロール群の差（2.6%ポイント）は統計的に5%水準で有意である。
#'
#+ ttest-1-3step, fig.cap = "Sample Average of Outcomes before Donor Candidate Selection"
stat %>%
  dplyr::filter(outcome %in% levels(stat$outcome)[1:3]) %>%
  ggplot(aes(x = treat, y = mean)) +
    geom_bar(
      color = "black",
      fill = "grey90",
      stat = "identity"
    ) +
    geom_errorbar(
      aes(ymin = lwr.mean, ymax = upr.mean),
      width = 0.5, position = position_dodge(0.9)
    ) +
    geom_text(
      aes(label = sprintf("%1.3f", mean)),
      vjust = 3, size = 5
    ) +
    geom_signif(
      data = show_ttest_info,
      aes(xmin = group1, xmax = group2, annotations = sign, y_position = y),
      textsize = 8, tip_length = 0.01,
      manual = TRUE
    ) +
    facet_wrap(~ outcome) +
    scale_y_continuous(breaks = seq(0, 1, 0.1)) +
    labs(
      x = "Experimental Arms",
      y = "Sample average",
      caption = "*** p < 0.01, ** p < 0.05, * p < 0.1"
    ) +
    simplegg(caption_size = 13)

#'
#' 図\@ref(fig:ttest-4-6step)は第一候補者選定後のプロセスについての記述統計とt検定の結果である。
#'
#' - いずれの介入も候補者選定・最終同意・提供に影響を与えていない
#' 
#+ ttest-4-6step, fig.cap = "Sample Average of Outcomes after Donor Candidate Selection"
stat %>%
  dplyr::filter(outcome %in% levels(stat$outcome)[4:6]) %>%
  ggplot(aes(x = treat, y = mean)) +
    geom_bar(
      color = "black",
      fill = "grey90",
      stat = "identity"
    ) +
    geom_errorbar(
      aes(ymin = lwr.mean, ymax = upr.mean),
      width = 0.5, position = position_dodge(0.9)
    ) +
    geom_text(
      aes(label = sprintf("%1.3f", mean)),
      vjust = 4, size = 5
    ) +
    facet_wrap(~ outcome) +
    scale_y_continuous(breaks = seq(0, 0.1, 0.01)) +
    labs(
      x = "Experimental Arms",
      y = "Sample average",
      fill = "Experimental Arms"
    ) +
    simplegg()

