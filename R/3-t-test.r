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

#+ fig.cap = "Sample Average of Outcomes before Donor Candidate Selection"
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

#+ fig.cap = "Sample Average of Outcomes after Donor Candidate Selection"
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

