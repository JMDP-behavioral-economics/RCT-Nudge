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
  mutate(treat = factor(treat, levels = LETTERS[1:4])) %>%
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

ttest_info <- expand.grid(
  outcome = out_lev,
  group1 = LETTERS[1:4],
  group2 = LETTERS[1:4],
  male = c(1, 0),
  stringsAsFactors = FALSE) %>%
  dplyr::filter(group1 < group2) %>%
  arrange(outcome) %>%
  mutate(id = seq(n()))


#+
outcomes <- use %>%
  select(id, male, treat, reply, intention:donate) %>%
  pivot_longer(reply:donate, names_to = "outcome")

exclude <- use %>%
  select(id, starts_with("exg_stop")) %>%
  pivot_longer(
    -id, names_to = "outcome", values_to = "exclude",
    names_prefix = "exg_stop_"
  )

testdt <- outcomes %>%
  dplyr::left_join(exclude, by = c("id", "outcome"))

#+
stat <- testdt %>%
  dplyr::filter(exclude == 0) %>%
  group_by(outcome, male, treat) %>%
  summarize(mean = mean(value), se = se(value)) %>%
  ungroup() %>%
  mutate(
    lwr.mean = mean - se,
    upr.mean = mean + se,
    outcome = factor(outcome, out_lev, out_lab)
  )

ttest <- ttest_info %>%
  group_by(id) %>%
  do(test = t.test(
    subset(
      testdt,
      treat == .$group1 & exclude == 0 &
      outcome == .$outcome & male == .$male
    )$value,
    subset(
      testdt,
      treat == .$group2 & exclude == 0 &
      outcome == .$outcome & male == .$male
    )$value
  )) %>%
  summarize(
    id = id,
    p = test$p.value,
    est1 = test$estimate[1],
    est2 = test$estimate[2]
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
    male = .$male,
    p = .$p,
    y_g1 = subset(
      stat, outcome == .$outcome & treat == .$group1 & male == .$male
    )$upr.mean,
    y_g2 = subset(
      stat, outcome == .$outcome & treat == .$group2 & male == .$male
    )$upr.mean
  )) %>%
  ungroup() %>%
  # arrange(male) %>%
  group_by(outcome, male) %>%
  mutate(y = if_else(y_g1 > y_g2, y_g1, y_g2) + 0.02) %>%
  mutate(y = max(y)) %>%
  ungroup() %>%
  mutate(
    y = if_else(outcome %in% out_lab[4:6], y - 0.015, y),
    y = if_else(id == 47 | id == 70, y + 0.05, y),
    label = case_when(
      p < 0.01 ~ "p < 0.01",
      TRUE ~ sprintf("p = %1.3f", p)
    )
  )

#+ ttest-1-3step-male-second, fig.cap = "Average of Outcomes before Donor Candidate Selection among Males"
stat %>%
  dplyr::filter(male == 1) %>%
  dplyr::filter(outcome %in% out_lab[1:3]) %>%
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
      vjust = 5, size = 5
    ) +
    geom_signif(
      data = subset(
        show_ttest_info, outcome %in% out_lab[1:3] & male == 1
      ),
      aes(xmin = group1, xmax = group2, annotations = label, y_position = y),
      textsize = 5, tip_length = 0.01,
      manual = TRUE
    ) +
    facet_wrap(~ outcome) +
    scale_y_continuous(breaks = seq(0, 1, 0.1)) +
    labs(
      x = "Experimental Arms",
      y = "Sample average"
    ) +
    simplegg(caption_size = 13)

#+ ttest-4-6step-male-second, fig.cap = "Average of Outcomes after Donor Candidate Selection among Males"
stat %>%
  dplyr::filter(male == 1) %>%
  dplyr::filter(outcome %in% out_lab[4:6]) %>%
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
      vjust = 8, size = 5
    ) +
    geom_signif(
      data = subset(
        show_ttest_info, outcome %in% out_lab[4:6] & male == 1
      ),
      aes(xmin = group1, xmax = group2, annotations = label, y_position = y),
      textsize = 5, tip_length = 0.01,
      manual = TRUE
    ) +
    facet_wrap(~ outcome) +
    scale_y_continuous(breaks = seq(0, 0.2, 0.02)) +
    labs(
      x = "Experimental Arms",
      y = "Sample average",
      fill = "Experimental Arms"
    ) +
    simplegg()

#+ ttest-1-3step-female-second, fig.cap = "Average of Outcomes before Donor Candidate Selection among Females"
stat %>%
  dplyr::filter(male == 0) %>%
  dplyr::filter(outcome %in% out_lab[1:3]) %>%
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
      vjust = 5, size = 5
    ) +
    geom_signif(
      data = subset(
        show_ttest_info, outcome %in% out_lab[1:3] & male == 0
      ),
      aes(xmin = group1, xmax = group2, annotations = label, y_position = y),
      textsize = 5, tip_length = 0.01,
      manual = TRUE
    ) +
    facet_wrap(~ outcome) +
    scale_y_continuous(breaks = seq(0, 1, 0.1)) +
    labs(
      x = "Experimental Arms",
      y = "Sample average"
    ) +
    simplegg(caption_size = 13)

#+ ttest-4-6step-female-second, fig.cap = "Average of Outcomes after Donor Candidate Selection among Females"
stat %>%
  dplyr::filter(male == 0) %>%
  dplyr::filter(outcome %in% out_lab[4:6]) %>%
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
      vjust = 12, size = 5
    ) +
    geom_signif(
      data = subset(
        show_ttest_info, outcome %in% out_lab[4:6] & male == 0
      ),
      aes(xmin = group1, xmax = group2, annotations = label, y_position = y),
      textsize = 5, tip_length = 0.01,
      manual = TRUE
    ) +
    facet_wrap(~ outcome) +
    scale_y_continuous(breaks = seq(0, 0.2, 0.02)) +
    labs(
      x = "Experimental Arms",
      y = "Sample average",
      fill = "Experimental Arms"
    ) +
    simplegg()
