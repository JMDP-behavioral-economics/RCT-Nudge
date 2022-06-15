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
    outcome = factor(
      outcome,
      levels = c(
        "reply", "intention", "test",
        "candidate", "consent", "donate"
      ),
      labels = c(
        "Reply to invitation",
        "Intention",
        "Confirmatory typing",
        "Candidate",
        "Final consent",
        "Donation"
      )
    )
  )

#+ fig.cap = "Sample Average of Outcomes before Donor Candidate Selection"
base_plot <- stat %>%
  dplyr::filter(outcome %in% levels(stat$outcome)[1:3]) %>%
  ggplot(aes(x = outcome, y = mean, group = treat)) +
    geom_bar(
      aes(fill = treat),
      color = "black", stat = "identity", position = "dodge"
    ) +
    geom_errorbar(
      aes(ymin = lwr.mean, ymax = upr.mean),
      width = 0.5, position = position_dodge(0.9)
    ) +
    scale_fill_grey(start = 1, end = 0.4) +
    labs(
      x = "Stage",
      y = "Sample average",
      fill = "Experimental Arms"
    ) +
    simplegg()


#+ fig.cap = "Sample Average of Outcomes after Donor Candidate Selection"
stat %>%
  dplyr::filter(outcome %in% levels(stat$outcome)[4:6]) %>%
  ggplot(aes(x = outcome, y = mean, group = treat)) +
    geom_bar(
      aes(fill = treat),
      color = "black", stat = "identity", position = "dodge"
    ) +
    geom_errorbar(
      aes(ymin = lwr.mean, ymax = upr.mean),
      width = 0.5, position = position_dodge(0.9)
    ) +
    scale_fill_grey(start = 1, end = 0.4) +
    labs(
      x = "Stage",
      y = "Sample average",
      fill = "Experimental Arms"
    ) +
    simplegg()

