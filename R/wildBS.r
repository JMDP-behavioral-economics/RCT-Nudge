library(here)
library(tidyverse)
library(lubridate)
library(estimatr)
library(Rcpp)

outcome_label <- list(
  reply = "Reply",
  positive = "Positive intention",
  negative = "Negative intention",
  test = "CT",
  candidate = "Candidate",
  consent = "Consent",
  donate = "Donation"
)

root <- "D:/JMDPフィールド実験"

rawdt <- read_csv(
  here(root, "shaped.csv"),
  locale = locale(encoding = "cp932")
)

use <- rawdt %>%
  mutate(treat = factor(treat, levels = LETTERS[1:4])) %>%
  dplyr::filter(ongoing == 0 & prefecture != "海外") %>%
  dplyr::filter(exg_stop_reply == 0) %>%
  rename(positive = intention) %>%
  mutate(
    negative = reply * (1 - positive),
    age_demean = age - mean(rawdt$age),
  ) %>%
  select(reply, positive, negative, everything()) %>%
  pivot_longer(reply:negative, "outcome") %>%
  mutate(outcome = factor(
    outcome,
    levels = unlist(names(outcome_label)[1:3]),
    labels = unlist(outcome_label[1:3])
  ))

mod <- value ~ treat + age_demean + male + coordinate +
  hospital_per_area + PB_per_area + BM_per_area +
  factor(month) + factor(week)

func_ols <- lm_robust(
  mod,
  data = subset(use, outcome == "Reply"),
  cluster = RCTweek,
  se_type = "stata"
)

summary(func_ols)

sourceCpp(here("cpp", "wildBS.cpp"))

estdt <- model.frame(mod, data = use, subset = outcome == "Reply", cluster = RCTweek)
x <- model.matrix(mod, estdt)
y <- estdt[, "value"]
g <- as.integer(estdt[, "(cluster)"])

runOLS(x, y, g)
coef(func_ols)
