#' ---
#' title: Random Causal Forest (Reply with Negative Intention)
#' ---
#'
#+ library-packages, include = FALSE
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
  mutate(treat = factor(treat, levels = LETTERS[1:4])) %>%
  dplyr::filter(ongoing == 0 & prefecture != "海外") %>%
  dplyr::filter(exg_stop_reply == 0) %>%
  rename(positive = intention) %>%
  mutate(negative = reply * (1 - positive))

#+ rcf-negative-run, include = FALSE
covariate <- ~ 0 + male + age + coordinate + hospital_per_area +
  PB_per_area + BM_per_area + prefecture

Y <- use$negative
X <- model.matrix(covariate, data = use)
W <- use$treat
G <- use$RCTweek

rcf <- multi_arm_causal_forest(X, Y, W)
tau <- predict(rcf, X)

#+ rcf-negative-data, include = FALSE
age_group <- list(
  "Age: 20-29" = X[, 2] < 30,
  "Age: 30-39" = 30 <= X[, 2] & X[, 2] < 40,
  "Age: over 40" = 40 < X[, 2]
)

df <- data.frame(X) %>%
  cbind(tau$predictions) %>%
  rename(
    effect_B = `B - A.Y.1`,
    effect_C = `C - A.Y.1`,
    effect_D = `D - A.Y.1`
  ) %>%
  pivot_longer(
    effect_B:effect_D,
    names_to = c(".value", "treat"),
    names_sep = "_"
  ) %>%
  mutate(treat = factor(treat, labels = paste("Message", LETTERS[2:4]))) %>%
  mutate(age_group = case_when(
    age < 30 ~ names(age_group)[1],
    age < 40 ~ names(age_group)[2],
    TRUE ~ names(age_group)[3]
  ))

#+ rcf-negative-importance
vi <- data.frame(names = colnames(X), idx = variable_importance(rcf)) %>%
  arrange(desc(idx)) %>%
  head(10) %>%
  mutate(names = recode(
    names,
    "age" = "Age",
    "BM_per_area" = "BM hospital (per 10 square kilometers)",
    "PB_per_area" = "PB hospital (per 10 square kilometers)",
    "hospital_per_area" = "Hospital (per 10 square kilometers)",
    "male" = "1 = Male",
    "coordinate" = "Number of past coordinations",
    "prefecture神奈川県" = "1 = Kanagawa",
    "prefecture大阪府" = "1 = Osaka",
    "prefecture東京都" = "1 = Tokyo",
    "prefecture埼玉県" = "1 = Saitama"
  ))

ggplot(vi, aes(x = reorder(names, idx), y = idx)) +
  geom_bar(stat = "identity") +
  labs(x = "Top 10 Variables", y = "Variable importance") +
  coord_flip() +
  simplegg(flip = TRUE)

#+ rcf-negative-dist, fig.width = 10, fig.cap = "Heterogenous Message Effects on Negative Intention."
ate <- average_treatment_effect(rcf) %>%
  data.frame() %>%
  mutate(treat = str_extract(contrast, "[B-D]")) %>%
  mutate(treat = factor(treat, labels = paste("Message", treat))) %>%
  mutate(p = pnorm(abs(estimate / std.err), lower.tail = FALSE) * 2) %>%
  mutate(summary = sprintf(
    "ATE = %1.3f \n(std.err = %1.3f; p-value = %1.3f)",
    estimate, std.err, p
  ))

ggplot(df, aes(x = effect, y = ..count.. / sum(..count..))) +
  geom_histogram(
    aes(fill = effect > 0),
    breaks = seq(-.2, .25, by = .01),
    color = "black"
  ) +
  geom_segment(
    aes(x = estimate, xend = estimate, y = 0, yend = 0.065),
    linetype = 2, data = ate
  ) +
  geom_text(
    aes(x = estimate, y = 0.07, label = summary),
    size = 4, data = ate
  ) +
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0, 0.075),
    breaks = seq(0, 0.06, 0.01)
  ) +
  scale_fill_manual(values = c("white", "red")) +
  facet_grid(~ treat) +
  labs(
    x = "Treatment effects",
    y = "Fraction"
  ) +
  simplegg() +
  theme(legend.position = "none")

#+ rcf-negative-male-dist, fig.width = 10, fig.height = 10, fig.cap = "Heterogenous Message Effects on Negative Intentions among Males."
ate_male <- lapply(seq_along(age_group), function(i) {
  average_treatment_effect(rcf, subset = X[, 1] == 1 & age_group[[i]]) %>%
    mutate(age_group = names(age_group)[i]) %>%
    mutate(treat = str_extract(contrast, "[B-D]")) %>%
    mutate(treat = factor(treat, labels = paste("Message", treat))) %>%
    select(-contrast, -outcome) %>%
    mutate(p = pnorm(abs(estimate / std.err), lower.tail = FALSE) * 2) %>%
    mutate(summary = sprintf(
      "ATE = %1.3f \n(std.err = %1.3f; p-value = %1.3f)",
      estimate, std.err, p
    ))
  }) %>%
  reduce(bind_rows)

df %>%
  dplyr::filter(male == 1) %>%
  ggplot(aes(x = effect, y = ..count.. / sum(..count..))) +
  geom_histogram(
    aes(fill = effect > 0),
    breaks = seq(-.25, .25, by = .01),
    color = "black"
  ) +
  geom_segment(
    aes(x = estimate, xend = estimate, y = 0, yend = 0.04),
    linetype = 2, data = ate_male
  ) +
  geom_text(
    aes(x = estimate, y = 0.045, label = summary),
    size = 4, data = ate_male
  ) +
  scale_fill_manual(values = c("white", "red")) +
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0, 0.05),
    breaks = seq(0, 0.04, 0.01)
  ) +
  facet_grid(age_group ~ treat) +
  labs(x = "Treatment Effects", y = "Fraction") +
  simplegg() +
  theme(legend.position = "none")

#+ rcf-negative-female-dist, fig.width = 10, fig.height = 10, fig.cap = "Heterogeneous Message Effects on Negative Intention among Females."
ate_female <- lapply(seq_along(age_group), function(i) {
  average_treatment_effect(rcf, subset = X[, 1] == 0 & age_group[[i]]) %>%
    mutate(age_group = names(age_group)[i]) %>%
    mutate(treat = str_extract(contrast, "[B-D]")) %>%
    mutate(treat = factor(treat, labels = paste("Message", treat))) %>%
    select(-contrast, -outcome) %>%
    mutate(p = pnorm(abs(estimate / std.err), lower.tail = FALSE) * 2) %>%
    mutate(summary = sprintf(
      "ATE = %1.3f \n(std.err = %1.3f; p-value = %1.3f)",
      estimate, std.err, p
    ))
  }) %>%
  reduce(bind_rows)

df %>%
  dplyr::filter(male == 0) %>%
  ggplot(aes(x = effect, y = ..count.. / sum(..count..))) +
  geom_histogram(
    aes(fill = effect > 0),
    breaks = seq(-.25, .25, by = .01),
    color = "black"
  ) +
  geom_segment(
    aes(x = estimate, xend = estimate, y = 0, yend = 0.04),
    linetype = 2, data = ate_female
  ) +
  geom_text(
    aes(x = estimate, y = 0.045, label = summary),
    size = 4, data = ate_female
  ) +
  scale_fill_manual(values = c("white", "red")) +
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0, 0.05),
    breaks = seq(0, 0.04, 0.01)
  ) +
  facet_grid(age_group ~ treat) +
  labs(x = "Treatment Effects", y = "Fraction") +
  simplegg() +
  theme(legend.position = "none")

#+ rcf-negative-male-character
df %>%
  dplyr::filter(male == 1) %>%
  mutate(positive = if_else(effect > 0, "Positive", "Negative")) %>%
  datasummary(
    title = "Mean Characteristics of Males whose Effect is Negative/Positive",
    age_group * (
      (`コーディネーション回数` = coordinate) +
      (`面談施設（10平方キロメートル当たり）` = hospital_per_area) +
      (`PB採取可能施設（10平方キロメートル当たり）` = PB_per_area) +
      (`BM採取可能施設（10平方キロメートル当たり）` = BM_per_area) +
      (`神奈川県` = prefecture神奈川県) +
      (`大阪府` = prefecture大阪府) +
      (`東京都` = prefecture東京都) +
      (`埼玉県` = prefecture埼玉県)
    ) ~ mean * treat * positive,
    data = .,
    fmt = 3
  )

#+ rcf-negative-female-character
df %>%
  dplyr::filter(male == 0) %>%
  mutate(positive = if_else(effect > 0, "Positive", "Negative")) %>%
  datasummary(
    title = "Mean Characteristics of Females whose Effect is Negative/Positive",
    age_group * (
      (`コーディネーション回数` = coordinate) +
      (`面談施設（10平方キロメートル当たり）` = hospital_per_area) +
      (`PB採取可能施設（10平方キロメートル当たり）` = PB_per_area) +
      (`BM採取可能施設（10平方キロメートル当たり）` = BM_per_area) +
      (`神奈川県` = prefecture神奈川県) +
      (`大阪府` = prefecture大阪府) +
      (`東京都` = prefecture東京都) +
      (`埼玉県` = prefecture埼玉県)
    ) ~ mean * treat * positive,
    data = .,
    fmt = 3
  )
