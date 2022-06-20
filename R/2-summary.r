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

#+
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
    title = "Assignment of Treatments",
    align = "ccccccc"
  ) %>%
  kableExtra::kable_styling() %>%
  kableExtra::add_header_above(c(
    " " = 1, "Month/Year" = 6
  ))

#+
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
      "Basic invitation", "X", "X", "X", "X", "",
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
group_rows("Intervention: Contents of Invitation", 1, 3) %>%
group_rows("Sample Size", 4, 4) %>%
group_rows("Covariate", 5, 7) %>%
kableExtra::footnote(note, general_title = "")
