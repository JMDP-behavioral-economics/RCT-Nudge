#' ---
#' title: About Field Experiment
#' subtitle: assignment schedule
#' ---
#'
#+ load-packages, include = FALSE
library(here)
source(here("R", "_library.r"))
source(here("R", "_outcome_labels.r"))

#+ load-data, include = FALSE
root <- "D:/JMDPフィールド実験"
schedule <- read_csv(here(root, "RCT-schedule.csv"))

#+ assignment-schedule
treat_value <- function(x) {
  x <- as.character(x)
  switch(x, '1' = 'A', '2' = 'B', '3' = 'C', '4' = 'D')
}

schedule %>%
  mutate(
    my = paste0(month, "/", year - 2000),
    my = factor(
      my,
      levels = c("9/21", "10/21", "11/21", "12/21", "1/22", "2/22"),
      labels = c("Sep 21", "Oct 21", "Nov 21", "Dec 21", "Jan 22", "Feb 22")
    ),
    week = factor(week, levels = 1:4, labels = paste("Week", 1:4)),
    treat = case_when(
      treat == "A" ~ 1,
      treat == "B" ~ 2,
      treat == "C" ~ 3,
      treat == "D" ~ 4
    )
  ) %>%
  datasummary(
    (` ` = week) ~ treat * treat_value * my,
    data = .,
    title = "Assignment Schedule",
    align = "lcccccc"
  ) %>%
  kableExtra::kable_styling() %>%
  kableExtra::add_header_above(c(" " = 1, "Month, Year" = 6))
