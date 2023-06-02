library(here)
library(tidyverse)
library(flextable)

path <- here("D:/JMDPフィールド実験", "RCT-schedule.csv")

schedule <- read_csv(path) %>%
  mutate(
    my = paste0(month, "/", year - 2000),
    my = factor(
      my,
      levels = c("9/21", "10/21", "11/21", "12/21", "1/22", "2/22"),
      labels = c("Sep 21", "Oct 21", "Nov 21", "Dec 21", "Jan 22", "Feb 22")
    ),
    week = factor(week, levels = 1:4, labels = paste("Week", 1:4))
  ) %>%
  dplyr::select(week, my, treat) %>%
  pivot_wider(values_from = treat, names_from = my)

flextable(schedule) %>%
  align(j = -1, align = "center", part = "all") %>%
  padding(j = 1:7, padding.top = 5, padding.bottom = 5, part = "all") %>%
  fontsize(size = 9, part = "all") %>%
  ft_theme()
