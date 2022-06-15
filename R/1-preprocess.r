#+ include = FALSE
library(here)
source(here("R", "_library.r"))

#+ include = FALSE
root <- "D:/JMDPフィールド実験"
raw <- "raw.csv"
schedule <- "schedule.csv"

#+ include = FALSE
rawdt <- read_csv(
  here(root, "original", raw),
  locale = locale(encoding = "cp932")
)

schedule_dt <- read_csv(
  here(root, "original", schedule),
  locale = locale(encoding = "cp932")
)