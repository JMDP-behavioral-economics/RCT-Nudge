library(tidyverse)
library(lubridate)
library(estimatr)
library(fixest)
library(modelsummary)
library(kableExtra)
library(RCTtoolbox)
library(ggsignif)

lapply(
  Sys.glob(here("R/func", "*.r")),
  source,
  encoding = "UTF-8"
)
