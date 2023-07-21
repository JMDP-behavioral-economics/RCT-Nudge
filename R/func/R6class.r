library(R6)
library(tidyverse)

RCT <- R6Class("RCT", list(
  data = NULL,
  label = list(
    reply = "Reply",
    positive = "Positive intention",
    negative = "Negative intention",
    test = "CT",
    candidate = "Candidate",
    consent = "Consent",
    donate = "Donation"
  ),
  intervention = list(),
  covariate = c(),
  size = NULL,
  initialize = function(data) {
    self$data <- data %>%
      mutate(
        treat = factor(treat, levels = LETTERS[1:4]),
        age_less30 = if_else(age < 30, 1, 0),
        age_demean = age - mean(data$age)
      )

    self$size <- self$data %>%
      group_by(treat) %>%
      summarize(n = n()) %>%
      pivot_wider(names_from = treat, values_from = n) %>%
      mutate_all(list(~ sprintf("%1d", .)))
  },
  print = function() View(self$data),
  add_intervention = function(label, assignment) {
    self$intervention <- append(self$intervention, assignment)
    i <- length(self$intervention)
    names(self$intervention)[i] <- label
    print(self$intervention)
  },
  add_covariate = function(add) {
    self$covariate <- append(self$covariate, add)
    print(self$covariate)
  },
  summary_experiment = function(is_cluster = params$is_cluster, se = params$se) {
    BalanceTest$new(
      self$intervention,
      self$size,
      self$covariate,
      is_cluster,
      se,
      self$data
    )
  }
))

BalanceTest <- R6Class("BalanceTest", list(
  table = NULL,
  initialize = function(intervention, size_table, covariate, is_cluster, se, data) {
    panelA <- reduce(intervention, bind_rows)
    panelA <- bind_cols(term = names(intervention), panelA)
    panelA <- bind_cols(panelA, p.value = "")
    panelA <- bind_cols(panelA, panel = "A. Interventions")

    panelB <- size_table
    panelB <- bind_cols(term = "N", panelB)
    panelB <- bind_cols(panelB, panel = "B. Sample Size")

    panelC <- data %>%
      select(!!!covariate, treat) %>%
      group_by(treat) %>%
      summarize_all(mean) %>%
      pivot_longer(-treat, names_to = "term") %>%
      pivot_wider(names_from = treat, values_from = value) %>%
      mutate_at(vars(-term), list(~ sprintf("%1.3f", .)))
    
    p <- sapply(covariate, function(y) {
      mod <- reformulate("treat", y)
      est <- lm_robust(
        mod,
        data = use,
        clusters = if (is_cluster) use$RCTweek,
        se_type = se
      )
      f <- summary(est)$fstatistic
      sprintf("%1.3f", pf(f[1], f[2], f[3], lower.tail = FALSE))
    })
    ptab <- tibble(term = names(p), p.value = p)

    panelC <- panelC %>%
      dplyr::left_join(ptab, by = "term")
    panelC <- bind_cols(panelC, panel = "C. Balance Test")

    self$table <- bind_rows(panelA, panelB) %>%
      bind_rows(panelC) %>%
      mutate_all(list(~ ifelse(is.na(.), "", .)))
  },
  print = function() print(self$table),
  flextable = function(notes = character(0), fontsize = 9) {
    self$table %>%
      as_grouped_data("panel") %>%
      as_flextable(hide_grouplabel = TRUE) %>%
      set_header_labels(term = "", p.value = "F-test, p-value") %>%
      add_header_row(values = c("", "Experimental Arms", ""), colwidths = c(1, 4, 1)) %>%
      add_footer_lines(notes) %>%
      align(j = -1, align = "center", part = "all") %>%
      padding(j = 1:6, padding.top = 5, padding.bottom = 5, part = "all") %>%
      padding(2:4, padding.left = 10) %>%
      padding(6, padding.left = 10) %>%
      padding(8:13, padding.left = 10) %>%
      width(j = 1, 2) %>%
      fontsize(size = fontsize, part = "all") %>%
      ft_theme()
  },
  kable = function(notes = "", fontsize = 9) {
    tab <- self$table

    label <- colnames(tab)
    label <- label[1:length(label) - 1]
    label[label == "term"] <- ""
    label[label == "p.value"] <- "F-test, p-value"

    struct <- rle(tab$panel)
    struct$lengths <- cumsum(struct$lengths)

    tab %>%
      select(-panel) %>%
      knitr::kable(
        col.names = label,
        align = paste(c("l", rep("c", length(label))), collapse = ""),
        booktabs = TRUE,
        linesep = ""
      ) %>%
      kable_styling(font_size = fontsize) %>%
      add_header_above(c(" " = 1, "Experimental Arms" = length(label) - 2, " " = 1)) %>%
      group_rows(struct$values[1], 1, struct$lengths[1]) %>%
      group_rows(struct$values[2], struct$lengths[1] + 1, struct$lengths[2]) %>%
      group_rows(struct$values[3], struct$lengths[2] + 1, struct$lengths[3]) %>%
      kableExtra::footnote(
        general = notes,
        general_title = ""
      )
  }
))

# use <- subset(rawdt, ongoing == 0)
# setup <- RCT$new(use)

# setup$add_intervention("Standard notification", list(c(A = "X", B = "X", C = "X", D = "X")))
# setup$add_intervention("Probability message", list(c(A = "", B = "X", C = "", D = "X")))
# setup$add_intervention("Early Coordination message", list(c(A = "", B = "", C = "X", D = "X")))

# covs <- c(
#   "Male (= 1)" = "male",
#   "Age" = "age",
#   "Number of past coordinations" = "coordinate",
#   "Number of listed hospitals" = "hospital_per_area",
#   "Number of hospitals listed with PBSC collection" = "PB_per_area",
#   "Number of hospitals listed with BM collection" = "BM_per_area"
# )
# setup$add_covariate(covs)

# setup$
#   summary_experiment(is_cluster = FALSE, se = "stata")$
#   flextable()