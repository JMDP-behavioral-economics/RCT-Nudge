library(R6)
library(tidyverse)
source("R/misc.r")

BalanceTest <- R6::R6Class("BalanceTest",
  public = list(
    initialize = function(data,
                          covariate,
                          intervention,
                          se,
                          cluster = NULL) 
    {
      dt <- data %>%
        select(
          treat,
          "Male (= 1)" = male,
          "Age" = age,
          "Number of past coordination" = coordinate,
          "Number of listed hospitals" = hospital_per_area,
          "Number of hospitals listed with PBSC collection" = PB_per_area,
          "Number of hospitals listed with BM collection" = BM_per_area,
          "Number of holidays in the assigned week" = holidays,
          "Skipped the CT (= 1)" = skip_test
        )

      panelA <- reduce(intervention, bind_rows)
      panelA <- bind_cols(term = names(intervention), panelA)
      panelA <- bind_cols(panelA, p.value = "")
      panelA <- bind_cols(panelA, panel = "A. Interventions")

      panelB <- dt %>%
        group_by(treat) %>%
        summarize(n = n()) %>%
        pivot_wider(names_from = treat, values_from = n) %>%
        mutate_all(list(~ sprintf("%1d", .)))
      panelB <- bind_cols(term = "N", panelB)
      panelB <- bind_cols(panelB, panel = "B. Sample Size")

      panelC <- dt %>%
        group_by(treat) %>%
        summarize_all(mean) %>%
        pivot_longer(-treat, names_to = "term") %>%
        pivot_wider(names_from = treat, values_from = value) %>%
        mutate_at(vars(-term), list(~ sprintf("%1.3f", .)))

      args <- list(formula = value ~ treat, se_type = se)
      if (!is.null(cluster)) {
        g <- self$data[, cluster, drop = TRUE]
        args <- append(args, list(cluster = g))
      }

      p <- sapply(names(dt)[-1], function(y) {
        d <- select(dt, treat, value = all_of(y))
        args <- append(args, list(data = d))
        est <- do.call(lm_robust, args)
        f <- summary(est)$fstatistic
        sprintf("%1.3f", pf(f[1], f[2], f[3], lower.tail = FALSE))
      })
      ptab <- tibble(term = names(p), p.value = p)

      panelC <- panelC %>%
        dplyr::left_join(ptab, by = "term")
      panelC <- bind_cols(panelC, panel = "C. Balance Test")

      private$table <- bind_rows(panelA, panelB) %>%
        bind_rows(panelC) %>%
        mutate_all(list(~ ifelse(is.na(.), "", .)))
    },
    print = function() private$table,
    flextable = function(title = "", notes = "", font_size = 9) {
      tab <- private$table
      panel_len <- rle(tab$panel)$lengths
      start_pos <- c(2, panel_len[1] + 2 + 1, sum(panel_len[1:2]) + 3 + 1)

      tab %>%
        as_grouped_data("panel") %>%
        as_flextable(hide_grouplabel = TRUE) %>%
        set_caption(title) %>%
        set_header_labels(term = "", p.value = "F-test, p-value") %>%
        add_header_row(
          values = c("", "Experimental Arms", ""),
          colwidths = c(1, ncol(tab) - 3, 1)
        ) %>%
        add_footer_lines(notes) %>%
        align(j = -1, align = "center", part = "all") %>%
        padding(padding.top = 5, padding.bottom = 5, part = "all") %>%
        padding(seq(start_pos[1], length.out = panel_len[1]), padding.left = 10) %>%
        padding(seq(start_pos[2], length.out = panel_len[2]), padding.left = 10) %>%
        padding(seq(start_pos[3], length.out = panel_len[3]), padding.left = 10) %>%
        width(j = 1, 2) %>%
        fontsize(size = font_size, part = "all") %>%
        ft_theme()
    },
    kable = function(title = "", notes = "", font_size = 9, hold = FALSE) {
      tab <- private$table

      label <- colnames(tab)
      label <- label[1:length(label) - 1]
      label[label == "term"] <- ""
      label[label == "p.value"] <- "F-test, p-value"

      struct <- rle(tab$panel)
      struct$lengths <- cumsum(struct$lengths)

      kbl <- tab %>%
        select(-panel) %>%
        knitr::kable(
          caption = title,
          col.names = label,
          align = paste(c("l", rep("c", length(label))), collapse = ""),
          booktabs = TRUE,
          linesep = ""
        )

      if (hold) {
        kbl <- kbl %>% kable_styling(font_size = font_size, latex_options = "HOLD_position")
      } else {
        kbl <- kbl %>% kable_styling(font_size = font_size)
      }

      kbl %>%
        add_header_above(c(" " = 1, "Experimental Arms" = length(label) - 2, " " = 1)) %>%
        group_rows(struct$values[1], 1, struct$lengths[1]) %>%
        group_rows(struct$values[2], struct$lengths[1] + 1, struct$lengths[2]) %>%
        group_rows(struct$values[3], struct$lengths[2] + 1, struct$lengths[3]) %>%
        kableExtra::footnote(
          general_title = "",
          general = notes,
          threeparttable = TRUE,
          escape = FALSE
        )
    }
  ),
  private = list(
    table = NULL
  )
)

SmdBalanceTest <- R6::R6Class("SmdBalanceTest",
  public = list(
    initialize = function(data, covariate) {
      statistics <- data %>%
        select(
          "Male (= 1)" = male,
          "Age" = age,
          "Number of past coordination" = coordinate,
          "Number of listed hospitals" = hospital_per_area,
          "Number of hospitals listed with PBSC collection" = PB_per_area,
          "Number of hospitals listed with BM collection" = BM_per_area,
          "Number of holidays in the assigned week" = holidays,
          "Skipped the CT (= 1)" = skip_test,
          treat
        ) %>%
        pivot_longer(-treat, names_to = "vars") %>%
        group_by(treat, vars) %>%
        summarize(
          mean = mean(value),
          var = var(value)
        ) %>%
        ungroup()

      ctrl_stats <- statistics %>%
        filter(treat == levels(statistics$treat)[1]) %>%
        rename(mean_c = mean, var_c = var) %>%
        select(-treat)

      tbl <- statistics %>%
        filter(treat != levels(statistics$treat)[1]) %>%
        left_join(ctrl_stats, by = "vars") %>%
        mutate(d = (mean - mean_c) / sqrt((var + var_c) / 2)) %>%
        select(treat, vars, d) %>%
        pivot_wider(names_from = treat, values_from = d)

      private$stats <- statistics
      private$tbl <- tbl
    },
    get_table = function() private$tbl,
    kable = function( title = "",
                      notes = "",
                      font_size = 9,
                      digits = 3,
                      hold = FALSE)
    {
      tbl <- private$tbl
      kbl <- tbl %>%
        knitr::kable(
          caption = title,
          digits = digits,
          col.names = c("", paste0("(", seq(ncol(tbl) - 1), ")")),
          align = paste(c("l", rep("c", ncol(tbl) - 1)), collapse = ""),
          booktabs = TRUE,
          linesep = ""
        )

      if (hold) {
        kbl <- kbl %>%
          kable_styling(font_size = font_size, latex_options = "HOLD_position")
      } else {
        kbl <- kbl %>%
          kable_styling(font_size = font_size)
      }

      kbl <- kbl %>%
        kableExtra::add_header_above(c(" ", names(tbl)[-1]))

      header <- c(1, ncol(tbl) - 1)
      names(header) <- c(" ", paste(levels(private$stats$treat)[1], "versus"))

      kbl <- kbl %>%
        kableExtra::add_header_above(header)

      kbl %>%
        kableExtra::footnote(
          general_title = "",
          general = notes,
          threeparttable = TRUE,
          escape = FALSE
        )
    }
  ),
  private = list(
    stats = NULL,
    tbl = NULL
  )
)