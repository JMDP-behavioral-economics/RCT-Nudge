library(R6)

StockData <- R6Class("StockData", list(
  data = NULL,
  initialize = function(data, label) {
    self$data <- data %>%
      dplyr::filter(exg_stop_reply == 0) %>%
      rename(positive = intention) %>%
      mutate(
        negative = reply * (1 - positive),
        age_demean = age - mean(rawdt$age),
      ) %>%
      select(reply, positive, negative, everything()) %>%
      pivot_longer(reply:negative, names_to = "outcome") %>%
      mutate(outcome = factor(
        outcome,
        levels = unlist(names(label)[1:3]),
        labels = unlist(label[1:3])
      ))
  },
  print = function() {
    View(self$data)
  },
  lm_all = function(is_cluster = params$is_cluster, se = params$se, is_fe = params$is_fe) {
    RegStock$new(self$data, is_cluster, se, is_fe)
  },
  lm_subset = function(is_cluster = params$is_cluster, se = params$se, is_fe = params$is_fe) {
    RegStockSubset$new(self$data, is_cluster, se, is_fe)
  }
))

RegStock <- R6Class("RegStock", list(
  reg = NULL,
  ctrl_avg = NULL,
  initialize = function(data, is_cluster, se, is_fe) {
    mod <- estimation_model(is_fe = is_fe)

    est_stock <- data %>%
      group_by(outcome) %>%
      nest() %>%
      mutate(
        fit1 = map_lm_robust(data, mod$unctrl, is_cluster, se),
        fit2 = map_lm_robust(data, mod$ctrl, is_cluster, se),
        avg = map_chr(data, ~ with(subset(., treat == "A"), sprintf("%.4f", mean(value))))
      ) %>%
      pivot_longer(
        fit1:fit2,
        names_prefix = "fit",
        names_to = "model",
        values_to = "fit"
      ) %>%
      select(-data)

    self$reg <- est_stock %>%
      pull(fit) %>%
      setNames(paste0("(", seq_len(length(.)), ")"))
    
    self$ctrl_avg <- est_stock$avg
  },
  modelsummary = function() {
    add_table <- c("Control average", self$ctrl_avg) %>%
      rbind(c("Covariates", "", "X", "", "X", "", "X")) %>%
      data.frame()

    attr(add_table, "position") <- 7:8

    tbl <- self$reg %>%
      modelsummary(
        coef_map = c(
          "treatB" = "Treatment B",
          "treatC" = "Treatment C",
          "treatD" = "Treatment D"
        ),
        stars = c("***" = .01, "**" = .05, "*" = .1),
        fmt = 4,
        gof_omit = "R2|AIC|BIC|Log|Std|FE|se_type",
        add_rows = add_table
      )
    
    tbl %>%
      add_header_row(
        values = c("", "Reply", "Positive", "Negative"),
        colwidths = c(1, 2, 2, 2)
      ) %>%
      add_header_row(
        values = c("", "Intention"),
        colwidths = c(3, 4)
      ) %>%
      align(j = -1, align = "center", part = "all") %>%
      add_footer_lines(paste(
        "Notes: * p < 0.1, ** p < 0.05, *** p < 0.01.",
        "The robust standard errors",
        "are reported in parentheses.",
        "Covariates are gender, squared polynomial of (demeaned) age,",
        "number of past coordinations,",
        "number of hospitals per 10 square kilometers,",
        "number of hospitals with PBSC collection per 10 square kilometers,",
        "number of hospitals with BM collection per 10 square kilometers,",
        "month dummies, and week dummies."
      )) %>%
      width(j = 1, 1) %>%
      fontsize(size = 9, part = "all") %>%
      ft_theme()
  }
))

RegStockSubset <- R6Class("RegStcokSubset", list(
  data = NULL,
  text = NULL,
  initialize = function(data, is_cluster, se, is_fe) {
    mod <- estimation_model(is_fe = is_fe)

    est_stock_sub <- data %>%
      group_by(outcome, male, age_less30) %>%
      nest() %>%
      mutate(est = map_lm_robust(
        data,
        update(mod$ctrl, . ~ . - male - age_demean),
        is_cluster,
        se
      ))

    self$data <- est_stock_sub %>%
      mutate(
        fit = map(est, tidy),
        fit = map(fit, ~ subset(.x, str_detect(term, "treat"))),
        fit = map(fit, ~ dplyr::select(.x, -outcome)),
        N = map_chr(est, ~ paste("N =", nobs(.x))),
        mean = map_dbl(data, ~ with(subset(., treat == "A"), mean(value))),
        mean = sprintf("Control avg = %1.3f", mean)
      ) %>%
      select(-data, -est) %>%
      unnest(cols = fit) %>%
      mutate(
        pos = paste0(male, age_less30),
        pos = factor(
          pos,
          levels = c("01", "00", "11", "10"),
          labels = c(
            "Female\u00d7\nAge<30",
            "Female\u00d7\n30\u2264Age",
            "Male\u00d7\nAge<30",
            "Male\u00d7\n30\u2264Age"
          )
        ),
        term = str_replace(term, "treat", ""),
        term = factor(term, LETTERS[2:4])
      )

    self$text <- self$data %>%
      select(male, age_less30, pos, N, mean) %>%
      distinct()
  },
  coefplot = function() {
    plot_list <- unique(self$data$outcome) %>%
      purrr::map(function(x) {
        subset(self$data, outcome == x) %>%
          ggplot(aes(x = pos, y = estimate)) +
          geom_hline(aes(yintercept = 0), linetype = 2) +
          geom_point(
            aes(color = term, shape = term),
            size = 3, position = position_dodge(0.5)
          ) +
          geom_errorbar(
            aes(ymin = conf.low, ymax = conf.high, color = term),
            position = position_dodge(0.5),
            width = 0
          ) +
          geom_text(
            aes(y = -0.15, label = N),
            data = subset(self$text, outcome == x),
            color = "black"
          ) +
          geom_text(
            aes(y = -0.175, label = mean),
            data = subset(self$text, outcome == x),
            color = "black"
          ) +
          scale_y_continuous(
            breaks = seq(-0.2, 0.2, by = 0.05), limits = c(-0.2, 0.2)
          ) +
          labs(
            title = paste("Outcome:", x),
            x = "Subset",
            y = "Estimated Effects (95%CI)",
            color = "Treatment", shape = "Treatment"
          ) +
          simplegg()
      })

    wrap_plots(plot_list, ncol = 2) +
      plot_layout(guides = "collect") &
      theme(legend.position = "bottom")
  }
))