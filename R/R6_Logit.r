library(here)
library(R6)
library(tidyverse)
source(here("R/misc.r"))

Logit <- R6::R6Class("Logit",
  public = list(
    data = NULL,
    initialize = function(data, demean_covariate) {
      private$ctrl_arm <- levels(data$treat)[1]

      dt <- data %>%
        mutate(
          RCTweek_fe = if_else(month == 12 | month == 1, RCTweek, 0),
          tiiki = case_when(
            str_detect(prefecture, "^(青森|岩手|秋田|宮城|山形|福島)") ~ "東北",
            str_detect(prefecture, "^(茨城|栃木|群馬)") ~ "北関東",
            str_detect(prefecture, "^(埼玉|千葉)") ~ "南関東",
            str_detect(prefecture, "^(新潟|富山|石川|福井)") ~ "北陸",
            str_detect(prefecture, "^(山梨|長野)") ~ "中央高地",
            str_detect(prefecture, "^(静岡|岐阜|三重)") ~ "東海",
            str_detect(prefecture, "^(滋賀|京都|奈良|和歌山|兵庫)") ~ "近畿",
            str_detect(prefecture, "^(鳥取|島根|岡山|広島|山口)") ~ "中国",
            str_detect(prefecture, "^(徳島|香川|愛媛|高知)") ~ "四国",
            str_detect(prefecture, "^(福岡|長崎|佐賀|大分|熊本|宮崎|鹿児島)") ~ "九州",
            TRUE ~ prefecture
          ),
          tiiki_week = paste0(tiiki, "_", RCTweek_fe)
        )

      if (demean_covariate) {
        dt <- dt %>%
          mutate_at(
            vars(
              age, coordinate, holidays,
              hospital_per_area, PB_per_area, BM_per_area
            ),
            list(~ . - mean(.))
          )
      }

      self$data <- dt

      use_x <- self$data %>%
        select(
          male,
          age,
          coordinate,
          holidays,
          hospital_per_area,
          PB_per_area,
          BM_per_area
        ) %>%
        summarize_all(~ var(.)) %>%
        pivot_longer(everything()) %>%
        filter(value != 0) %>%
        pull(name)

      if (any(use_x %in% "age")) {
        use_x <- c(use_x, "I(age^2)")
      }

      private$covariates <- use_x

      private$model <- list(
        unctrl = reformulate("treat", "value"),
        ctrl1 = reformulate(
          c("treat", use_x),
          "value"
        )
      )

      cat("\n")
      cat("Options for binary regression (Logit)\n")
      cat("- Control arm:", private$ctrl_arm, "\n")
      cat("Regression models\n")
      for (i in 1:length(private$model)) print(private$model[[i]])
      cat("\n")
    },
    fit = function() {
      est <- self$data %>%
        group_by(outcome) %>%
        nest() %>%
        mutate(
          fit1 = private$call_glm(private$model$unctrl, data),
          fit2 = private$call_glm(private$model$ctrl1, data)
        ) %>%
        ungroup() %>%
        pivot_longer(
          fit1:fit2,
          names_prefix = "fit",
          names_to = "model",
          values_to = "fit"
        ) %>%
        mutate(covs = if_else(model != "1", "X", ""))

      LogitAll$new(est)
    }
  ),
  private = list(
    ctrl_arm = "",
    covariates = NULL,
    model = list(),
    call_glm = function(model, data) {
      map(data, ~ glm(model, data = ., family = binomial()))
    }
  )
)

LogitAll <- R6::R6Class("LogitAll",
  public = list(
    initialize = function(est) private$est <- est,
    get_est = function() private$est,
    print_msummary = function() private$reg_tab,
    flextable = function(title = "", notes = "", font_size = 9, ...) {
      private$msummary("flextable", title = title, ...)

      label <- private$label_structure(private$est)
      rle1 <- label$rle1
      # rle2 <- label$rle2

      flex <- private$reg_tab %>%
        add_header_row(values = rle1$values, colwidths = rle1$lengths)

      # if (!is.null(rle2)) {
      #   flex <- flex %>%
      #     add_header_row(values = rle2$values, colwidths = rle2$lengths)
      # }

      flex %>%
        align(j = -1, align = "center", part = "all") %>%
        add_footer_lines(paste(
          "Notes: We show odds ratios and associated 95 percent confidential intervals",
          "in square brackets.",
          notes
        )) %>%
        width(j = 1, 1) %>%
        fontsize(size = font_size, part = "all") %>%
        ft_theme()
    },
    kable = function(title = "", notes = "", font_size = 9, hold = FALSE, ...) {
      private$msummary("kableExtra", title = title, ...)
      tbl <- private$reg_tab

      if (hold) {
        tbl <- tbl %>% kableExtra::kable_styling(font_size = font_size, latex_options = "HOLD_position")
      } else {
        tbl <- tbl %>% kableExtra::kable_styling(font_size = font_size)
      }

      label <- private$label_structure(private$est)
      rle1 <- label$rle1
      lab1 <- rle1$lengths
      names(lab1) <- rle1$values

      tbl <- tbl %>%
        kableExtra::add_header_above(lab1)

      # if (!is.null(label$rle2)) {
      #   rle2 <- label$rle2
      #   lab2 <- rle2$lengths
      #   names(lab2) <- rle2$values

      #   tbl <- tbl %>%
      #     kableExtra::add_header_above(lab2)
      # }

      tbl %>%
        kableExtra::footnote(
          general_title = "",
          general = paste(
            "\\\\emph{Note}: We show odds ratios and associated 95 percent confidential intervals",
            "in square brackets.",
            notes
          ),
          threeparttable = TRUE,
          escape = FALSE
        )
    }
  ),
  private = list(
    est = NULL,
    reg_tab = NULL,
    label_structure = function(est) {
      label <- c(" ", as.character(est$outcome))
      # intention_label <- str_detect(label, "intention")

      # if (any(intention_label)) {
      #   label2 <- ifelse(intention_label, "Intention", " ")
      #   rle2 <- rle(label2)
      # } else {
      #   label2 <- rle2 <- NULL
      # }

      # label1 <- str_remove(label, " intention")
      rle1 <- rle(label)

      return(list(rle1 = rle1))
    },
    msummary = function(output, ...) {
      fit <- private$est %>% pull(fit)
      estimate <- "{or}"
      statistic <- "[{lower.or}, {upper.or}]"
      coef_map <- c(
        "treatB" = "Treatment B",
        "treatC" = "Treatment C",
        "treatD" = "Treatment D"
      )
      stars <- c("***" = .01, "**" = .05, "*" = .1)
      gof_omit <- "R2|AIC|BIC|RMSE|Std|FE|se_type"
      align <- paste(c("l", rep("c", nrow(private$est))), collapse = "")
      add_tab <- data.frame(
        rbind(
          c("Covariates", private$est$covs)
        )
      )
      attr(add_tab, "position") <- 7

      args <- list(
        models = fit,
        output = output,
        estimate = estimate,
        statistic = statistic,
        coef_map = coef_map,
        stars = stars,
        gof_omit = gof_omit,
        add_rows = add_tab,
        align = align
      )

      if (!missing(...)) args <- append(args, list(...))
      private$reg_tab <- do.call("modelsummary", args)

      invisible(self)
    }
  )
)