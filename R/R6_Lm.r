library(here)
library(R6)
library(tidyverse)
library(estimatr)
library(modelsummary)
library(kableExtra)
library(patchwork)
library(fixest)
source(here("R/misc.r"))

Lm <- R6::R6Class("Lm",
  public = list(
    data = NULL,
    initialize = function(data, demean_covariate, se, cluster, hide_message = TRUE) {
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
        private$mean_age <- mean(data$age)

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
          BM_per_area,
          skip_test
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
        ctrl1 = reformulate(c("treat", use_x), "value"),
        ctrl2 = reformulate(c("treat", use_x, "factor(month)"), "value")
      )

      private$se_type <- se
      private$cluster <- cluster

      if (!hide_message) {
        cat("\n")
        cat("Options for linear regression\n")
        cat("- Control arm:", private$ctrl_arm, "\n")
        if (is.null(private$cluster)) {
          cat("- Clustered standard error: FALSE\n")
        } else {
          cat("- Clustered standard error: TRUE\n")
        }
        cat("  - Cluster:", private$cluster, "\n")
        cat("  - Standard error type:", private$se_type, "\n")
        cat("Regression models\n")
        for (i in 1:length(private$model)) print(private$model[[i]])
        cat("\n")
      }
    },
    fit = function( scale = 1,
                    interaction = NULL,
                    drop_x = NULL)
    {
      model_type <- if (is.null(interaction)) {
        "average effect"
      } else {
        "heterogeneous effect"
      }

      est_dt <- self$data %>%
        mutate(value = value * scale) %>%
        group_by(outcome) %>%
        nest()

      est_dt2 <- est_dt

      if (model_type == "average effect") {
        est_dt2 <- est_dt2 %>%
          mutate(
            avg = map_dbl(data, ~ private$ctrl_mean(.))
          )
      }

      if (model_type == "average effect") {
        lh <- FALSE
        model <- private$model
      } else {
        lh <- TRUE
        use_x <- private$covariates
        use_x_2 <- use_x[!str_detect(use_x, paste0("^", interaction, "$"))]
        if (!is.null(drop_x)) {
          pat <- paste(drop_x, collapse = "|")
          use_x_2 <- use_x_2[!str_detect(use_x_2, pat)]
        }
        cross_term_x <- paste0(use_x_2, ":", interaction)
        cross_term_treat <- paste0("treat * ", interaction)

        model <- list(
          unctrl = reformulate(cross_term_treat, "value"),
          ctrl1 = reformulate(c(cross_term_treat, use_x_2, cross_term_x), "value"),
          ctrl2 = reformulate(c(cross_term_treat, use_x_2, cross_term_x, "factor(month)"), "value")
        )
      }

      est <- est_dt2 %>%
        mutate(
          fit_1 = map(data, ~ private$call_lh(., model$unctrl, lh)),
          fit_2 = map(data, ~ private$call_lh(., model$ctrl1, lh)),
          fit_3 = map(data, ~ private$call_lh(., model$ctrl2, lh))
        ) %>%
        ungroup() %>%
        pivot_longer(
          fit_1:fit_3,
          names_prefix = "fit_",
          names_to = "model",
          values_to = "fit"
        ) %>%
        mutate(
          covariate = if_else(model != "1", "X", ""),
          fe = if_else(model == "3", "X", "")
        ) %>%
        arrange(outcome)

      LmFit$new(est, model_type)
    }
  ),
  private = list(
    ctrl_arm = "",
    model = list(),
    se_type = "",
    cluster = NULL,
    covariates = "",
    mean_age = 0,
    call_lh = function(data, model, lh = FALSE) {
      if (is.null(private$cluster)) {
        if (!lh) {
          lm_robust(model, data = data, se_type = private$se_type)
        } else {
          reg <- lm_robust(model, data = data, se_type = private$se_type)

          term <- tidy(reg)$term
          term <- term[str_detect(term, "treat")]
          single <- term[!str_detect(term, ":")]

          hypo <- single %>%
            sapply(function(x) paste(term[str_detect(term, x)], collapse = " + "))

          lh <- hypo %>%
            map(~ lh_robust(model, data = data, se_type = private$se_type, linear_hypothesis = .)) %>%
            map(~ .$lh) %>%
            map(tidy) %>%
            reduce(bind_rows)

          list(lm_robust = reg, lh = lh)
        }
      } else {
        g <- data[, private$cluster, drop = TRUE]
        if (!lh) {
          lm_robust(model, data = data, se_type = private$se_type, clusters = g)
        } else {
          reg <- lm_robust(model, data = data, se_type = private$se_type, clusters = g)

          term <- tidy(reg)$term
          term <- term[str_detect(term, "treat")]
          single <- term[!str_detect(term, ":")]

          hypo <- single %>%
            sapply(function(x) paste(term[str_detect(term, x)], collapse = " + "))

          lh <- hypo %>%
            map(~ lh_robust(
              model,
              data = data,
              se_type = private$se_type,
              clusters = g,
              linear_hypothesis = .
            )) %>%
            map(~ .$lh) %>%
            map(tidy) %>%
            reduce(bind_rows)

          list(lm_robust = reg, lh = lh)
        }
      }
    },
    ctrl_mean = function(data) {
      with(
        subset(data, treat == private$ctrl_arm),
        mean(value)
      )
    }
  )
)

LmFit <- R6::R6Class("LmFit",
  public = list(
    initialize = function(est, model_type) {
      private$est <- est
      private$type <- model_type
    },
    get_est = function() private$est,
    kable_reg = function( title = "",
                          coef_map_lh = NULL,
                          coef_map_lm = c(
                            "treatB" = "Experimental group B",
                            "treatC" = "Experimental group C",
                            "treatD" = "Experimental group D"
                          ),
                          label_lh = "Linear combination test",
                          notes = "",
                          font_size = 9,
                          digit = 2,
                          hold = FALSE)
    {
      res <- private$est

      if (private$type == "average effect") {
        avg_format <- paste0("%1.", digit, "f")

        add_tab <- data.frame(
          rbind(
            c("Control average", sprintf(avg_format, res$avg)),
            c("Covariates", res$covariate),
            c("Month FE", res$fe)
          )
        )
      } else {
        lh_summary <- res %>%
          pull(fit) %>%
          map(~ .$lh) %>%
          map(function(x) {
            x %>%
              mutate(
                p.value_label = case_when(
                  p.value < 0.01 ~ "***",
                  p.value < 0.05 ~ "**",
                  p.value < 0.1 ~ "*",
                  TRUE ~ ""
                ),
                estimate_label = sprintf("%1.2f%s", estimate, p.value_label),
                se_label = sprintf("(%1.2f)", std.error)
              ) %>%
              select(term, estimate = estimate_label, se = se_label) %>%
              pivot_longer(cols = c(estimate, se), names_to = "statistic") %>%
              mutate(term = if_else(statistic == "se", "", term)) %>%
              select(-statistic)
          })

        lh_summary[-1] <- lh_summary[-1] %>%
          map(~ select(., -term))

        lh_tab <- reduce(lh_summary, bind_cols)
        if (!is.null(coef_map_lh)) {
          lh_tab <- lh_tab %>%
            mutate(term = dplyr::recode(term, !!!coef_map_lh))
        }

        add_tab <- data.frame(rbind(
          lh_tab,
          c("Covariates", res$covariate),
          c("Month FE", res$fe)
        ))
      }

      attr(add_tab, "position") <- seq(
        length(coef_map_lm) * 2 + 1,
        length.out = nrow(add_tab)
      )

      fit <- pull(res, fit)
      if (private$type != "average effect") fit <- map(fit, ~ .$lm_robust)

      kbl <- fit %>%
        modelsummary(
          title = title,
          coef_map = coef_map_lm,
          stars = c("***" = .01, "**" = .05, "*" = .1),
          gof_omit = "R2|AIC|BIC|Log|Std|FE|se_type|RMSE",
          align = paste(c("l", rep("c", nrow(res))), collapse = ""),
          add_rows = add_tab,
          fmt = digit,
          escape = FALSE,
          output = "kableExtra"
        )

      if (hold) {
        kbl <- kbl %>%
          kableExtra::kable_styling(font_size = font_size, latex_options = "HOLD_position")
      } else {
        kbl <- kbl %>%
          kableExtra::kable_styling(font_size = font_size)
      }

      label <- c(" ", as.character(res$outcome))
      label_structure <- rle(label)
      lab1 <- label_structure$lengths
      names(lab1) <- label_structure$values

      kbl <- kbl %>%
        kableExtra::add_header_above(lab1)

      if (private$type != "average effect") {
        pos <- base::attr(add_tab, "position")

        kbl <- kbl %>%
          kableExtra::group_rows(
            label_lh,
            pos[1],
            pos[length(pos) - 1],
            bold = FALSE,
            italic = TRUE,
            escape = FALSE
          )
      }

      kbl %>%
        kableExtra::footnote(
          general_title = "",
          general = paste(
            "\\\\emph{Note}: * $p < 0.1$, ** $p < 0.05$, *** $p < 0.01$.",
            notes
          ),
          threeparttable = TRUE,
          escape = FALSE
        )
    },
    plot_ate = function(segment_margin = 3,
                        add_segment_margin = 7,
                        edge_length = 2,
                        show_p = 0.1,
                        p_digit = 3,
                        p_text_size = 5,
                        p_text_margin = 2,
                        avg_digit = 1,
                        avg_percent = TRUE,
                        avg_text_size = 5,
                        avg_text_pos = 10,
                        xlab = "Experimental Arm",
                        ylab = "Sample Average",
                        ylim = c(0, 100),
                        ybreaks = seq(0, 100, by = 10),
                        base_size = 15)
    {
      wocov <- subset(private$est, covariate == "")

      if (private$type == "average effect") {
        est <- wocov %>%
          mutate(
            tidy = map(fit, broom::tidy),
            tidy = map(tidy, ~ subset(., str_detect(term, "treat"))),
            tidy = map(tidy, ~ select(., -outcome))
          ) %>%
          select(outcome, tidy) %>%
          unnest(tidy) %>%
          mutate(
            treat = str_remove(term, "treat"),
            group = "Full sample"
          ) %>%
          select(outcome, group, treat, p.value)
      } else {
        est <- wocov %>%
          mutate(
            fit = map(fit, ~ .$lh),
            tidy = map(fit, broom::tidy),
            tidy = map(tidy, ~ select(., -outcome))
          ) %>%
          select(outcome, tidy) %>%
          unnest(tidy) %>%
          mutate(
            term = dplyr::recode(term, !!!private$coef_map_lh),
            treat = str_remove(str_split(term, "_", simplify = TRUE)[, 1], "Treatment "),
            group = str_split(term, "_", simplify = TRUE)[, 2]
          ) %>%
          select(outcome, group, treat, p.value)
      }

      if (private$type == "average effect") {
        wocov <- wocov %>%
          mutate(data = map(data, ~ mutate(., group = "Full sample")))
      }

      mu <- wocov %>%
        select(outcome, data) %>%
        mutate(avg = map(
          data,
          function(x) {
            group_by(x, treat, group) %>%
              summarize(mu = mean(value), se = se(value))
          }
        )) %>%
        select(-data) %>%
        unnest(avg)

      plotdt <- mu %>%
        dplyr::left_join(est, by = c("outcome", "group", "treat"), keep = TRUE) %>%
        select(-outcome.y, -treat.y, -group.y) %>%
        rename(treat = treat.x, outcome = outcome.x, group = group.x)

      mu_ctrl <- plotdt %>%
        dplyr::filter(is.na(p.value)) %>%
        select(outcome, group, ctrl_mu = mu)

      plotdt <- plotdt %>%
        dplyr::left_join(mu_ctrl, by = c("outcome", "group")) %>%
        group_by(outcome, group) %>%
        mutate(x_start = 1, x_end = 1:n()) %>%
        rowwise() %>%
        mutate(
          y = max(mu, ctrl_mu),
          y = y + segment_margin + add_segment_margin * (x_end - 1)
        ) %>%
        ungroup() %>%
        select(-ctrl_mu) %>%
        mutate_at(
          vars(y, x_end, x_start),
          list(~ ifelse(p.value > show_p | is.na(p.value), NA_real_, .))
        )

      avg_format <- paste0("%1.", avg_digit, "f")
      if (avg_percent) avg_format <- paste0(avg_format, "%%")

      plt <- ggplot(plotdt, aes(x = treat, y = mu)) +
        geom_bar(stat = "identity", fill = "grey90", color = "black") +
        geom_errorbar(aes(ymin = mu - se, ymax = mu + se), width = 0.25) +
        geom_text(
          aes(y = avg_text_pos, label = sprintf(avg_format, mu)),
          color = "black", size = avg_text_size
        ) +
        geom_segment(
          aes(x = x_start, xend = x_end, y = y, yend = y),
          color = "black"
        ) +
        geom_segment(
          aes(x = x_start, xend = x_start, y = y, yend = y - edge_length),
          color = "black"
        ) +
        geom_segment(
          aes(x = x_end, xend = x_end, y = y, yend = y - edge_length),
          color = "black"
        ) +
        geom_text(
          aes(
            x = x_start,
            y = y + p_text_margin,
            label = sprintf(paste0("p = %1.", p_digit, "f"), p.value)
          ),
          hjust = 0, color = "black", size = p_text_size
        ) +
        scale_y_continuous(limits = ylim, breaks = ybreaks) +
        labs(x = xlab, y = ylab) +
        my_theme_classic(size = base_size, strip_hjust = 0.5)

      if (private$type == "average effect") {
        plt + facet_wrap(~ outcome)
      } else {
        plt + facet_grid(group ~ outcome)
      }
    }
  ),
  private = list(
    est = NULL,
    type = NULL,
    coef_map_lm = NULL,
    coef_map_lh = NULL
  )
)
