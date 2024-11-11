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
    initialize = function(data, se) {
      private$ctrl_arm <- levels(data$treat)[1]

      self$data <- data %>%
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

      private$model <- list(
        unctrl = value ~ treat,
        ctrl1 = value ~ treat + male + age_demean + I(age_demean^2) + coordinate +
          holidays + hospital_per_area + PB_per_area + BM_per_area,
        ctrl2 = value ~ treat + male + age_demean + I(age_demean^2) + coordinate +
          holidays + hospital_per_area + PB_per_area + BM_per_area + factor(tiiki_week)
      )

      private$se_type <- se

      cat("\n")
      cat("Options for linear regression\n")
      cat("- Control arm:", private$ctrl_arm, "\n")
      cat("- Clustered standard error: FALSE\n")
      cat("  - Standard error type:", private$se_type, "\n")
      cat("Regression models\n")
      for (i in 1:length(private$model)) print(private$model[[i]])
      cat("\n")
    },
    fit = function(scale = 1) {
      est <- self$data %>%
        mutate(value = value * scale) %>%
        group_by(outcome) %>%
        nest() %>%
        mutate(
          fit1 = private$call_lm(data, private$model$unctrl, private$se_type),
          fit2 = private$call_lm(data, private$model$ctrl1, private$se_type),
          fit3 = private$call_lm(data, private$model$ctrl2, private$se_type),
          avg = map_dbl(
            data,
            ~ mean(subset(., treat == private$ctrl_arm)$value)
          )
        ) %>%
        ungroup() %>%
        pivot_longer(
          fit1:fit3,
          names_prefix = "fit",
          names_to = "model",
          values_to = "fit"
        ) %>%
        mutate(
          covs_i = if_else(model != "1", "X", ""),
          covs_p = if_else(model != "1", "X", ""),
          covs_w = if_else(model != "1", "X", ""),
          covs_fe = if_else(model == "3", "X", "")
        )

      LmAll$new(est)
    },
    fit_subset_by_gender_age = function(age_cut = 30, scale = 1, covariates = TRUE) {
      model <- if (covariates) {
        update(private$model$ctrl, . ~ . - male - age_demean - I(age_demean^2))
      } else {
        private$model$unctrl
      }

      est <- self$data %>%
        mutate(value = value * scale) %>%
        mutate(young = if_else(age < age_cut, 1, 0)) %>%
        group_by(outcome, male, young) %>%
        nest() %>%
        mutate(
          fit = private$call_lm(data, model, private$se_type),
          avg = map_chr(
            data,
            ~ with(
              subset(., treat == private$ctrl_arm),
              sprintf("Ctrl Avg = %1.2f", mean(value))
            )
          ),
          covariates = if_else(covariates, "X", "")
        )

      LmSubset$new(est, age_cut)
    },
    fit_interaction_of_gender_age = function(age_cut = 30, scale = 1) {
      lh_null <- c(
        "treatB",
        "treatC",
        "treatD",
        "treatB + treatB:groupOlder female",
        "treatC + treatC:groupOlder female",
        "treatD + treatD:groupOlder female",
        "treatB + treatB:groupYoung male",
        "treatC + treatC:groupYoung male",
        "treatD + treatD:groupYoung male",
        "treatB + treatB:groupOlder male",
        "treatC + treatC:groupOlder male",
        "treatD + treatD:groupOlder male"
      )

      interaction_mod <- lapply(
        private$model,
        function(x) {
          update(
            x,
            . ~ . + group + treat:group - male - age_demean - I(age_demean^2)
          )
        }
      )

      est <- self$data %>%
        mutate(
          value = value * scale,
          young = if_else(age < age_cut, 1, 0),
          group = case_when(
            male == 0 & young == 1 ~ 1,
            male == 0 & young == 0 ~ 2,
            male == 1 & young == 1 ~ 3,
            male == 1 & young == 0 ~ 4
          ),
          group = factor(group, labels = c("Young female", "Older female", "Young male", "Older male"))
        ) %>%
        group_by(outcome) %>%
        nest() %>%
        mutate(
          fit1 = private$call_lh(
            data,
            interaction_mod$unctrl,
            private$se_type,
            lh_null
          ),
          fit2 = private$call_lh(
            data,
            interaction_mod$ctrl1,
            private$se_type,
            lh_null
          ),
          fit3 = private$call_lh(
            data,
            interaction_mod$ctrl2,
            private$se_type,
            lh_null
          )
        ) %>%
        ungroup() %>%
        pivot_longer(
          fit1:fit3,
          names_prefix = "fit",
          names_to = "model",
          values_to = "fit"
        ) %>%
        mutate(
          covs_i = if_else(model != "1", "X", ""),
          covs_p = if_else(model != "1", "X", ""),
          covs_w = if_else(model != "1", "X", ""),
          covs_fe = if_else(model == "3", "X", "")
        )

      LmInteraction$new(est, age_cut)
    }
  ),
  private = list(
    ctrl_arm = "",
    model = list(),
    se_type = "",
    call_lm = function(data, model, se) {
      map(data, ~ lm_robust(model, data = ., se_type = se))
    },
    call_lh = function(data, model, se, lh) {
      map(data, ~ lh_robust(model, data = ., se_type = se, linear_hypothesis = lh))
    }
  )
)

LmCluster <- R6::R6Class("LmCluster",
  public = list(
    data = NULL,
    initialize = function(data, se, cluster) {
      private$ctrl_arm <- levels(data$treat)[1]

      self$data <- data %>%
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

      private$model <- list(
        unctrl = value ~ treat,
        ctrl1 = value ~ treat + male + age_demean + I(age_demean^2) + coordinate +
          holidays + hospital_per_area + PB_per_area + BM_per_area,
        ctrl2 = value ~ treat + male + age_demean + I(age_demean^2) + coordinate +
          holidays + factor(tiiki_week)
      )

      private$se_type <- se
      private$cluster <- cluster

      cat("\n")
      cat("Options for linear regression\n")
      cat("- Control arm:", private$ctrl_arm, "\n")
      cat("- Clustered standard error: TRUE\n")
      cat("  - Cluster:", private$cluster, "\n")
      cat("  - Standard error type:", private$se_type, "\n")
      cat("Regression models\n")
      for (i in 1:length(private$model)) print(private$model[[i]])
      cat("\n")
    },
    fit = function(scale = 1) {
      est <- self$data %>%
        mutate(value = value * scale) %>%
        group_by(outcome) %>%
        nest() %>%
        mutate(
          fit1 = private$call_lm(data, private$model$unctrl, private$se_type, private$cluster),
          fit2 = private$call_lm(data, private$model$ctrl1, private$se_type, private$cluster),
          fit3 = private$call_lm(data, private$model$ctrl2, private$se_type, private$cluster),
          avg = map_dbl(
            data,
            ~ mean(subset(., treat == private$ctrl_arm)$value)
          )
        ) %>%
        ungroup() %>%
        pivot_longer(
          fit1:fit3,
          names_prefix = "fit",
          names_to = "model",
          values_to = "fit"
        ) %>%
        mutate(
          covs_i = if_else(model != "1", "X", ""),
          covs_p = if_else(model != "1", "X", ""),
          covs_w = if_else(model != "1", "X", ""),
          covs_fe = if_else(model == "3", "X", "")
        )

      LmAll$new(est)
    },
    fit_subset_by_gender_age = function(age_cut = 30, scale = 1, covariates = TRUE) {
      model <- if (covariates) {
        update(private$model$ctrl, . ~ . - male - age_demean - I(age_demean^2))
      } else {
        private$model$unctrl
      }

      est <- self$data %>%
        mutate(value = value * scale) %>%
        mutate(young = if_else(age < age_cut, 1, 0)) %>%
        group_by(outcome, male, young) %>%
        nest() %>%
        mutate(
          fit = private$call_lm(data, model, private$se_type, private$cluster),
          avg = map_chr(
            data,
            ~ with(
              subset(., treat == private$ctrl_arm),
              sprintf("Ctrl Avg = %1.2f", mean(value))
            )
          )
        )

      LmSubset$new(est, age_cut)
    },
    fit_interaction_of_gender_age = function(age_cut = 30, scale = 1) {
      lh_null <- c(
        "treatB",
        "treatC",
        "treatD",
        "treatB + treatB:groupOlder female",
        "treatC + treatC:groupOlder female",
        "treatD + treatD:groupOlder female",
        "treatB + treatB:groupYoung male",
        "treatC + treatC:groupYoung male",
        "treatD + treatD:groupYoung male",
        "treatB + treatB:groupOlder male",
        "treatC + treatC:groupOlder male",
        "treatD + treatD:groupOlder male"
      )

      interaction_mod <- lapply(
        private$model,
        function(x) {
          update(
            x,
            . ~ . + group + treat:group - male - age_demean - I(age_demean^2)
          )
        }
      )

      est <- self$data %>%
        mutate(
          value = value * scale,
          young = if_else(age < age_cut, 1, 0),
          group = case_when(
            male == 0 & young == 1 ~ 1,
            male == 0 & young == 0 ~ 2,
            male == 1 & young == 1 ~ 3,
            male == 1 & young == 0 ~ 4
          ),
          group = factor(
            group,
            labels = c("Young female", "Older female", "Young male", "Older male")
          )
        ) %>%
        group_by(outcome) %>%
        nest() %>%
        mutate(
          fit1 = private$call_lh(
            data,
            interaction_mod$unctrl,
            private$se_type,
            private$cluster,
            lh_null
          ),
          fit2 = private$call_lh(
            data,
            interaction_mod$ctrl1,
            private$se_type,
            private$cluster,
            lh_null
          ),
          fit3 = private$call_lh(
            data,
            interaction_mod$ctrl2,
            private$se_type,
            private$cluster,
            lh_null
          )
        ) %>%
        ungroup() %>%
        pivot_longer(
          fit1:fit3,
          names_prefix = "fit",
          names_to = "model",
          values_to = "fit"
        ) %>%
        mutate(
          covs_i = if_else(model != "1", "X", ""),
          covs_p = if_else(model != "1", "X", ""),
          covs_w = if_else(model != "1", "X", ""),
          covs_fe = if_else(model == "3", "X", "")
        )

      LmInteraction$new(est, age_cut)
    }
  ),
  private = list(
    ctrl_arm = "",
    model = list(),
    se_type = "",
    cluster = NULL,
    call_lm = function(data, model, se, cluster) {
      map(
        data,
        function(d) {
          g <- d[, cluster, drop = TRUE]
          lm_robust(
            model,
            data = d,
            clusters = g,
            se_type = se
          )
        }
      )
    },
    call_lh = function(data, model, se, cluster, lh) {
      map(
        data,
        function(d) {
          g <- d[, cluster, drop = TRUE]
          lh_robust(model, data = d, clusters = g, se_type = se, linear_hypothesis = lh)
        }
      )
    }
  )
)

LmAll <- R6::R6Class("LmAll",
  public = list(
    initialize = function(est) private$est <- est,
    get_est = function() private$est,
    print_msummary = function() private$reg_tab,
    flextable = function( title = "",
                          notes = "",
                          font_size = 9,
                          digit = 2,
                          ...)
    {
      private$msummary("flextable", title = title, digit = digit, ...)

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
          "Notes: * $p < 0.1$, ** $p < 0.05$, *** $p < 0.01$.",
          "The robust standard errors are in parentheses.",
          notes
        )) %>%
        width(j = 1, 1) %>%
        fontsize(size = font_size, part = "all") %>%
        ft_theme()
    },
    kable = function( title = "",
                      notes = "",
                      font_size = 9,
                      digit = 2,
                      hold = FALSE,
                      ...)
    {
      private$msummary("kableExtra", title = title, digit = digit, ...)

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

      tbl <- tbl %>% kableExtra::add_header_above(lab1)

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
            "\\\\emph{Note}: * $p < 0.1$, ** $p < 0.05$, *** $p < 0.01$.",
            "The robust standard errors are in parentheses.",
            notes
          ),
          threeparttable = TRUE,
          escape = FALSE
        )
    },
    plot = function(segment_margin = 3,
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
                    ylim = c(0, 100),
                    ybreaks = seq(0, 100, by = 10),
                    base_size = 15)
    {
      wocov <- subset(private$est, covs_i == "")

      est <- wocov %>%
        mutate(
          tidy = map(fit, broom::tidy),
          tidy = map(tidy, ~ subset(., str_detect(term, "treat"))),
          tidy = map(tidy, ~ select(., -outcome))
        ) %>%
        select(outcome, tidy) %>%
        unnest(tidy) %>%
        mutate(treat = str_remove(term, "treat")) %>%
        select(outcome, treat, p.value)

      mu <- wocov %>%
        select(outcome, data) %>%
        mutate(avg = map(
          data,
          function(x) {
            group_by(x, treat) %>%
              summarize(mu = mean(value), se = se(value))
          }
        )) %>%
        select(-data) %>%
        unnest(avg)

      plotdt <- mu %>%
        dplyr::left_join(est, by = c("outcome", "treat"), keep = TRUE) %>%
        select(-outcome.y, -treat.y) %>%
        rename(treat = treat.x, outcome = outcome.x)

      mu_ctrl <- plotdt %>%
        dplyr::filter(is.na(p.value)) %>%
        select(outcome, ctrl_mu = mu)

      plotdt <- plotdt %>%
        dplyr::left_join(mu_ctrl, by = "outcome") %>%
        group_by(outcome) %>%
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

      ggplot(plotdt, aes(x = treat, y = mu)) +
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
        facet_wrap(~outcome) +
        labs(x = "Treatment", y = "Sample average") +
        my_theme_classic(size = base_size, strip_hjust = 0.5)
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
    msummary = function(output, digit = 2, ...) {
      fit <- private$est %>% pull(fit)
      coef_map <- c(
        "treatB" = "Treatment B",
        "treatC" = "Treatment C",
        "treatD" = "Treatment D"
      )
      stars <- c("***" = .01, "**" = .05, "*" = .1)
      gof_omit <- "R2|AIC|BIC|Log|Std|FE|se_type"
      align <- paste(c("l", rep("c", nrow(private$est))), collapse = "")

      avg_format <- paste0("%1.", digit, "f")

      add_tab <- data.frame(
        rbind(
          c("Control average", sprintf(avg_format, private$est$avg)),
          c("Individual-level covariates", private$est$covs_i),
          c("Week-level covariates", private$est$covs_w),
          c("Prefecture-level covariates", private$est$covs_p),
          c("Region$\\times$Week (in Dec. and Jan.) FE", private$est$covs_fe)
        )
      )
      attr(add_tab, "position") <- 7:12

      args <- list(
        models = fit,
        output = output,
        coef_map = coef_map,
        stars = stars,
        gof_omit = gof_omit,
        align = align,
        add_rows = add_tab,
        fmt = digit
      )

      if (!missing(...)) args <- append(args, list(...))
      private$reg_tab <- do.call("modelsummary", args)

      invisible(self)
    }
  )
)

LmSubset <- R6::R6Class("LmSubset",
  public = list(
    initialize = function(est, age_cut) {
      private$est <- est

      lev <- c("01", "00", "11", "10")
      young_lab <- paste0("Age < ", age_cut)
      old_lab <- paste0(age_cut, " \u2264 Age")
      labs <- c(
        paste0("Female\u00d7\n", young_lab),
        paste0("Female\u00d7\n", old_lab),
        paste0("Male\u00d7\n", young_lab),
        paste0("Male\u00d7\n", old_lab)
      )
      names(lev) <- labs
      private$subset_labels <- lev

    },
    get_est = function() private$est,
    kable = function( title = "",
                      notes = "",
                      font_size = 9,
                      digit = 2,
                      hold = FALSE,
                      ...)
    {
      est <- private$est
      tbl <- private$reg_table(est, digit)

      outcome_labels <- unique(est$outcome)

      group_labels <- est %>%
        arrange(male, desc(young), outcome) %>%
        ungroup() %>%
        mutate(N = map_dbl(fit, nobs)) %>%
        select(male, young, N) %>%
        mutate(g = case_when(
          male == 0 & young == 1 ~ "Young females",
          male == 0 & young == 0 ~ "Older females",
          male == 1 & young == 1 ~ "Young males",
          male == 1 & young == 0 ~ "Older males"
        )) %>%
        mutate(label = sprintf(paste(g, "(N = %1d)"), N)) %>%
        select(male, young, label) %>%
        distinct()

      kbl <- tbl %>%
        knitr::kable(
          caption = title,
          col.names = c("", as.character(outcome_labels)),
          align = paste0(c("l", rep("c", length(outcome_labels))), collapse = ""),
          booktabs = TRUE,
          linesep = ""
        ) %>%
        pack_rows(group_labels$label[1], 1, 4) %>%
        pack_rows(group_labels$label[2], 5, 8) %>%
        pack_rows(group_labels$label[3], 9, 12) %>%
        pack_rows(group_labels$label[4], 13, 16) %>%
        kableExtra::footnote(
          general_title = "",
          general = paste("\\\\emph{Note}:", notes),
          threeparttable = TRUE,
          escape = FALSE
        )

      if (hold) {
        kbl %>%
          kableExtra::kable_styling(font_size = font_size, latex_options = "HOLD_position")
      } else {
        kbl %>%
          kableExtra::kable_styling(font_size = font_size)
      }
    },
    coefplot = function(label_N_y_pos = -0.15,
                        label_mean_y_pos = label_N_y_pos - 0.025,
                        y_lim = c(-0.2, 0.2),
                        y_break = seq(-1, 1, by = 0.05))
    {
      plotdt <- private$est %>%
        mutate(
          tidy = map(fit, tidy),
          tidy = map(tidy, ~ subset(.x, str_detect(term, "treat"))),
          tidy = map(tidy, ~ dplyr::select(.x, -outcome)),
          N = map_chr(fit, ~ paste("N =", nobs(.x)))
        ) %>%
        select(-fit) %>%
        unnest(cols = tidy) %>%
        mutate(
          pos = paste0(male, young),
          pos = factor(
            pos,
            levels = private$subset_labels,
            labels = names(private$subset_labels)
          ),
          term = str_remove(term, "treat")
        )

      text <- plotdt %>%
        select(male, young, pos, N, avg) %>%
        distinct()

      plot_list <- unique(plotdt$outcome) %>%
        purrr::map(function(x) {
          subset(plotdt, outcome == x) %>%
            ggplot(aes(x = pos, y = estimate, shape = term)) +
            geom_hline(aes(yintercept = 0), linetype = 2) +
            geom_point(
              size = 3, position = position_dodge(0.5),
              color = "black"
            ) +
            geom_errorbar(
              aes(ymin = conf.low, ymax = conf.high),
              position = position_dodge(0.5),
              width = 0,
              color = "black"
            ) +
            geom_text(
              aes(x = pos, y = label_N_y_pos, label = N),
              data = subset(text, outcome == x),
              color = "black",
              inherit.aes = FALSE
            ) +
            geom_text(
              aes(x = pos, y = label_mean_y_pos, label = avg),
              data = subset(text, outcome == x),
              color = "black",
              inherit.aes = FALSE
            ) +
            scale_y_continuous(breaks = y_break, limits = y_lim) +
            labs(
              title = paste("Outcome:", x),
              x = "Subset",
              y = "Estimated Effects (95%CI)",
              color = "Treatment", shape = "Treatment"
            ) +
            my_theme_classic() +
            guides(shape = guide_legend(override.aes = list(size = 4)))
        })

      wrap_plots(plot_list, ncol = 2) +
        plot_layout(guides = "collect") &
        theme(legend.position = "bottom")
    }
  ),
  private = list(
    est = NULL,
    subset_labels = NULL,
    reg_table = function(x, digit = 2) {
      ctrl_avg <- private$est %>%
        arrange(male, desc(young), outcome) %>%
        ungroup() %>%
        pull(data) %>%
        map_chr(
          function(x) {
            sprintf(
              paste0("%1.", digit, "f"),
              mean(subset(x, treat == levels(x$treat)[1])$value)
            )
          }
        )

      ctrl_avg_tbl <- tibble(
        name = c("term", paste0("(", seq(length(ctrl_avg)), ")")),
        value = c("Control average", ctrl_avg)
      )

      tab <- x %>%
        arrange(male, desc(young), outcome) %>%
        ungroup() %>%
        pull(fit) %>%
        modelsummary(
          estimate = "{estimate}{stars} ({std.error})",
          statistic = NULL,
          stars = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
          coef_map = c(
            "treatB" = "Treatment B",
            "treatC" = "Treatment C",
            "treatD" = "Treatment D"
          ),
          gof_omit = "R2|AIC|BIC|Log|Std|FE|se_type",
          fmt = digit,
          add_rows = pivot_wider(ctrl_avg_tbl),
          output = "data.frame"
        ) %>%
        select(-part, -statistic)

      num_outcomes <- length(unique(x$outcome))
      tab_cut <- vector("list", 4)
      for (i in seq_len(length(tab_cut))) {
        if (i == 1) {
          tab_cut[[i]] <- c(1, seq(2, length = num_outcomes))
        } else {
          tab_cut[[i]] <- c(1, seq(max(tab_cut[[i-1]]) + 1, length = num_outcomes))
        }
      }

      stack_tab <- tab_cut %>%
        map(function(cols) {
          cutting <- tab[, cols]
          names(cutting) <- c("term", paste0("(", seq(num_outcomes), ")"))
          return(cutting)
        }) %>%
        reduce(bind_rows) %>%
        filter(term != "Num.Obs.")

      stack_tab
    }
  )
)

LmInteraction <- R6::R6Class("LmInteraction",
  public = list(
    initialize = function(est, age_cut) {
      private$est <- est
      private$age_cut <- age_cut
    },
    get_est = function() private$est,
    kable_reg = function( title = "",
                          notes = "",
                          font_size = 9,
                          digit = 2,
                          hold = FALSE,
                          ...)
    {
      kbl <- private$reg_msummary("kableExtra", digit = digit, title = title, ...)

      if (hold) {
        kbl <- kbl %>%
          kableExtra::kable_styling(font_size = font_size, latex_options = "HOLD_position")
      } else {
        kbl <- kbl %>%
          kableExtra::kable_styling(font_size = font_size)
      }

      label <- c(" ", as.character(private$est$outcome))
      label_structure <- rle(label)
      lab1 <- label_structure$lengths
      names(lab1) <- label_structure$values

      kbl <- kbl %>%
        kableExtra::add_header_above(lab1)

      kbl %>%
        kableExtra::footnote(
          general_title = "",
          general = paste(
            "\\\\emph{Note}: * $p < 0.1$, ** $p < 0.05$, *** $p < 0.01$.",
            "The robust standard errors are in parentheses.",
            notes
          ),
          threeparttable = TRUE,
          escape = FALSE
        )
    },
    kable_lh = function(outcome,
                        title = "",
                        notes = "",
                        font_size = 9,
                        digits = 2,
                        hold = FALSE)
    {
      mtab <- private$lh_msummary(outcome, digits)

      mtab_wide1 <- mtab %>%
        select(subset, treat, statistic, "(1)") %>%
        pivot_wider(names_from = subset, values_from = "(1)") %>%
        mutate(
          covs_i = private$est$covs_i[1],
          covs_w = private$est$covs_w[1],
          covs_p = private$est$covs_p[1],
          covs_fe = private$est$covs_fe[1]
        )

      mtab_wide2 <- mtab %>%
        select(subset, treat, statistic, "(2)") %>%
        pivot_wider(names_from = subset, values_from = "(2)") %>%
        mutate(
          covs_i = private$est$covs_i[2],
          covs_w = private$est$covs_w[2],
          covs_p = private$est$covs_p[2],
          covs_fe = private$est$covs_fe[2]
        )

      mtab_wide3 <- mtab %>%
        select(subset, treat, statistic, "(3)") %>%
        pivot_wider(names_from = subset, values_from = "(3)") %>%
        mutate(
          covs_i = private$est$covs_i[3],
          covs_w = private$est$covs_w[2],
          covs_p = private$est$covs_p[3],
          covs_fe = private$est$covs_fe[3]
        )

      avg_format <- paste0("%1.", digits, "f")

      dt <- private$est$data[[1]]
      statistic <- dt %>%
        filter(treat == levels(dt$treat)[1]) %>%
        mutate(
          group = forcats::fct_recode(
            group,
            "YoungFemale" = "Young female",
            "OlderFemale" = "Older female",
            "YoungMale" = "Young male",
            "OlderMale" = "Older male"
          )
        ) %>%
        group_by(group) %>%
        summarize(`Control average` = sprintf(avg_format, mean(value))) %>%
        pivot_longer(-group, names_to = "statistic") %>%
        pivot_wider(names_from = group, values_from = value) %>%
        mutate(treat = statistic, statistic = "gof")

      tbl <- bind_rows(statistic, mtab_wide1, mtab_wide2, mtab_wide3) %>%
        mutate(
          treat = if_else(statistic == "std.error", "", treat),
          model = case_when(
            is.na(covs_i) ~ NA_character_,
            covs_i == "" ~ "Model (1): No covariates",
            covs_fe == "X" ~ "Model (3): Including covariates and FE",
            TRUE ~ "Model (2): Including covariates"
          )
        ) %>%
        select(-statistic) %>%
        select(treat, everything())

      kbl <- tbl %>%
        select(-covs_i, -covs_p, -covs_w, -covs_fe, -model) %>%
        knitr::kable(
          caption = title,
          col.names = c("", paste0("(", seq(ncol(tbl) - 6), ")")),
          align = paste(c("l", rep("c", ncol(tbl) - 6)), collapse = ""),
          booktabs = TRUE,
          linesep = ""
        )

      if (hold) {
        kbl <- kbl %>%
          kableExtra::kable_styling(font_size = font_size, latex_options = "HOLD_position")
      } else {
        kbl <- kbl %>%
          kableExtra::kable_styling(font_size = font_size)
      }

      col_lab <- names(tbl)[2:5]
      # label1 <- ifelse(
      #   str_detect(col_lab, "Young"),
      #   paste0("$\\\\text{Age} < ", private$age_cut, "$"),
      #   paste0("$", private$age_cut, " \\\\le \\\\text{Age}$")
      # )
      # label1 <- c(" ", label1)
      # label1_structure <- rle(label1)
      # label1_rle <- label1_structure$lengths
      # names(label1_rle) <- label1_structure$values

      # kbl <- kbl %>% kableExtra::add_header_above(label1_rle, escape = FALSE)

      label2 <- ifelse(str_detect(col_lab, "Female"), "Females", "Males")
      label2 <- c(" ", label2)
      label2_structure <- rle(label2)
      label2_rle <- label2_structure$lengths
      names(label2_rle) <- label2_structure$values

      kbl <- kbl %>% kableExtra::add_header_above(label2_rle)

      mod <- unique(tbl$model)

      for (i in mod) {
        if (is.na(i)) next
        row_mod <- which(tbl$model == i)
        min_row_mod <- min(row_mod)
        max_row_mod <- max(row_mod)

        kbl <- kbl %>%
          group_rows(
            i,
            start_row = min_row_mod,
            end_row = max_row_mod
          )
      }

      kbl %>%
        kableExtra::footnote(
          general_title = "",
          general = paste(
            "\\\\emph{Note}: * $p < 0.1$, ** $p < 0.05$, *** $p < 0.01$.",
            "The robust standard errors are in parentheses.",
            notes
          ),
          threeparttable = TRUE,
          escape = FALSE
        )
    }
  ),
  private = list(
    est = NULL,
    age_cut = NULL,
    reg_msummary = function(output, digit = 2, ...) {
      fit <- pull(private$est, fit) %>% map(~ .$lm_robust)
      coef_map <- c(
        "treatB" = "Treatment B",
        "treatC" = "Treatment C",
        "treatD" = "Treatment D",
        "groupOlder female" = "Older female",
        "groupYoung male" = "Young male",
        "groupOlder male" = "Older male",
        "treatB:groupOlder female" = "Treatment B $\\times$ Older female",
        "treatC:groupOlder female" = "Treatment C $\\times$ Older female",
        "treatD:groupOlder female" = "Treatment D $\\times$ Older female",
        "treatB:groupYoung male" = "Treatment B $\\times$ Young male",
        "treatC:groupYoung male" = "Treatment C $\\times$ Young male",
        "treatD:groupYoung male" = "Treatment D $\\times$ Young male",
        "treatB:groupOlder male" = "Treatment B $\\times$ Older male",
        "treatC:groupOlder male" = "Treatment C $\\times$ Older male",
        "treatD:groupOlder male" = "Treatment D $\\times$ Older male"
      )
      stars <- c("***" = .01, "**" = .05, "*" = .1)
      gof_omit <- "R2|AIC|BIC|Log|Std|FE|se_type"
      align <- paste(c("l", rep("c", nrow(private$est))), collapse = "")

      add_tab <- data.frame(
        rbind(
          c("Individual-level covariates", private$est$covs_i),
          c("Week-level covariates", private$est$covs_w),
          c("Prefecture-level covariates", private$est$covs_p),
          c("Region$\\times$Week (in Dec. and Jan.) FE", private$est$covs_fe)
        )
      )
      attr(add_tab, "position") <- 31:34

      args <- list(
        models = fit,
        output = output,
        coef_map = coef_map,
        stars = stars,
        gof_omit = gof_omit,
        align = align,
        add_rows = add_tab,
        fmt = digit
      )

      if (!missing(...)) args <- append(args, list(...))
      do.call("modelsummary", args)
    },
    lh_msummary = function(y, digit = 2) {
      use_est <- subset(private$est, outcome == y)
      if (nrow(use_est) == 0) {
        stop(
          paste(
            "Choose an outcome from the set {",
            paste(unique(as.character(private$est$outcome)), collapse = ", "),
            "}"
          )
        )
      }
      fit <- pull(use_est, fit) %>% map(~ .$lh)

      fit %>%
        modelsummary(
          coef_map = c(
            "treatB" = "B_YoungFemale",
            "treatC" = "C_YoungFemale",
            "treatD" = "D_YoungFemale",
            "treatB + treatB:groupOlder female" = "B_OlderFemale",
            "treatC + treatC:groupOlder female" = "C_OlderFemale",
            "treatD + treatD:groupOlder female" = "D_OlderFemale",
            "treatB + treatB:groupYoung male" = "B_YoungMale",
            "treatC + treatC:groupYoung male" = "C_YoungMale",
            "treatD + treatD:groupYoung male" = "D_YoungMale",
            "treatB + treatB:groupOlder male" = "B_OlderMale",
            "treatC + treatC:groupOlder male" = "C_OlderMale",
            "treatD + treatD:groupOlder male" = "D_OlderMale"
          ),
          stars = c("***" = .01, "**" = .05, "*" = .1),
          output = "data.frame",
          fmt = digit
        ) %>%
        filter(part == "estimates") %>%
        mutate(
          treat = str_extract(term, "^[^_]+"),
          subset = str_extract(term, "(?<=_)[^_]+$")
        ) %>%
        select(-term, -part)
    }
  )
)