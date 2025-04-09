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
        ctrl1 = reformulate(
          c("treat", use_x),
          "value"
        )
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
                    interaction_of_gender = FALSE,
                    interaction_of_gender_age = FALSE,
                    age_cut = 30)
    {
      est_dt <- self$data %>%
        mutate(value = value * scale)

      model_type <- if (interaction_of_gender) {
        "hetero-gender"
      } else if (interaction_of_gender_age) {
        "hetero-gender-age"
      } else {
        "ate"
      }

      if (model_type == "hetero-gender-age") {
        mean_age <- private$mean_age

        est_dt <- est_dt %>%
          mutate(
            young = if_else(age < age_cut - mean_age, 1, 0),
            group = case_when(
              male == 0 & young == 1 ~ 1,
              male == 0 & young == 0 ~ 2,
              male == 1 & young == 1 ~ 3,
              male == 1 & young == 0 ~ 4
            ),
            group = factor(
              group,
              labels = c(
                "Young female", "Older female",
                "Young male", "Older male"
              )
            )
          )
      } else if (model_type == "hetero-gender") {
        est_dt <- est_dt %>%
          mutate(
            group = male,
            group = factor(group, labels = c("Female", "Male"))
          )
      }

      if (model_type == "ate") {
        lh <- NULL
        model <- private$model
      } else {
        if (model_type == "hetero-gender") {
          lh <- c(
            "treatB",
            "treatC",
            "treatD",
            "treatB + treatB:groupMale",
            "treatC + treatC:groupMale",
            "treatD + treatD:groupMale"
          )

          use_x <- private$covariates
          use_x_2 <- use_x[!str_detect(use_x, "male")]
        } else {
          lh <- c(
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

          use_x <- private$covariates
          use_x_2 <- use_x[!str_detect(use_x, "male|age")]
        }
        use_x_2_int <- paste0(use_x_2, ":group")

        model <- list(
          unctrl = reformulate("treat * group", "value"),
          ctrl = reformulate(c("treat * group", use_x_2, use_x_2_int), "value")
        )
      }

      est <- est_dt %>%
        group_by(outcome) %>%
        nest() %>%
        mutate(
          avg = map_dbl(data, ~ private$ctrl_mean(., model_type)),
          fit_1 = map(data, ~ private$call_lh(., model$unctrl, lh)),
          fit_2 = map(data, ~ private$call_lh(., model$ctrl, lh))
        ) %>%
        ungroup() %>%
        pivot_longer(
          fit_1:fit_2,
          names_prefix = "fit_",
          names_to = "model",
          values_to = "fit"
        ) %>%
        mutate(covariate = if_else(model != "1", "X", "")) %>%
        arrange(outcome)

      LmFit$new(est, model_type)
    },
    fit_subset = function(scale = 1,
                          gender_age = FALSE,
                          age_cut = 30,
                          covariates = TRUE)
    {
      model <- if (!covariates) {
        private$model$unctrl
      } else {
        if (!gender_age) {
          update(private$model$ctrl, . ~ . - male)
        } else {
          update(private$model$ctrl, . ~ . - male - age - I(age^2))
        }
      }

      est_dt <- self$data %>%
        mutate(value = value * scale)

      if (!gender_age) {
        est_dt <- est_dt %>%
          mutate(
            group = male,
            group = factor(group, labels = c("Female", "Male"))
          )
      } else {
        mean_age <- private$mean_age

        est_dt <- est_dt %>%
          mutate(
            young = if_else(age < age_cut - mean_age, 1, 0),
            group = case_when(
              male == 0 & young == 1 ~ 1,
              male == 0 & young == 0 ~ 2,
              male == 1 & young == 1 ~ 3,
              male == 1 & young == 0 ~ 4
            ),
            group = factor(
              group,
              labels = c(
                "Young female", "Older female",
                "Young male", "Older male"
              )
            )
          )
      }

      est <- est_dt %>%
        group_by(outcome, group) %>%
        nest() %>%
        mutate(
          avg = map_dbl(data, ~ private$ctrl_mean(., "ate")),
          fit = map(data, ~ private$call_lh(., model))
        )

      LmFitSubset$new(est)
    }
  ),
  private = list(
    ctrl_arm = "",
    model = list(),
    se_type = "",
    cluster = NULL,
    covariates = "",
    mean_age = 0,
    call_lh = function(data, model, lh = NULL) {
      if (is.null(private$cluster)) {
        if (is.null(lh)) {
          lm_robust(model, data = data, se_type = private$se_type)
        } else {
          lh_robust(model, data = data, se_type = private$se_type, linear_hypothesis = lh)
        }
      } else {
        g <- data[, private$cluster, drop = TRUE]
        if (is.null(lh)) {
          lm_robust(model, data = data, se_type = private$se_type, clusters = g)
        } else {
          lh_robust(model, data = data, se_type = private$se_type, clusters = g, linear_hypothesis = lh)
        }
      }
    },
    ctrl_mean = function(data, model_type) {
      if (model_type == "hetero-gender") {
        with(
          subset(data, treat == private$ctrl_arm & male == 0),
          mean(value)
        )
      } else if (model_type == "hetero-gender-age") {
        with(
          subset(data, treat == private$ctrl_arm & group == levels(data$group)[1]),
          mean(value)
        )
      } else {
        with(
          subset(data, treat == private$ctrl_arm),
          mean(value)
        )
      }
    }
  )
)

LmFit <- R6::R6Class("LmFit",
  public = list(
    initialize = function(est, model_type) {
      private$est <- est
      private$type <- model_type
      private$coef_map_lm <- if (model_type == "ate") {
        c(
          "treatB" = "Treatment B",
          "treatC" = "Treatment C",
          "treatD" = "Treatment D"
        )
      } else if (model_type == "hetero-gender") {
        c(
          "treatB" = "Treatment B",
          "treatC" = "Treatment C",
          "treatD" = "Treatment D",
          "groupMale" = "Male",
          "treatB:groupMale" = "Treatment B $\\times$ Male",
          "treatC:groupMale" = "Treatment C $\\times$ Male",
          "treatD:groupMale" = "Treatment D $\\times$ Male"
        )
      } else {
        c(
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
      }
      private$coef_map_lh <- if (model_type == "hetero-gender") {
        c(
          "treatB" = "Treatment B_Female",
          "treatC" = "Treatment C_Female",
          "treatD" = "Treatment D_Female",
          "treatB + treatB:groupMale" = "Treatment B_Male",
          "treatC + treatC:groupMale" = "Treatment C_Male",
          "treatD + treatD:groupMale" = "Treatment D_Male"
        )
      } else {
        c(
          "treatB" = "Treatment B_Young female",
          "treatC" = "Treatment C_Young female",
          "treatD" = "Treatment D_Young female",
          "treatB + treatB:groupOlder female" = "Treatment B_Older female",
          "treatC + treatC:groupOlder female" = "Treatment C_Older female",
          "treatD + treatD:groupOlder female" = "Treatment D_Older female",
          "treatB + treatB:groupYoung male" = "Treatment B_Young male",
          "treatC + treatC:groupYoung male" = "Treatment C_Young male",
          "treatD + treatD:groupYoung male" = "Treatment D_Young male",
          "treatB + treatB:groupOlder male" = "Treatment B_Older male",
          "treatC + treatC:groupOlder male" = "Treatment C_Older male",
          "treatD + treatD:groupOlder male" = "Treatment D_Older male"
        )
      }
    },
    get_est = function() private$est,
    kable_reg = function( title = "",
                          notes = "",
                          font_size = 9,
                          digit = 2,
                          hold = FALSE)
    {
      res <- private$est

      if (private$type == "ate") {
        avg_format <- paste0("%1.", digit, "f")

        add_tab <- data.frame(
          rbind(
            c("Control average", sprintf(avg_format, res$avg)),
            c("Covariates", res$covariate)
          )
        )
      } else {
        add_tab <- data.frame(
          rbind(c("Covariates", res$covariate))
        )
      }

      attr(add_tab, "position") <- seq(
        length(private$coef_map_lm) * 2 + 1,
        length.out = nrow(add_tab)
      )

      fit <- pull(res, fit)
      if (private$type != "ate") fit <- map(fit, ~ .$lm_robust)

      kbl <- fit %>%
        modelsummary(
          title = title,
          coef_map = private$coef_map_lm,
          stars = c("***" = .01, "**" = .05, "*" = .1),
          gof_omit = "R2|AIC|BIC|Log|Std|FE|se_type",
          align = paste(c("l", rep("c", nrow(res))), collapse = ""),
          add_rows = add_tab,
          fmt = digit,
          escape = FALSE
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
    kable_lh = function(title = "",
                        notes = "",
                        font_size = 9,
                        digit = 2,
                        hold = FALSE,
                        output_data_frame = FALSE)
    {
      if (private$type == "ate") stop("No linear combination test!")

      res <- private$est
      add_tab <- data.frame(rbind(c("", "Covariates", res$covariate)))
      names(add_tab) <- c("group", "term", paste0("(", seq(length(res$covariate)), ")"))

      attr(add_tab, "position") <- seq(
        length(private$coef_map_lh) * 2 + 1,
        length.out = nrow(add_tab)
      )

      fit <- res %>%
        pull(fit) %>%
        map(~ .$lh)

      tbl <- fit %>%
        modelsummary(
          coef_map = private$coef_map_lh,
          estimate = "{estimate} ({std.error}){stars}",
          statistic = NULL,
          stars = c("***" = .01, "**" = .05, "*" = .1),
          fmt = digit,
          output = "data.frame"
        ) %>%
        filter(part == "estimates") %>%
        select(-part)

      tbl2 <- tbl %>%
        mutate(
          group = str_split(term, "_", simplify = TRUE)[, 2],
          group = if_else(str_detect(term, "Treatment B") & statistic == "estimate", group, ""),
          term = str_split(term, "_", simplify = TRUE)[, 1],
          term = if_else(statistic == "estimate", str_remove(term, "Treatment "), "")
        ) %>%
        select(-statistic) %>%
        select(group, term, everything()) %>%
        bind_rows(add_tab)

      if (output_data_frame) {
        tbl2
      } else {
        kbl <- tbl2 %>%
          knitr::kable(
            caption = title,
            col.names = c("Group", "Treatment", names(tbl)[-c(1:2)]),
            align = paste(c("ll", rep("c", nrow(res))), collapse = ""),
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

        label <- c(" ", " ", as.character(res$outcome))
        label_structure <- rle(label)
        lab1 <- label_structure$lengths
        names(lab1) <- label_structure$values

        kbl <- kbl %>%
          kableExtra::add_header_above(lab1)

        kbl <- kbl %>%
          kableExtra::row_spec(nrow(tbl), hline_after = TRUE)

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
      }
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
                        ylim = c(0, 100),
                        ybreaks = seq(0, 100, by = 10),
                        base_size = 15)
    {
      wocov <- subset(private$est, covariate == "")

      if (private$type == "ate") {
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

      if (private$type == "ate") {
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
        labs(x = xlab, y = "Sample average") +
        my_theme_classic(size = base_size, strip_hjust = 0.5)

      if (private$type == "ate") {
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

LmFitSubset <- R6::R6Class("LmFitSubset",
  public = list(
    initialize = function(est) private$est <- est,
    get_est = function() private$est,
    kable = function(title = "",
                    notes = "",
                    font_size = 9,
                    digit = 2,
                    hold = FALSE)
    {
      est <- private$est

      coef_map <- c(
        "treatB" = "Treatment B",
        "treatC" = "Treatment C",
        "treatD" = "Treatment D"
      )

      avg_format <- paste0("%1.", digit, "f")

      tbl_list <- list()

      for (g in levels(est$group)) {
        use <- est[est$group == g, ]

        add_tab <- data.frame(
          rbind(
            c("Control average", sprintf(avg_format, use$avg))
          )
        )
        names(add_tab) <- c("term", paste0("(", seq(nrow(use)), ")"))

        tbl <- use %>%
          pull(fit) %>%
          modelsummary(
            coef_map = coef_map,
            estimate = "{estimate} ({std.error}){stars}",
            statistic = NULL,
            stars = c("***" = .01, "**" = .05, "*" = .1),
            gof_omit = "R2|AIC|BIC|Log|Std|FE|se_type",
            add_rows = add_tab,
            fmt = digit,
            output = "data.frame"
          )

        tbl_part <- list(
          estimates = tbl[tbl$part == "estimates", ],
          manual = tbl[tbl$part == "manual", ],
          gof = tbl[tbl$part == "gof", ]
        )

        tbl2 <- tbl_part %>%
          reduce(bind_rows) %>%
          mutate(
            term = if_else(statistic == "std.error", "", term),
            group = g
          ) %>%
          select(-part, -statistic) %>%
          select(group, everything())

        tbl_list <- append(tbl_list, list(tbl2))
      }

      tbl3 <- tbl_list %>%
        reduce(bind_rows)

      kbl <- tbl3 %>%
        select(-group) %>%
        knitr::kable(
          caption = title,
          col.names = c("", names(tbl3)[-c(1:2)]),
          align = paste(c("l", rep("c", ncol(tbl3) - 2)), collapse = ""),
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

      label <- c(" ", as.character(unique(est$outcome)))
      label_structure <- rle(label)
      lab1 <- label_structure$lengths
      names(lab1) <- label_structure$values

      kbl <- kbl %>% kableExtra::add_header_above(lab1)

      for (g in levels(est$group)) {
        pos <- which(tbl3$group == g)
        start <- min(pos)
        end <- max(pos)

        kbl <- kbl %>% group_rows(g, start, end)
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
    est = NULL
  )
)

