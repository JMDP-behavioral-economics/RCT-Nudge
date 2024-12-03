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
        mutate(covariate = if_else(model != "1", "X", ""))

      LmFit$new(est, model_type)
    },
    fit_subset_by_gender = function(scale = 1, covariates = TRUE) {
      model <- if (!covariates) {
        private$model$unctrl
      } else {
        update(private$model$ctrl1, . ~ . - male)
      }

      est <- self$data %>%
        mutate(value = value * scale) %>%
        group_by(outcome, male) %>%
        nest() %>%
        mutate(
          fit = private$call_lm(data, model, private$se_type),
          avg = map_chr(
            data,
            ~ with(
              subset(., treat == private$ctrl_arm),
              sprintf("Ctrl Avg = %1.2f", mean(value))
            )
          )
        )

      LmSubsetGender$new(est)
    },
    fit_subset_by_gender_age = function(age_cut = 30,
                                        scale = 1,
                                        covariates = TRUE)
    {
      model <- if (!covariates) {
        private$model$unctrl
      } else {
        update(private$model$ctrl1, . ~ . - male - age - I(age^2))
      }

      mean_age <- private$mean_age

      est <- self$data %>%
        mutate(value = value * scale) %>%
        mutate(young = if_else(age < age_cut - mean_age, 1, 0)) %>%
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
          )
        )

      LmSubset$new(est, age_cut)
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

LmCluster <- R6::R6Class("LmCluster",
  public = list(
    data = NULL,
    initialize = function(data, demean_covariate, se, cluster) {
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
    fit_subset_by_gender = function(scale = 1, covariates = TRUE) {
      model <- if (!covariates) {
        private$model$unctrl
      } else {
        update(private$model$ctrl1, . ~ . - male)
      }

      est <- self$data %>%
        mutate(value = value * scale) %>%
        group_by(outcome, male) %>%
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

      LmSubsetGender$new(est)
    },
    fit_subset_by_gender_age = function(age_cut = 30,
                                        scale = 1,
                                        covariates = TRUE)
    {
      model <- if (!covariates) {
        private$model$unctrl
      } else {
        update(private$model$ctrl1, . ~ . - male - age - I(age^2))
      }

      mean_age <- private$mean_age

      est <- self$data %>%
        mutate(value = value * scale) %>%
        mutate(young = if_else(age < age_cut - mean_age, 1, 0)) %>%
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
    }
  ),
  private = list(
    ctrl_arm = "",
    model = list(),
    se_type = "",
    cluster = NULL,
    mean_age = 0,
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

        attr(add_tab, "position") <- seq(
          length(private$coef_map_lm) * 2 + 1,
          length.out = nrow(add_tab)
        )
      } else {
        add_tab <- data.frame(
          rbind(c("Covariates", res$covariate))
        )

        attr(add_tab, "position") <- seq(
          length(private$coef_map_lm) * 2 + 1,
          length.out = nrow(add_tab)
        )
      }

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
          fmt = digit
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
                        hold = FALSE)
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
          term = if_else(statistic == "estimate", str_remove(term, "_.*"), "")
        ) %>%
        select(-statistic) %>%
        select(group, term, everything()) %>%
        bind_rows(add_tab)

      kbl <- tbl2 %>%
        knitr::kable(
          caption = title,
          col.names = c("Group", "Treatment", names(tbl)[-c(1:2)]),
          align = paste(c("ll", rep("c", nrow(res))), collapse = "")
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
            "The robust standard errors are in parentheses.",
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
        labs(x = "Treatment", y = "Sample average") +
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
          c("Covariates", private$est$covs)
        )
      )
      attr(add_tab, "position") <- 7:8

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
      private$age_cut <- age_cut

    },
    get_est = function() private$est,
    kable = function( title = "",
                      notes = "",
                      font_size = 9,
                      digits = 2,
                      hold = FALSE,
                      debug = FALSE,
                      ...)
    {
      if (debug) {
        young_label <- "Young"
        older_label <- "Older"
      } else {
        young_label <- paste0("$\\\\text{Age} < ", private$age_cut, "$")
        older_label <- paste0("$", private$age_cut, " \\\\le \\\\text{Age}$")
      }

      est <- private$est %>%
        arrange(male, desc(young)) %>%
        mutate(
          male = if_else(male == 1, "Males", "Females"),
          young = if_else(young == 1, young_label, older_label)
        )

      addtab <- data.frame(
        rbind(c("Control average", str_remove(est$avg, "Ctrl Avg = ")))
      )

      attr(addtab, "position") <- 7

      kbl <- est %>%
        pull(fit) %>%
        modelsummary(
          title = title,
          coef_map = c(
            "treatB" = "Treatment B",
            "treatC" = "Treatment C",
            "treatD" = "Treatment D"
          ),
          stars = c("***" = .01, "**" = .05, "*" = .1),
          gof_omit = "R2|AIC|BIC|Log|Std|FE|se_type",
          align = paste(c("l", rep("c", nrow(est))), collapse = ""),
          add_rows = addtab,
          fmt = digits
        )

      if (hold) {
        kbl <- kbl %>%
          kableExtra::kable_styling(font_size = font_size, latex_options = "HOLD_position")
      } else {
        kbl <- kbl %>%
          kableExtra::kable_styling(font_size = font_size)
      }

      label <- list(
        y = rle(c(" ", as.character(est$outcome))),
        gender = rle(c(" ", est$male)),
        young = rle(c(" ", est$young))
      )

      label <- label %>%
        map(function(x) {
          vec <- x$lengths
          names(vec) <- x$values
          return(vec)
        })

      kbl <- kbl %>%
        kableExtra::add_header_above(label$young, escape = FALSE) %>%
        kableExtra::add_header_above(label$gender) %>%
        kableExtra::add_header_above(label$y)

      kbl %>%
        kableExtra::footnote(
          general_title = "",
          general = paste("\\\\emph{Note}:", notes),
          threeparttable = TRUE,
          escape = FALSE
        )
    },
    stack_kable = function( title = "",
                            notes = "",
                            font_size = 9,
                            digit = 2,
                            hold = FALSE,
                            ...)
    {
      est <- private$est
      tbl <- private$stack_reg_table(est, digit)

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
    age_cut = NULL,
    stack_reg_table = function(x, digit = 2) {
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

LmSubsetGender <- R6::R6Class("LmSubsetGender",
  public = list(
    initialize = function(est) private$est <- est,
    get_est = function() private$est,
    kable = function( title = "",
                      notes = "",
                      font_size = 9,
                      digits = 2,
                      hold = FALSE)
    {
      est <- private$est %>%
        arrange(outcome, male) %>%
        mutate(male = if_else(male == 1, "Males", "Females"))

      addtab <- data.frame(
        rbind(c("Control average", str_remove(est$avg, "Ctrl Avg = ")))
      )

      attr(addtab, "position") <- 7

      kbl <- est %>%
        pull(fit) %>%
        modelsummary(
          title = title,
          coef_map = c(
            "treatB" = "Treatment B",
            "treatC" = "Treatment C",
            "treatD" = "Treatment D"
          ),
          stars = c("***" = .01, "**" = .05, "*" = .1),
          gof_omit = "R2|AIC|BIC|Log|Std|FE|se_type",
          align = paste(c("l", rep("c", nrow(est))), collapse = ""),
          add_rows = addtab,
          fmt = digits
        )

      if (hold) {
        kbl <- kbl %>%
          kableExtra::kable_styling(font_size = font_size, latex_options = "HOLD_position")
      } else {
        kbl <- kbl %>%
          kableExtra::kable_styling(font_size = font_size)
      }

      label <- list(
        y = rle(c(" ", as.character(est$outcome))),
        gender = rle(c(" ", est$male))
      )

      label <- label %>%
        map(function(x) {
          vec <- x$lengths
          names(vec) <- x$values
          return(vec)
        })

      kbl <- kbl %>%
        kableExtra::add_header_above(label$gender) %>%
        kableExtra::add_header_above(label$y)

      kbl %>%
        kableExtra::footnote(
          general_title = "",
          general = paste("\\\\emph{Note}:", notes),
          threeparttable = TRUE,
          escape = FALSE
        )
    }
  ),
  private = list(
    est = NULL
  )
)
