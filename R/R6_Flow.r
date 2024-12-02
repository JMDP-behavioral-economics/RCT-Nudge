library(here)
library(R6)
source(here("R/misc.r"))

Flow <- R6::R6Class("Flow",
  public = list(
    data = NULL,
    initialize = function(data,
                          demean_covariate,
                          se,
                          cluster)
    {
      private$ctrl_arm <- levels(data$treat)[1]

      if (demean_covariate) {
        private$mean_age <- mean(data$age)

        dt <- data %>%
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
        unctrl = reformulate("treat", "flow_value"),
        ctrl = reformulate(c("treat", use_x), "flow_value")
      )

      private$se_type <- se
    },
    plot_cumulative = function( ...,
                                label_list,
                                xlim = c(0, 40),
                                ylab = "Cumulative response rate (%)",
                                base_size = 15)
    {

      dt <- self$data %>%
        select(treat, days_reply, value)

      if (!missing(...)) {
        cond <- enquos(...)
        for (i in 1:length(cond)) {
          col_lab <- paste0("cond", i)
          dt[, col_lab] <- eval_tidy(cond[[i]], self$data)
          if (!missing(label_list)) {
            dt[, col_lab] <- factor(
              dt[, col_lab, drop = TRUE],
              levels = c(FALSE, TRUE),
              labels = label_list[[i]]
            )
          }
        }
      }

      plotdt <- dt %>%
        mutate(days_reply = if_else(is.na(days_reply), 99999, days_reply)) %>%
        group_by(treat, days_reply, across(starts_with("cond"))) %>%
        summarize(n = n()) %>%
        group_by(treat, across(starts_with("cond"))) %>%
        arrange(days_reply) %>%
        mutate(
          cum = cumsum(n),
          fraq = 100 * cum / sum(n),
          fraq = if_else(
            days_reply == 99999,
            100 * (cum - n) / sum(n),
            fraq
          )
        )

      out <- plotdt %>%
        ggplot(aes(x = days_reply, y = fraq, linetype = treat, group = treat)) +
        geom_line(linewidth = 1) +
        scale_linetype_manual(values = c(1, 2, 4, 3)) +
        coord_cartesian(xlim = xlim, ylim = c(0, 100)) +
        scale_x_continuous(breaks = seq(0, 40, 5)) +
        scale_y_continuous(breaks = seq(0, 100, 10))

      if (!missing(...)) {
        wrap_formula <- reformulate(names(dt)[str_detect(names(dt), "cond")])
        out <- out +
          facet_wrap(wrap_formula, scales = "free")
      }

      out +
        labs(
          x = "Response speed (days)",
          y = ylab,
          linetype = "Treatment"
        ) +
        my_theme_classic(size = base_size) +
        theme(
          legend.key.size = grid::unit(1.5, "cm"),
          legend.position = "bottom"
        )
    },
    plot_density = function(upper_days = 99999,
                            subset_by_gender = FALSE,
                            subset_by_gender_age = FALSE,
                            age_cut = 30,
                            xlab = "Days",
                            ylab = "Density",
                            dlab = "Experimental arm",
                            base_size = 15,
                            x_breaks = seq(0, 100, by = 10))
    {
      if (subset_by_gender) {
        dt <- self$data %>%
          mutate(
            group = male + 1,
            group = factor(group, labels = c("Females", "Males"))
          )
      } else if (subset_by_gender_age) {
        mean_age <- private$mean_age

        dt <- self$data %>%
          mutate(
            group = case_when(
              male == 0 & age < age_cut - mean_age ~ 1,
              male == 0 & age >= age_cut - mean_age ~ 2,
              male == 1 & age < age_cut - mean_age ~ 3,
              male == 1 & age >= age_cut - mean_age ~ 4
            ),
            group = factor(
              group,
              labels = c(
                "Young females", "Older females",
                "Young males", "Older males"
              )
            )
          )
      } else {
        dt <- self$data %>%
          mutate(group = "Full sample")
      }

      dt %>%
        filter(0 < days_reply & days_reply < upper_days) %>%
        ggplot(aes(x = days_reply)) +
          geom_density(aes(color = treat, linetype = treat), linewidth = .7) +
          scale_color_manual(
            values = c("grey30", "#00468BB2", "#ED0000B2", "#42B540B2")
          ) +
          scale_x_continuous(breaks = x_breaks) +
          facet_wrap(~ group, ncol = 2, scales = "free_x") +
          labs(
            x = xlab,
            y = ylab,
            color = dlab,
            linetype = dlab
          ) +
          my_theme_classic(size = base_size) +
          theme(legend.position = "bottom")
    },
    fit_cumulative = function(days, scale = 1, ...) {
      dt <- self$data
      model <- private$model$ctrl

      if (!missing(...)) {
        rhs <- labels(terms(model))
        cond <- enquos(...)

        pat_sep <- c()
        for (i in seq(length(cond))) {
          col_label <- paste0("cond", i)
          dt[, col_label] <- eval_tidy(cond[[i]], dt)
          pat_sep <- append(pat_sep, all.vars(cond[[i]]))
        }

        pat <- paste(pat_sep, collapse = "|")
        remove_vars <- rhs[str_detect(rhs, pat)]
        remove_formula <- paste0(". ~ . -", paste(remove_vars, collapse = "-"))
        model <- update(model, as.formula(remove_formula))
      }

      cat("\n")
      cat("We will estimate following model:\n")
      print(model)
      cat("\n")

      est <- days %>%
        map(function(x) {
          dt %>%
            mutate(flow_value = case_when(
              value == 0 ~ 0,
              days_reply > x ~ 0,
              TRUE ~ 1 * scale
            )) %>%
            group_by(across(starts_with("cond"))) %>%
            nest() %>%
            mutate(
              day = x,
              fit = map(data, ~ private$call_lh(., model))
            )
        }) %>%
        reduce(bind_rows)

      plotdt <- est %>%
        mutate(
          tidy = map(fit, tidy),
          tidy = map(tidy, ~ subset(.x, str_detect(term, "treat"))),
          tidy = map(tidy, ~ dplyr::select(.x, -outcome))
        ) %>%
        dplyr::select(-data, -fit) %>%
        unnest(cols = tidy) %>%
        mutate(
          term = str_replace(term, "treat", "Experimental Arm ")
        )

      FlowFitCumulative$new(plotdt)
    },
    fit_segment = function( cut_days = c(7, 11),
                            interaction_of_gender = FALSE,
                            interaction_of_gender_age = FALSE,
                            age_cut = 30,
                            scale = 1)
    {
      est_dt <- self$data %>%
        mutate(
          period_1 = case_when(
            is.na(days_reply) ~ 0,
            days_reply <= cut_days[1] ~ value * scale,
            TRUE ~ 0
          ),
          period_2 = case_when(
            is.na(days_reply) ~ 0,
            days_reply <= cut_days[1] ~ 0,
            days_reply <= cut_days[2] ~ value * scale,
            TRUE ~ 0
          ),
          period_3 = case_when(
            is.na(days_reply) ~ 0,
            days_reply <= cut_days[2] ~ 0,
            TRUE ~ value * scale
          )
        ) %>%
        pivot_longer(
          period_1:period_3,
          names_to = "period",
          names_prefix = "period_",
          values_to = "flow_value"
        )

      if (interaction_of_gender_age) {
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

      if (interaction_of_gender) {
        lh <- c(
          "treatB",
          "treatC",
          "treatD",
          "treatB + treatB:male",
          "treatC + treatC:male",
          "treatD + treatD:male"
        )

        use_x <- private$covariates
        use_x_2 <- use_x[!str_detect(use_x, "male")]
        use_x_2_int <- paste0(use_x_2, ":male")

        model <- list(
          unctrl = reformulate("treat * male", "flow_value"),
          ctrl = reformulate(
            c("treat * male", use_x_2, use_x_2_int),
            "flow_value"
          )
        )
      } else if (interaction_of_gender_age) {
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
        use_x_2_int <- paste0(use_x_2, ":group")

        model <- list(
          unctrl = reformulate("treat * group", "flow_value"),
          ctrl = reformulate(
            c("treat * group", use_x_2, use_x_2_int),
            "flow_value"
          )
        )
      } else {
        lh <- NULL
        model <- private$model
      }

      est <- est_dt %>%
        group_by(period) %>%
        nest() %>%
        mutate(
          min_days = map_dbl(data, ~ with(., min(days_reply, na.rm = TRUE))),
          max_days = map_dbl(data, ~ with(., max(days_reply, na.rm = TRUE))),
          start_days = case_when(
            period == "1" ~ min_days,
            period == "2" ~ cut_days[1] + 1,
            period == "3" ~ cut_days[2] + 1
          ),
          end_days = case_when(
            period == "1" ~ cut_days[1],
            period == "2" ~ cut_days[2],
            period == "3" ~ max_days
          ),
          avg = map_dbl(
            data,
            ~ private$ref_ctrl_avg(., interaction_of_gender, interaction_of_gender_age)
          ),
          fit_1 = map(data, ~ private$call_lh(., model$unctrl, lh)),
          fit_2 = map(data, ~ private$call_lh(., model$ctrl, lh))
        ) %>%
        select(-min_days, -max_days)

      FlowFitSegment$new(est)
    }
  ),
  private = list(
    covariates = NULL,
    model = NULL,
    se_type = NULL,
    cluster = NULL,
    ctrl_arm = NULL,
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
    ref_ctrl_avg = function(data, interaction_of_gender, interaction_of_gender_age) {
      if (interaction_of_gender) {
        with(
          subset(data, treat == private$ctrl_arm & male == 0),
          mean(flow_value)
        )
      } else if (interaction_of_gender_age) {
        with(
          subset(data, treat == private$ctrl_arm & group == levels(data$group)[1]),
          mean(flow_value)
        )
      } else {
        with(
          subset(data, treat == private$ctrl),
          mean(flow_value)
        )
      }
    }
  )
)

FlowFitCumulative <- R6::R6Class("FlowFitCumulative",
  public = list(
    data = NULL,
    initialize = function(data) self$data <- data,
    plot = function(xlab = "Days after sending notification",
                    ylab = "Estimated Effects (95%CI)",
                    ylim = c(-100, 100),
                    ybreaks = 10,
                    base_size = 15,
                    ...) {
      dt <- self$data

      if (!missing(...)) {
        cond <- list(...)
        for (i in seq(length(cond))) {
          col_lab <- paste0("cond", i)
          dt <- dt[dt[, col_lab] == cond[[i]], ]
        }
      }

      ggplot(dt, aes(x = day, y = estimate, ymin = conf.low, ymax = conf.high)) +
        geom_hline(aes(yintercept = 0), linetype = 2) +
        geom_point(size = 3) +
        geom_line(linewidth = 1) +
        geom_ribbon(alpha = 0.1) +
        scale_y_continuous(
          limits = ylim,
          breaks = seq(ylim[1], ylim[2], by = ybreaks)
        ) +
        scale_x_continuous(breaks = c(1, seq(5, 80, by = 5))) +
        facet_wrap(~term, ncol = 2, scales = "free_x") +
        labs(
          x = xlab,
          y = ylab
        ) +
        my_theme_classic(size=base_size)
    }
  ),
  private = list()
)

FlowFitSegment <- R6::R6Class("FlowFitSegment",
  public = list(
    initialize = function(est) private$est <- est,
    get_est = function() private$est,
    kable = function( title = "",
                      notes = "",
                      font_size = 9,
                      digit = 2,
                      hold = FALSE)
    {
      res <- private$est %>%
        pivot_longer(
          fit_1:fit_2,
          names_to = "model",
          names_prefix = "fit_",
          values_to = "fit"
        ) %>%
        mutate(
          covariate = if_else(model == "2", "X", ""),
          range_days = paste0(start_days, "--", end_days, " days")
        )

      avg_format <- paste0("%1.", digit, "f")

      add_tab <- data.frame(
        rbind(
          c("Control average", sprintf(avg_format, res$avg)),
          c("Covariates", res$covariate)
        )
      )

      attr(add_tab, "position") <- 7:8

      kbl <- res %>%
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

      label <- c(" ", res$range_days)
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

      kbl
    }
  ),
  private = list(
    est = NULL
  )
)