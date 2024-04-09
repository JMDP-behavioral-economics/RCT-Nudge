library(here)
library(R6)
source(here("R/misc.r"))

Flow <- R6::R6Class("Flow",
  public = list(
    data = NULL,
    initialize = function(reply_data,
                          covariate,
                          se,
                          cluster,
                          fe) {
      self$data <- reply_data
      rhs <- c("treat", covariate)

      if (!missing(fe)) {
        rhs <- append(rhs, sapply(fe, function(x) paste0("factor(", x, ")")))
      }

      private$model <- reformulate(rhs, "value")
      private$rhs <- rhs
      private$se_type <- se
    },
    plot = function(...,
                    label_list,
                    xlim = c(0, 40),
                    ylab = "Cumulative response rate (%)") {
      
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
        mutate(days_reply = if_else(value == 0, 99999, days_reply)) %>%
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
        my_theme_classic() +
        theme(
          legend.key.size = grid::unit(1.5, "cm"),
          legend.position = "bottom"
        )
    },
    fit = function(days, ...) {
      dt <- self$data
      model <- private$model

      if (!missing(...)) {
        cond <- enquos(...)
        
        pat_sep <- c()
        for (i in seq(length(cond))) {
          col_label <- paste0("cond", i)
          dt[, col_label] <- eval_tidy(cond[[i]], dt)
          pat_sep <- append(pat_sep, all.vars(cond[[i]]))
        }
        
        pat <- paste(pat_sep, collapse = "|")
        remove_vars <- private$rhs[str_detect(private$rhs, pat)]
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
            mutate(value = case_when(
              value == 0 ~ 0,
              days_reply > x ~ 0,
              TRUE ~ 1
            )) %>%
            group_by(across(starts_with("cond"))) %>%
            nest() %>%
            mutate(
              day = x,
              fit = map(data, ~ private$call_lm(., model))
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
      
      FlowFit$new(plotdt)
    }
  ),
  private = list(
    model = NULL,
    rhs = NULL,
    se_type = NULL,
    cluster = NULL,
    call_lm = function(data, model) {
      if (is.null(private$cluster)) {
        lm_robust(model, data, se_type = private$se_type)
      } else {
        g <- data[, cluster, drop = TRUE]
        lm_robust(model, data, clusters = g, se_type = private$se_type)
      }
    }
  )
)

FlowFit <- R6::R6Class("FlowFit",
  public = list(
    data = NULL,
    initialize = function(data) self$data <- data,
    plot = function(...) {
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
        scale_x_continuous(breaks = c(1, seq(5, 80, by = 5))) +
        facet_wrap(~term, ncol = 2, scales = "free_x") +
        labs(
          x = "Days after sending notification",
          y = "Estimated Effects (95%CI)"
        ) +
        my_theme_classic()
    }
  ),
  private = list()
)