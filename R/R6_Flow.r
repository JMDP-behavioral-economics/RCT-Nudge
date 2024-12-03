library(here)
library(R6)
source(here("R/misc.r"))

Flow <- R6::R6Class("Flow",
  public = list(
    data = NULL,
    initialize = function(data,
                          se,
                          cluster)
    {
      private$ctrl_arm <- levels(data$treat)[1]
      self$data <- dt
      private$se_type <- se
      private$cluster <- cluster
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
    lm_time_segment = function( cut_days = c(7, 11),
                                demean_covariate = TRUE,
                                hide_message = TRUE)
    {
      dt <- self$data %>%
        mutate(
          period_1 = case_when(
            is.na(days_reply) ~ 0,
            days_reply <= cut_days[1] ~ value,
            TRUE ~ 0
          ),
          period_2 = case_when(
            is.na(days_reply) ~ 0,
            days_reply <= cut_days[1] ~ 0,
            days_reply <= cut_days[2] ~ value,
            TRUE ~ 0
          ),
          period_3 = case_when(
            is.na(days_reply) ~ 0,
            days_reply <= cut_days[2] ~ 0,
            TRUE ~ value
          )
        ) %>%
        pivot_longer(
          period_1:period_3,
          names_to = "period",
          names_prefix = "period_",
          values_to = "flow_value"
        )

      start_days <- c(
        min(dt2$days_reply, na.rm = TRUE), cut_days[1:2] + 1
      )

      end_days <- c(
        cut_days[1:2], max(dt2$days_reply, na.rm = TRUE)
      )

      range_days <- paste0(start_days, "--", end_days, " days")

      dt2 <- dt %>%
        mutate(
          period = as.numeric(period),
          period = factor(period, labels = range_days)
        ) %>%
        select(-outcome, -value) %>%
        rename(outcome = period, value = flow_value)

      Lm$new(dt2, demean_covariate, private$se, private$cluster, hide_message)
    }
  ),
  private = list(
    se_type = NULL,
    cluster = NULL,
    ctrl_arm = NULL
  )
)