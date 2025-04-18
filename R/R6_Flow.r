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
      self$data <- data
      private$ctrl_arm <- levels(data$treat)[1]
      private$se_type <- se
      private$cluster <- cluster
    },
    plot_cumulative = function( upper_days,
                                subset_by_gender = FALSE,
                                subset_by_gender_age = FALSE,
                                age_cut = 30,
                                xbreaks = seq(0, 100, 5),
                                ylim = c(0, 100),
                                ybreaks = seq(0, 100, 10),
                                xlab = "Days",
                                ylab = "Cumulative response rate (%)",
                                dlab = "Experimental group",
                                base_size = 15)
    {

      if (subset_by_gender) {
        dt <- self$data %>%
          mutate(
            group = male + 1,
            group = factor(group, labels = c("Females", "Males"))
          )
      } else if (subset_by_gender_age) {
        dt <- self$data %>%
          mutate(
            group = case_when(
              male == 0 & age < age_cut ~ 1,
              male == 0 & age >= age_cut ~ 2,
              male == 1 & age < age_cut ~ 3,
              male == 1 & age >= age_cut ~ 4
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

      plotdt <- dt %>%
        mutate(days_reply = if_else(is.na(days_reply), 99999, days_reply)) %>%
          group_by(treat, days_reply, group) %>%
          summarize(
            n = n(),
            value = sum(value)
          ) %>%
          group_by(treat, group) %>%
          arrange(days_reply) %>%
          mutate(
            cum_value = cumsum(value),
            fraq = 100 * cum_value / sum(n)
          ) %>%
          ungroup()

      if (missing(upper_days)) {
        xmax <- max(plotdt[plotdt$days_reply != 99999, ]$days_reply)
      } else {
        xmax <- upper_days
      }

      plotdt %>%
        ggplot(aes(x = days_reply, y = fraq, linetype = treat, group = treat, color = treat)) +
        geom_line(linewidth = 0.7) +
        scale_color_manual(
          values = c("grey30", "#00468BB2", "#ED0000B2", "#42B540B2")
        ) +
        coord_cartesian(xlim = c(0, xmax), ylim = ylim) +
        scale_x_continuous(breaks = xbreaks) +
        scale_y_continuous(breaks = ybreaks) +
        facet_wrap(~ group, ncol = 2, scales = "free_x") +
        labs(
          x = xlab,
          y = ylab,
          linetype = dlab,
          color = dlab
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
        dt <- self$data %>%
          mutate(
            group = case_when(
              male == 0 & age < age_cut ~ 1,
              male == 0 & age >= age_cut ~ 2,
              male == 1 & age < age_cut ~ 3,
              male == 1 & age >= age_cut ~ 4
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
        min(dt$days_reply, na.rm = TRUE), cut_days[1:2] + 1
      )

      end_days <- c(
        cut_days[1:2], max(dt$days_reply, na.rm = TRUE)
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