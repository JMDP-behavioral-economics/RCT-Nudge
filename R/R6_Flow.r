library(here)
library(R6)
source(here("R/misc.r"))

Flow <- R6::R6Class("Flow",
  public = list(
    data = NULL,
    initialize = function(reply_data) self$data <- reply_data,
    plot = function(...,
                    label_list,
                    xlim = c(0, 40)) {
      
      dt <- self$data %>%
        select(treat, days_reply)
      
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
          y = "Cumulative response rate (%)",
          linetype = "Treatment"
        ) +
        my_theme_classic() +
        theme(
          legend.key.size = grid::unit(1.5, "cm"),
          legend.position = "bottom"
        )
    }
  ),
  private = list()
)