coefplot <- function(obj, cond, ...) UseMethod("coefplot")

coefplot.lm_subset_stock <- function(obj) {
  plot_list <- unique(obj$plotdt$outcome) %>%
    purrr::map(function(x) {
      subset(obj$plotdt, outcome == x) %>%
        ggplot(aes(x = pos, y = estimate)) +
        geom_hline(aes(yintercept = 0), linetype = 2) +
        geom_point(
          aes(color = term, shape = term),
          size = 3, position = position_dodge(0.5)
        ) +
        geom_errorbar(
          aes(ymin = conf.low, ymax = conf.high, color = term),
          position = position_dodge(0.5),
          width = 0
        ) +
        geom_text(
          aes(y = -0.15, label = N),
          data = subset(obj$text, outcome == x),
          color = "black"
        ) +
        geom_text(
          aes(y = -0.175, label = mean),
          data = subset(obj$text, outcome == x),
          color = "black"
        ) +
        scale_y_continuous(
          breaks = seq(-0.2, 0.2, by = 0.05), limits = c(-0.2, 0.2)
        ) +
        labs(
          title = paste("Outcome:", x),
          x = "Subset",
          y = "Estimated Effects (95%CI)",
          color = "Treatment", shape = "Treatment"
        ) +
        simplegg()
    })

  wrap_plots(plot_list, ncol = 2) +
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom")
}

coefplot.lm_subset_coordination <- coefplot.lm_subset_stock

coefplot.lm_subset_flow <- function(obj, cond) {
  cond <- rlang::enquo(cond)
  cond_eval <- rlang::eval_tidy(cond, obj$plotdt)

  sub_plotdt <- obj$plotdt[cond_eval, , drop = FALSE]

  plot_list <- unique(sub_plotdt$outcome) %>%
    purrr::map(function(x) {
      subset(sub_plotdt, outcome == x) %>%
        ggplot(aes(x = within, y = estimate, color = term, shape = term)) +
        geom_hline(aes(yintercept = 0), linetype = 2) +
        geom_point(size = 3, position = position_dodge(2)) +
        geom_errorbar(
          aes(ymin = conf.low, ymax = conf.high),
          position = position_dodge(2),
          width = 0
        ) +
        scale_x_continuous(
          breaks = subset(sub_plotdt, outcome == x)$within,
          labels = subset(sub_plotdt, outcome == x)$avg
        ) +
        scale_y_continuous(
          breaks = seq(-0.3, 0.2, by = 0.05),
          labels = sprintf("%1.2f", seq(-0.3, 0.2, by = 0.05)),
          limits = c(-0.25, 0.2)
        ) +
        labs(
          title = paste("Outcome:", x),
          x = "Days after sending notification\n(Control average)",
          y = "Estimated Effects (95%CI)",
          color = "Treatment", shape = "Treatment"
        ) +
        simplegg()
    })

  wrap_plots(plot_list, ncol = 2) +
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom")
}
