coefplot <- function(obj) UseMethod("coefplot")

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