library(here)
library(R6)
library(kableExtra)
source(here("R/misc.r"))

Schedule <- R6::R6Class("Schedule",
  public = list(
    data = NULL,
    initialize = function(path) {
      self$data <- read_csv(path) %>%
        mutate(
          my = paste0(month, "/", year - 2000),
          my = factor(
            my,
            levels = c("9/21", "10/21", "11/21", "12/21", "1/22", "2/22"),
            labels = c("Sep 21", "Oct 21", "Nov 21", "Dec 21", "Jan 22", "Feb 22")
          ),
          week = factor(week, levels = 1:4, labels = paste("Week", 1:4))
        ) %>%
        dplyr::select(week, my, treat) %>%
        pivot_wider(values_from = treat, names_from = my)
    },
    flextable = function(title = "", notes = "", font_size = 9) {
      flextable(self$data) %>%
        set_caption(title) %>%
        align(j = -1, align = "center", part = "all") %>%
        add_footer_lines(notes) %>%
        fontsize(size = font_size, part = "all") %>%
        ft_theme()
    },
    kable = function(title = "", notes = "", font_size = 9, hold = FALSE) {
      kbl <- self$data %>%
        knitr::kable(
          caption = title,
          align = "lcccccc",
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

      kbl %>%
        kableExtra::kable_styling(font_size = font_size) %>%
        kableExtra::footnote(
          general_title = "",
          general = notes,
          threeparttable = TRUE,
          escape = FALSE
        )
    }
  ),
  private = list()
)