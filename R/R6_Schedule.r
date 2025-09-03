library(here)
library(R6)
library(kableExtra)
library(lubridate)
source(here("R/misc.r"))

Schedule <- R6::R6Class("Schedule",
  public = list(
    data = NULL,
    initialize = function(path, treat_labels = NULL) {
      dt <- read_csv(path) %>%
        mutate(
          date = paste(format(start_date, "%m/%d"), "to", format(end_date, "%m/%d")),
          date = paste0("(", date, ")"),
          year_month = format(make_date(year, month), "%B, %Y")
        ) %>%
        select(year_month, week, treat, date)

        if (!is.null(treat_labels)) {
          dt <- dt %>%
            mutate(treat = dplyr::recode(treat, !!!treat_labels))
        }

      self$data <- dt %>%
        pivot_longer(treat:date) %>%
        pivot_wider(names_from = year_month) %>%
        mutate(week = if_else(name == "date", NA_real_, week)) %>%
        select(-name)
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